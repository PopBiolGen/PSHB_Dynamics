# test of a spatio-temporal inference model using minimal stochastic dispersal
# dynamics in greta.dynamics

library(tidyverse)
library(sf)
library(terra)
library(geodata)

# given random variables z (with standard normal distribution a priori), and
# Poisson rate parameter lambda, return a strictly positive continuous random
# variable with the same mean and variance as a poisson random variable with
# rate lambda, by approximating the poisson as a lognormal distribution.
lognormal_continuous_poisson <- function(lambda, z) {
  sigma <- sqrt(log1p(lambda / exp(2 * log(lambda))))
  # sigma2 <- log1p(1 / lambda)
  mu <- log(lambda) - sigma^2 / 2
  exp(mu + z * sigma)
}

# given 'edges', a description of a network (a two column matrix with 'from' and
# 'to' columns indexing the nodes connected by each edge in the network,
# *excluding self-dispersal*) and 'fraction_dispersing' either a scalar or a
# vector (with the same length as the *number* of nodes) of the fraction of
# individuals in each node that will disperse to other nodes at each timestep,
# compute the object (a named list with three matching vectors: 'from', 'to',
# and 'weight') needed to efficiently compute dispersal on the network, assuming
# equal probabilities of dispersing along each of the edges from the 'from'
# nodes
uniform_dispersal_network <- function(edges, fraction_dispersing) {
  
  n_nodes <- max(edges)
  
  # expand out the fraction dispersing to all nodes if it is a scalar
  if (length(fraction_dispersing) == n_nodes) {
    fraction_dispersing_vec <- fraction_dispersing
  } else {
    fraction_dispersing_vec <- rep(fraction_dispersing, n_nodes)
  }
  
  # compute the numbers of edges from each node
  n_routes <-  table(edges[, "from"])
  
  # check there are no isolated nodes
  stopifnot(min(n_routes) > 0)
  
  # compute the fraction of dispersing individuals that would use each route,
  # under the uniformity assumption
  dispersal_fraction_normalised <- (1 / n_routes)[edges[, "from"]]
  
  # compute the overall fractions dispersing on each of the true edges
  other_dispersal <- dispersal_fraction_normalised *
    fraction_dispersing_vec[edges[, "from"]]
  
  # and the overall fraction not leaving
  self_dispersal <- 1 - fraction_dispersing_vec
  
  # add edges for non-dispersal (self-dispersal)
  edges_self <- cbind(from = seq_len(n_clusters),
                      to = seq_len(n_clusters))
  
  # combine into a single edges matrix, including the self-dispersal
  edges_all <- rbind(edges, edges_self)
  
  list(from = edges_all[, "from"],
       to = edges_all[, "to"],
       weight = c(other_dispersal, self_dispersal))
  
}

# given a vector 'state' of population state values in each node, and a weighted
# network structure 'network', disperse individuals across the network.
# 'network' should be a named list with three vectors of equal length: 'from'
# and 'to' vectors indexing the nodes connected by each edge in the network
# (including self-dispersal), and a 'weight' vector giving the fraction of the
# individuals in the corresponding 'from' node dispersing via that route
sparse_dispersal <- function(state, network) {
  
  # expected number going via each route
  expected_edges <- state[network$from] * network$weight
  
  # sum over all edges by cluster of arrival to get the expected number arriving
  # at each
  tapply(expected_edges,
         network$to,
         FUN = "sum")
  
}

# path to the data
data_dir <- "../mozambique_example/data/"

# load the local data and tidy up
moz_pr_data <- read.csv(file.path(data_dir, "moz_pr_data.csv"))
moz_shp <- st_read(file.path(data_dir, "moz_shp.shp"))
moz_evi <- rast(file.path(data_dir, "moz_evi.tif"))
moz_ntl <- rast(file.path(data_dir, "moz_ntl.tif"))
moz_tsi <- rast(file.path(data_dir, "moz_tsi.tif"))

# combine environmental variables into a single stack
names(moz_evi) <-"evi"
names(moz_ntl) <-"ntl"
names(moz_tsi) <-"tsi"
moz_env <- c(moz_evi, moz_ntl, moz_tsi)

moz_mask <- moz_pop[[1]] * 0
names(moz_mask) <- "mask"

# get rid of invalid lat-longs
moz_pr_data <- moz_pr_data %>%
  filter(longitude != 0)

# get out the coordinates of observations
moz_coords_obs <- cbind(moz_pr_data$longitude,
                        moz_pr_data$latitude)

# extract covariate values at these observation coordinates
env_obs <- terra::extract(moz_env,
                          moz_coords_obs)

# get population density raster for mozambique
pop <- geodata::population("2020", path = tempdir())
moz_pop <- crop(pop, moz_mask, extend = TRUE)
moz_pop <- disagg(moz_pop, 10)
moz_pop <- resample(moz_pop, moz_mask)
moz_pop <- mask(moz_pop, moz_mask)

# create spatial clusters for modelling

# first generate a bunch of points, proportional to population, then kmeans
# cluster them to block up the country
max_pop <- global(moz_pop, max, na.rm = TRUE)[1, 1]
locs <- terra::spatSample(moz_pop / max_pop,
                          1e4,
                          xy = TRUE,
                          method = "weights",
                          replace = TRUE)
n_clusters <- 500

kmn <- kmeans(locs, centers = n_clusters)
cluster_centres <- terra::vect(kmn$centers[, 1:2])
voronoi <- terra::voronoi(cluster_centres)
values(voronoi) <- data.frame(cluster = seq_along(voronoi))
clusters <- rasterize(voronoi, moz_mask, field = "cluster")
clusters <- mask(clusters, moz_mask)

# # plot them
# plot(clusters, col = sample(rainbow(n_clusters)))

# get the cluster populations
cluster_pops <- zonal(moz_pop, clusters, fun = "sum")

# check none are too small
min(cluster_pops$population_density)

# check totals match
expected_pop <- global(moz_pop, sum, na.rm = TRUE)[1, 1]
observed_pop <- sum(cluster_pops$population_density)
stopifnot(abs(observed_pop - expected_pop) < 1e-6)

# define an adjacency matrix for dispersal
adjacency <- adjacent(voronoi)

# build a dispersal network object based on this
dispersal_network <- uniform_dispersal_network(edges = adjacency, fraction_dispersing = 0.1)

# get pixel and cluster ids
cell_id <- cells(moz_mask)
cluster_id <- terra::extract(clusters, cell_id)$cluster
n_cells <- length(cell_id)

# fake state
state <- runif(n_cells)

# fake growth rates
r <- abs(rnorm(n_cells, 1, 0.25))

# fake innovations (cluster level)
z <- rnorm(n_clusters)

# apply growth rates to get pixel-level expected cells
state_grown_expected <- state * r

# aggregate to get expected total state in each clusters
cluster_state_expected <- tapply(state_grown_expected,
                                 cluster_id,
                                 FUN = "sum")

# do sparse expected dispersal to new states
cluster_new_state_expected <- sparse_dispersal(
  state = cluster_state_expected,
  network = dispersal_network)

# apply stochasticity to the expected new state
cluster_new_state <- lognormal_continuous_poisson(cluster_new_state_expected, z)

# compute ratios in each cluster, to apply the same dispersal effect and
# stochastic demographic change across all pixels
cluster_ratios <- cluster_new_state / cluster_old_state

# compute the new pixel-level states
new_state <- state * cluster_ratios[cluster_id]

# build greta model
library(greta)