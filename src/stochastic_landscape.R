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

# plot them
plot(clusters, col = sample(rainbow(n_clusters)))

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
head(adjacency)

# set up dispersal information
n_routes <-  table(adjacency[, "from"])
dispersal_info <- list(
  # total fraction dispersing from each cluster on each timestep
  fraction_dispersing = 0.1,
  # the routes between clusters
  routes = adjacency,
  # the fraction *of those leaving a cluster* that go via each route
  dispersal_fraction_normalised = (1 / n_routes)[adjacency[, "from"]]
)

# explicit, but slightly inefficient implementation of sparse dispersal
dispersal_sparse_slow <- function(cluster_state, dispersal_info) {
  
  # expected number leaving each cluster
  leaving_cluster_expected <- cluster_state *
    dispersal_info$fraction_dispersing
  
  # expected number going via each route
  leaving_route_expected <- leaving_cluster_expected[dispersal_info$routes[, "from"]] *
    dispersal_info$dispersal_fraction_normalised
  
  # sum over all routes by cluster of arrival to get the expected numebr arriving
  # at each
  
  # number travelling via each adjacency
  arriving_cluster_expected <- tapply(leaving_route_expected,
                                      dispersal_info$routes[, "to"],
                                      FUN = "sum")
  
  # sum up dispersals (note this could be made faster by having self-dispersal in
  # the lookup)
  cluster_state_expected -
    leaving_cluster_expected +
    arriving_cluster_expected
  
}

# explicit, but slightly inefficient implementation of sparse dispersal
dispersal_sparse <- function(cluster_state, dispersal_info) {
  
  # combine dispersal info to get a single lookup of dispersal and non-dispersal
  dispersal_info_new <- dispersal_info

  n_clusters <- length(cluster_state)
  routes_self <- cbind(from = seq_len(n_clusters),
                       to = seq_len(n_clusters))
  
  other_dispersal_vec <- dispersal_info$dispersal_fraction_normalised *
    dispersal_info$fraction_dispersing
  
  self_dispersal <- 1 - dispersal_info$fraction_dispersing
  if (length(self_dispersal) == n_clusters) {
    self_dispersal_vec <- self_dispersal
  } else {
    self_dispersal_vec <- rep(self_dispersal, n_clusters)
  }
  
  # combine into a single sparse matrix
  routes_all <- rbind(dispersal_info$routes, routes_self)
  dispersal_vec <- c(other_dispersal_vec, self_dispersal_vec)
  
  # expected number going via each route
  route_state_expected <- cluster_state[routes_all[, "from"]] * dispersal_vec
  
  # sum over all routes by cluster of arrival to get the expected number arriving
  # at each
  tapply(route_state_expected,
         routes_all[, "to"],
         FUN = "sum")
  
}

# remind myself of sparse dispersal thingo

# proportion dispersing from each cluster at each timestep

# fraction of those dispersing from each cluster that use each route

# get all pixels

# pixel and cluster ids
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
cluster_new_state_expected <- dispersal_sparse(
  cluster_state = cluster_state_expected,
  dispersal_info = dispersal_info)

cluster_new_state_expected2 <- dispersal_sparse_slow(
  cluster_state = cluster_state_expected,
  dispersal_info = dispersal_info)

# check these are the same
max(abs(cluster_new_state_expected - cluster_new_state_expected2)) < 1e-6

# apply stochasticity to the expected new state
cluster_new_state <- lognormal_continuous_poisson(cluster_new_state_expected, z)

# compute ratios in each cluster, to apply the same dispersal effect and
# stochastic demographic change across all pixels
cluster_ratios <- cluster_new_state / cluster_old_state

# compute the new pixel-level states
new_state <- state * cluster_ratios[cluster_id]


# build greta model
library(greta)