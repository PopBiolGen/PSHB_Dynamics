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
    fraction_dispersing_vec <- fraction_dispersing * rep(1, n_nodes)
  }
  
  # compute the numbers of edges from each node
  n_routes <- as.vector(table(edges[, "from"]))
  
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
  edges_self <- cbind(from = seq_len(n_nodes),
                      to = seq_len(n_nodes))
  
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
# moz_pr_data <- read.csv(file.path(data_dir, "moz_pr_data.csv"))
moz_shp <- st_read(file.path(data_dir, "moz_shp.shp"))
moz_evi <- rast(file.path(data_dir, "moz_evi.tif"))
moz_ntl <- rast(file.path(data_dir, "moz_ntl.tif"))
moz_tsi <- rast(file.path(data_dir, "moz_tsi.tif"))

# combine environmental variables into a single stack
names(moz_evi) <-"evi"
names(moz_tsi) <-"tsi"
moz_env <- c(moz_evi, moz_tsi)
moz_env <- scale(moz_env)
# 
# # get rid of invalid lat-longs
# moz_pr_data <- moz_pr_data %>%
#   filter(longitude != 0)
# 
# # get out the coordinates of observations
# moz_coords_obs <- cbind(moz_pr_data$longitude,
#                         moz_pr_data$latitude)

# get population density raster for Mozambique
pop <- geodata::population("2020", path = tempdir())
moz_pop <- crop(pop, moz_env, extend = TRUE)
moz_pop <- disagg(moz_pop, 10)
moz_pop <- resample(moz_pop, moz_env)
moz_pop <- mask(moz_pop, moz_env[[1]])

# create a mask to drop some dead pixels from moz_env (where there was nothing
# in pop)
moz_mask <- moz_pop * 0
names(moz_mask) <- "mask"
moz_env <- mask(moz_env, moz_mask)

# # extract covariate values at these observation coordinates
# env_obs <- terra::extract(moz_env,
#                           moz_coords_obs)

# create spatial clusters for modelling

# first generate a bunch of points, proportional to population, then kmeans
# cluster them to block up the country
max_pop <- global(moz_pop, max, na.rm = TRUE)[1, 1]
locs <- terra::spatSample(moz_pop / max_pop,
                          1e4,
                          xy = TRUE,
                          method = "weights",
                          replace = TRUE)
n_clusters <- 100

kmn <- kmeans(locs, centers = n_clusters)
cluster_centres <- terra::vect(kmn$centers[, 1:2])
voronoi <- terra::voronoi(cluster_centres)
values(voronoi) <- data.frame(cluster = seq_along(voronoi))
clusters <- rasterize(voronoi, moz_mask, field = "cluster")
clusters <- mask(clusters, moz_mask)

# # plot them
# plot(clusters, col = sample(rainbow(n_clusters)))

# get the cluster populations
cluster_pop <- zonal(moz_pop,
                     clusters,
                     fun = "sum",
                     na.rm = TRUE)$population_density

# check none are too small
stopifnot(min(cluster_pop) > 0)

# check totals match
expected_pop <- global(moz_pop, sum, na.rm = TRUE)[1, 1]
stopifnot(abs(sum(cluster_pop) - expected_pop) < 1e-6)

# define an adjacency matrix for dispersal
adjacency <- adjacent(voronoi)

# get pixel and cluster ids
cell_id <- cells(moz_mask)
cluster_id <- terra::extract(clusters, cell_id)$cluster
n_cells <- length(cell_id)

# get cell populations, and pad 0s
pop <- terra::extract(moz_pop, cell_id)$population_density
pop <- pmax(pop, min(pop[pop > 0]))
# get cluster populations and cell population weights
# population weights for each pixels within each cluster (within each
# cluster these are proportional to population and sum to 1)
pop_weights <- pop / cluster_pop[cluster_id]

# build greta model
library(greta.dynamics)

# define variables:

n_times <- 20

# initial population size (around 10% of population)
init_cluster <- exponential(1 / (cluster_pop * 0.1))
init <- init_cluster[cluster_id] * pop_weights

# growth rate - function of environment
env <- terra::extract(moz_env, cell_id)
X <- as.matrix(cbind(1, env))
beta <- normal(0, 0.1, dim = ncol(X))
log_r <- X %*% beta
r <- exp(log_r)
# hist(calculate(r, nsim = 1)[[1]])

# latent variable for stochastic transitions
z_cluster_vec <- normal(0, 1, dim = c(n_times, n_clusters))

# probability of dispersing (per timestep) and within dispersers, the
# probability that dispersal happens within the same cluster
# prob_disperse <- normal(0.1, 0.1, truncation = c(0, 1))
prob_disperse <- 0.1

# note, we need a neat way to pass the greta array variable inside
# dispersal_network into iterate_dynamic_function if we want to make this a
# variable. Breaks the nice packaging of dispersal networks

# build a between-cluster dispersal network object based adjacency
dispersal_network <- uniform_dispersal_network(
  edges = adjacency,
  fraction_dispersing = prob_disperse)

# transition function for the population process as a difference equation in
# integer timestep
fun <- function(state, iter, r, z_cluster) {
  
  # computed expected population growth in each pixel
  state_grown <- state * r
  
  # aggregate at cluster level
  cluster_grown <- tapply(state_grown,
                          cluster_id,
                          FUN = "sum")

  # now apply dispersal
  cluster_dispersed <- sparse_dispersal(cluster_grown,
                                        dispersal_network)
  
  # now apply stochasticity
  cluster_new <- lognormal_continuous_poisson(cluster_dispersed, z_cluster)
  
  # now compute the ratio in the cluster (from the dispersal stage) and apply to
  # all pixels
  cluster_ratio <- cluster_new / cluster_grown
  
  state_new <- state_grown * cluster_ratio[cluster_id]
  
  state_new
}

# solve it (integer times)
pop_sim <- iterate_dynamic_function(
  transition_function = fun,
  initial_state = init,
  niter = n_times,
  tol = 0,
  r = r,
  z_cluster = z_cluster_vec,
  parameter_is_time_varying = "z_cluster",
  # clamp the populations to reasonably values to avoid numerical under/overflow
  state_limits = c(1e-3, max(pop) / 2)
)

sims <- calculate(pop_sim$all_states,
                  r,
                  values = list(beta = c(-0.1, 0.05, 0.05)),
                  nsim = 1)

# put these back in the raster to plot
r_plot <- moz_mask
names(r_plot) <- "r"
r_plot[cell_id] <- sims$r[1, , 1]
plot(r_plot)

moz_plot <- moz_mask
names(moz_plot) <- "state"
moz_plot_many <- replicate(n_times, moz_plot, simplify = FALSE)
moz_plot_many <- do.call(c, moz_plot_many)
names(moz_plot_many) <- paste0("state", seq_len(n_times))
for (i in seq_len(n_times)) {
  moz_plot_many[[i]][cell_id] <- sims$`pop_sim$all_states`[1, , i]
}
plot(moz_plot_many[[16+1:4]] > 5)

i <- 2
ratio <- moz_plot_many[[i]] / moz_plot_many[[i - 1]]
plot(ratio / r_plot)

par(mfrow = c(1, 1))
i <- 20
plot(moz_plot_many[[i]])
plot(log1p(moz_plot_many[[i]]))


# plot stochastic trajectories for cells with R near 1
i <- sample(which(abs(sims$r - 1) < 0.1), 1)
sims$r[1, i, ]
plot(sims$`pop_sim$all_states`[1, i, ], type = "l")


# the dynamics are evolving away from bad initial values to the regions with
# high r

# probably need to implement some sort of density dependence in there to
# stabilise the populations more

# do logistic growth-type dynamics? With K as a function of r?
# or do seomthing with depletion of susceptibles?

# try fitting to data and see how well it does


