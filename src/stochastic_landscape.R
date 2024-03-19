# test of a spatio-temporal inference model using minimal stochastic dispersal
# dynamics in greta.dynamics

library(tidyverse)
library(sf)
library(terra)
library(geodata)

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
stopifnot( abs(observed_pop - expected_pop) < 1e-6)

# define an adjacency matrix for dispersal
adjacency <- adjacent(voronoi)
head(adjacency)




# build greta model
library(greta)