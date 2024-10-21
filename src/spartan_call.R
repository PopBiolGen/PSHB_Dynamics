#SPARTAN

#### Assign parameters to model ####

rm(list=ls()) # Clear workspace

args <- commandArgs(trailingOnly = TRUE) # Create command line for interacting with job_submission.slurm script
iter <- as.numeric(args[1]) # iter corresponds to array number (iteration) of job

# Check for package dependenices and install/load in packages as required.
.libPaths("/home/alcoates/R/lib")
lib = .libPaths()[1]
repo<- "https://cran.ms.unimelb.edu.au/" # Set mirror to download packages.
pkgs = c("readr", "dplyr", "gdata", "ggplot2", "gganimate", "gifski", 
         "mapdata", "egg", "corrplot", "Matrix", "magic", "reshape", "ggstance",
         "pillar",
         "httpcode", # NEW PACKAGES
         "urltools",
         "ozmaps",
         "sf",
         "viridis",
         'data.table',
         'terra',
         'apsimx',
         'zoo',
         "dplyr",
         "lubridate",
         "foreach",
         "parallel",
         "doParallel",
         "maps", "nasapower") # Define packages.
new.pkgs <- pkgs[!(pkgs %in% installed.packages(lib=lib)[,"Package"])]
if(length(new.pkgs)) install.packages(new.pkgs, lib=lib, repos=repo)
inst = lapply(pkgs, library, character.only = TRUE)

install.packages("weatherOz", lib=lib,
                 repos = "https://ropensci.r-universe.dev")
library(weatherOz)

Sys.setenv(SILO_API_KEY="andrew.coates@curtin.edu.au")

# Set parameters:

# n.cores.spartan <- 20 # Assign number of Spartan cores to use (when running parallel only)

# mu values 
# mu_est_iter <- c(0.35, 0, 0.5) # Run diff mu over different iterations
mu_est <- 0 # Let's focus on just 1 mu for now...

map.res <- 0.1 # resolution of map (degrees) -> 0.05 deg = 5 km

# Run different, smaller jobs with different segments of Aus:
# Run 2x separate lat bands:
lat1 <- c(seq(-45, -25.1, by=map.res)) # South
lat2 <- c(seq(-25, -10, by=map.res)) # North
# Run 4x separate long bands:
lon1 <- c(seq(110, 130, by=map.res)) # WA
lon2 <- c(seq(130.1, 140, by=map.res)) # NT SA
lon3 <- c(seq(140.1, 145, by=map.res)) # Qld Vic NSW
lon4 <- c(seq(145.1, 160, by=map.res)) # east coast

# Create lists of lat and lon to match up:
lat_list <- list(lat1, lat1, lat1, lat1,
                 lat2, lat2, lat2, lat2)
lon_list <- list(lon1, lon2, lon3, lon4,
                 lon1, lon2, lon3, lon4)

model <- "weighted_mean" # if model == 'weighted_mean' use weighted mean model (with greta coeff) to predicts tree temp
 # otherwise use 'mod_fit' lm

# Source script
source("src/growth_rate_map_spartan.R")
