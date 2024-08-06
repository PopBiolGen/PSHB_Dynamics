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
         "doParallel") # Define packages.
new.pkgs <- pkgs[!(pkgs %in% installed.packages(lib=lib)[,"Package"])]
if(length(new.pkgs)) install.packages(new.pkgs, lib=lib, repos=repo)
inst = lapply(pkgs, library, character.only = TRUE)

install.packages("weatherOz", lib=lib,
                 repos = "https://ropensci.r-universe.dev")
library(weatherOz)

# Assign number of Spartan cores to use
n.cores.spartan <- 20

# Assign mu values to run sims with (over different iterations)
mu_est_iter <- c(0.35, 0, 0.5)

map.res <- 0.2 # resolution of map (degrees) - 0.05 deg = 5 km

# Save files (sprint) according to mu values (?)

# Source script
source("src/growth_rate_map_spartan.R")
