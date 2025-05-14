## ---------------------------
##
## Script name: spartan_call.R
##
## Purpose of script: This is the script to be first called by a job submission to HPC (Spartan/Pawsey) 
## Assign sim details (country, mu value), then call script for generating SDM (growth_rate_map_spartan.R)
## ---------------------------
########

rm(list=ls()) # Clear workspace
args <- commandArgs(trailingOnly = TRUE) # Create command line for interacting with job_submission.slurm script
iter_spartan <- as.numeric(args[1]) # iter corresponds to array number (iteration) of job

#### Set parameters for this sim: ####

mu_est <- 0 # mu value
# Assign which country you're working in
country <- "Australia" 
#country <- "South Africa"
#country <- "US"
#country <- "Israel"

map.res <- 0.1 # resolution of map (degrees) -> 0.05 deg = 5 km


model <- "weighted_mean" # Predict tree temp with weighted mean model (greta coeff)
#model <- "amb_temp"
# otherwise use 'mod_fit' lm

### Install/load packages ####

# Check for package dependenices and install/load in packages as required.
# .libPaths("/home/alcoates/R/lib") # Old Spartan Library
# R_LIBS_SITE="/software/projects/pawsey1103/setonix/2024.05/r/4.3:/software/projects/pawsey1103/acoates/setonix/2024.05/r/4.3:/software/setonix/2024.05/software/linux-sles15-zen3/gcc-12.2.0/r-4.3.0-llieqbuwjngu7buqaftswodfq3wx65dc/rlib/R/library"
# Sys.setenv(SILO_API_KEY="andrew.coates@curtin.edu.au")

# lib = .libPaths()[1]
repo<- "https://cran.ms.unimelb.edu.au/" # Set mirror to download packages.
pkgs = c("readr", "dplyr", "gdata", 
         "ggplot2", 
         #"gganimate", "gifski", 
         "mapdata", 
         #"egg", 
         "corrplot", 
         #"Matrix", 
         "magic", "reshape", 
         #"ggstance",
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
         #"foreach","parallel","doParallel",
         "maps", "nasapower") # Define packages.
# Ignore installation for now (should hopefully already be installed?)
# new.pkgs <- pkgs[!(pkgs %in% installed.packages(lib=lib)[,"Package"])]
# if(length(new.pkgs)) install.packages(new.pkgs, lib=lib, repos=repo)
inst = lapply(pkgs, library, character.only = TRUE)

# Issue with ozmaps
#install.packages("ozmaps", lib=lib,
#                 repos = "https://cloud.r-project.org/package=ozmaps")

#install.packages("weatherOz", lib=lib,
#                 repos = "https://ropensci.r-universe.dev")
library(weatherOz)

#### Other parameters ####

# n.cores.spartan <- 20 # Assign number of Spartan cores to use (when running parallel only)
 mu_disp_est <- 0 # DISPERSAL RATE (Need to estimate number of dispersing P)
 phi_mu_est <- 1 # DISPERSAL SURVIVAL
# mu_est <- mu_disp_est * (1 - phi_mu_est) # MU as function of dispersal RATE * MORTALITY
# mu_est_iter <- c(0.35, 0, 0.5) # Run diff mu over different iterations

#### Australia coords ####
# Sometimes weatherOz crashes if running for all of Australia
# Split Australia into parts and run as separate jobs (iter_spartan = 1 - 8)
# 2x separate lat bands:
lat1 <- c(seq(-45, -25.1, by=map.res)) # South
lat2 <- c(seq(-25, -10, by=map.res)) # North
# 4x separate long bands:
lon1 <- c(seq(110, 130, by=map.res)) # WA
lon2 <- c(seq(130.1, 140, by=map.res)) # NT & SA
lon3 <- c(seq(140.1, 145, by=map.res)) # Qld, Vic & NSW
lon4 <- c(seq(145.1, 160, by=map.res)) # east coast

# Create lists of lat and lon -- 8 sections in total
lat_list <- list(lat1, lat1, lat1, lat1,
                 lat2, lat2, lat2, lat2)
lon_list <- list(lon1, lon2, lon3, lon4,
                 lon1, lon2, lon3, lon4)


# Source script
source("src/growth_rate_map_spartan.R")
