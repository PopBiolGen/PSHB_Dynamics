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
         "dplyr",
         "lubridate",
         "foreach",
         "parallel",
         "doParallel") # Define packages.
new.pkgs <- pkgs[!(pkgs %in% installed.packages(lib=lib)[,"Package"])]
if(length(new.pkgs)) install.packages(new.pkgs, lib=lib, repos=repo)
inst = lapply(pkgs, library, character.only = TRUE)


# Assign number of Spartan cores to use
n.core.spartan <- 8

# Assign mu values to run sims with (over different iterations)
mu_est_iter <- c(0.35, 0, 0.5)

# Save files (sprint) according to mu values (?)

# Source script
source("src/growth_rate_map_spartan.R")
