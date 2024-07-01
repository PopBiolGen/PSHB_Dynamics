## ---------------------------
##
## Script name: basic-within-pop-model.R
##
## Purpose of script: Run and display numerical realizations of the within-population PSHB model
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## Load up the required packages
library(dplyr)
library(lubridate)

## Load up the functions
source("src/TPCFunctions.R")
source("src/modelFunctions.R")

# Pick a location
locLat <- -32.005892
locLong <- 115.896019

# Run numerical sims
yearSim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)

# report mean daily growth rate for each life-history stage
yearSim$growthRate
