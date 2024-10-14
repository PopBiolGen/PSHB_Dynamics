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

# Load data for temperature function (from temperature-prediction-function.R -- spartan can't load Rdata file)
merge_temp <- read.csv('src/temperatures/merge_temp.csv')
mod_fit <- lm(mean_d ~ air_tmax*rh_tmax + ma30*rh_tmax, data = merge_temp)
tree_temp_model_pars <- coef(mod_fit)

# Assign mu parameter
mu_est <- 0

# Run numerical sims
yearSim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)

# report mean daily growth rate for each life-history stage
yearSim$growthRate
