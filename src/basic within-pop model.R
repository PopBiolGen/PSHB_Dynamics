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
library(maps)
library(ozmaps)
library(sf)
sf_oz <- subset(ozmap("country"))

## Load up the functions
source("src/TPCFunctions.R")
source("src/modelFunctions.R")

model <- "weighted_mean" # if model == 'weighted_mean' use weighted mean model (with greta coeff) to predicts tree temp
# otherwise use 'mod_fit' lm

# Pick a location
locLat <- -31.96165
locLong <- 115.8317

country <- "Australia"
#country <- "South Africa"
#country <- "US"
#country <- "Israel"

# Previous LM approach for tree temp
# Load data for temperature function (from temperature-prediction-function.R -- spartan can't load Rdata file)
# merge_temp <- read.csv('src/temperatures/merge_temp.csv')
# mod_fit <- lm(mean_d ~ air_tmax*rh_tmax + ma30*rh_tmax, data = merge_temp)
# tree_temp_model_pars <- coef(mod_fit)

# Assign mu parameter
mu_disp_est <- 0 # estimated mu parameter (proportion P dispersing)
phi_mu_est <- 1 # estimated phi_mu (proportion survival during dispersal)
# mu_est <- 0
mu_est <- mu_disp_est * (1 - phi_mu_est) # new 'mu' estimate is the proportion of P lost through dispersal mortality (assuming net incoming vs outgoing P = 0)


# Run numerical sims
yearSim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)

# report mean daily growth rate for each life-history stage
yearSim$growthRate
