## ---------------------------
##
## Script name: basic-within-pop-model.R
##
## Purpose of script: Run and display numerical realizations of the within-population PSHB model
##
##
## Date Created: 2024-01-29
##
## Email:kanishkwalavalkar@gmail.com;  ben.l.phillips@curtin.edu.au
##
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

# Get tree temperature data
locLat <- -32.005892
locLong <- 115.896019


yearSim <- run_year(lat = locLat, long = locLong)

# Plot all figures using the NvTPlot function
NvTPlot(yearSim$temps, yearSim$popDat)
