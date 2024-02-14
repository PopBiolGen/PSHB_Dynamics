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

## Load up the data
soil <- read.csv("dat/soil.csv")
soil.daily <- soil %>%
  group_by(DOY) %>%
  select(-dates, -TIME) %>%
  summarise(across(everything(), mean))
temps <- soil.daily$D10cm

## Run simulation and plot results
population_data <- run_simulation_and_return_data(temps)
plot_simulation_results(temps, population_data)
