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

## Plotting
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

plot_temperatures(temps)
plot_population_dynamics(temps)
plot_growth_rates(population_data)





