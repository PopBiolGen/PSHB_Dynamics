## ---------------------------
##
## Script name: basic-within-pop-model.R
##
## Purpose of script: Run and display numerical realisations of the within-population PSHB model
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
## load up the packages we will need 
library(dplyr)
library(lubridate)
## ---------------------------

## load up our functions into memory 
source("src/TPCFunctions.R")
source("src/modelFunctions.R")
## ---------------------------

## Load up data we need

# Load the soil.csv dataset
soil <- read.csv("dat/soil.csv")
# Summarise to daily mean temperatures
soil.daily <- soil %>%
          group_by(DOY) %>%
          select(-dates, -TIME) %>%
          summarise(across(everything(), mean))

# Extract the temperature data from the D10 column
temps <- soil.daily$D10cm
## ---------------------------

# Print diagnostic information
cat("Temperature data summary:\n")
print(summary(temps))
plot(temps, type = "l")

# Initial population 
n_initial <- c(0, 0, 1)  # Sample initial population size
cumulative_offspring <- 0
survival_threshold <- 100000  # Adjust as needed

# Run the simulation
time_steps <- length(temps)
population_data <- matrix(0, nrow = 3, ncol = time_steps)
population_data[, 1] <- n_initial

for (tt in 2:time_steps) {
  print(tt)
  step_result <- step_within_population(population_data[, tt - 1], cumulative_offspring, temps[tt], f, phi_A, phi_P, mu = 0, survival_threshold)
  population_data[, tt] <- step_result$n
  cumulative_offspring <- step_result$cum_n
}

# Plot population dynamics over time
matplot(t(population_data), type = "l", bty = "l")
legend('topright', legend = c('Juveniles', 'Pre-adults', 'Adults'), col = 1:3, lty = 1:3)

