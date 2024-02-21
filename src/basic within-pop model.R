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
matplot(t(population_data), type = "l", bty = "l", xlab = "Day of the year", ylab = "N")
legend('topleft', legend = c('Juveniles', 'Pre-adults', 'Adults'), col = 1:3, lty = 1:3)

# plot growth rates of adults over time
ad_vec <- population_data[3,]
growth_rate <- diff(log(ad_vec))
plot(growth_rate, type = "l")


# Plot all figures using the NvTPlot function
NvTPlot(temps, population_data)
