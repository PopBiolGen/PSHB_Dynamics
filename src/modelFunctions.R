## ---------------------------
##
## Script name: modelFunctions.R
##
## Purpose of script: Functions for running and plotting numerical solutions to PSHB models
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
## ---------------------------


## Scalar parameters

# Parameters
phi_A <- 0.97
phi_P <- 0.97
mu <- 0
f <- 0.69

## load up our functions into memory

alpha_J_temp <- function(temperature, lower = 13.5, upper = 31){
  rate <- -0.0273 + 0.0023*temperature
  prob <- 1-exp(-rate)
  ifelse(temperature>lower & temperature < upper, prob, 0)
}

alpha_P_temp <- function(temperature, lower = 15, upper = 31){
  DD <- 136 #degree days from walgama
  beta <- 1/DD # implied slope of rate on temperature
  alpha <- -15*beta # implied intercept
  rate <- alpha + beta*temperature
  prob <- 1-exp(-rate)
  ifelse(temperature>lower & temperature < upper, prob, 0)
}

phi_J_temp <- function(temperature){
  TPC.q(temperature, rmax = 0.99, Trmax = 29.5, acc = 80, dec.prop = 0.15)
}

# Recursion for the within-host model with cumulative offspring affecting survival
step_within_population <- function(n_t, cumulative_offspring, temperature, f, phi_A, phi_P, mu = 0, survival_threshold) {
  # Calculate the survival probability based on cumulative offspring
  survival_prob <- ifelse(cumulative_offspring < survival_threshold, 1, 0)
  
  # calculate temperature dependent vital rates for this time interval
  alpha_J <- alpha_J_temp(temperature)
  alpha_P <- alpha_P_temp(temperature)
  phi_J <- phi_J_temp(temperature)
  
  # check for inadmissable probabilities
  if (alpha_J < 0 | alpha_J > 1 | alpha_P < 0 | alpha_P > 1 | phi_J < 0 | phi_J > 1) {
    stop("Temperature dependent parameters are incorrect. Check your temperature data or functions.")
  }
  
  # Transition matrix for within population with density-dependent survival
  W <- matrix(c(survival_prob * phi_J * (1 - alpha_J), 0, f,
                survival_prob * phi_J * alpha_J, phi_P * (1 - alpha_P) * (1 - mu), 0,
                0, phi_P * alpha_P * (1 - mu), phi_A), nrow = 3, byrow = TRUE)
  
  # Calculate the number of offspring produced in this time step
  offspring_produced <- f * n_t[3]  # Adults contribute to offspring based on fA
  
  # Update cumulative offspring
  cumulative_offspring <- cumulative_offspring + offspring_produced
  
  # 1 time step for within-population
  n_tplus <- W %*% n_t
  
  # Return both the updated population vector and cumulative offspring
  return(list(n = n_tplus, cum_n = cumulative_offspring))
}

## Function to plot daily mean temperatures
plot_temperatures <- function(temps, xlabel = FALSE) {
  xl <- ""
  if (xlabel) xl <- "Day of year"
  plot(temps, type = "l", main = "Daily mean temperatures", xlab = xl, ylab = "Temperature (°C)", col = "blue", lwd = 2, bty = "l")
}

## Function to plot population dynamics over time
plot_population_dynamics <- function(temps, population_data, legend_size = 0.7, log.N = FALSE, xlabel = FALSE) {
  xl <- ""
  yl <- "Population size"
  if (xlabel) xl <- "Day of year"
  # Plot population dynamics over time
  if (log.N) {
    population_data <- log(population_data) 
    yl <- "log(Population size)"
  }
  matplot(t(population_data), type = "l", bty = "l", main = "Population dynamics over time", xlab = xl, ylab = yl, col = c("red", "green", "darkorange"), lwd = 2)
  legend('topleft', legend = c('Juveniles', 'Pre-adults', 'Adults'), col = c("red", "green", "darkorange"), lty = 1:3, lwd = 2, cex = legend_size)
}

## Function to plot growth rates of all life stages over time with smoothing
plot_growth_rates <- function(population_data, window_size = 5, legend_size = 0.7) {
  # Extract population data for all life stages
  juv_vec <- population_data[1, ]
  pre_adult_vec <- population_data[2, ]
  ad_vec <- population_data[3, ]
  
  # Calculate growth rates for all life stages
  juv_growth_rate <- diff(log(juv_vec))
  pre_adult_growth_rate <- diff(log(pre_adult_vec))
  ad_growth_rate <- diff(log(ad_vec))
  
  # Smooth the growth rate curves using a moving average
  juv_growth_rate_smoothed <- stats::filter(juv_growth_rate, rep(1/window_size, window_size), sides = 2)
  pre_adult_growth_rate_smoothed <- stats::filter(pre_adult_growth_rate, rep(1/window_size, window_size), sides = 2)
  ad_growth_rate_smoothed <- stats::filter(ad_growth_rate, rep(1/window_size, window_size), sides = 2)
  
  # Plot smoothed growth rates for all life stages
  plot(juv_growth_rate_smoothed, type = "l", main = "Growth rates over time", 
       xlab = "Day of year", ylab = "Growth rate", col = "red", lwd = 2, bty = "l")
  lines(pre_adult_growth_rate_smoothed, col = "green", lwd = 2)
  lines(ad_growth_rate_smoothed, col = "darkorange", lwd = 2)
  legend("topright", legend = c("Juveniles", "Pre-adults", "Adults"), col = c("red", "green", "darkorange"), lty = 1, lwd = 2, cex = legend_size)
}

## Function to plot population dynamics over time
NvTPlot <- function(temps, population_data) {
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
  
  plot_temperatures(temps)
  plot_population_dynamics(temps, population_data)
  plot_growth_rates(population_data)
  plot_population_dynamics(temps, population_data, log.N = TRUE, xlabel = TRUE)
}
