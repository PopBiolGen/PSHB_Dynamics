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
source("src/TPCFunctions.R")
source("src/greta_valid_inits.R")

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

# Gets environmental data for tree temperature prediction, given a lat and long
# requires an API key (your email address) for SILO stored in .Renviron
get_env_data <- function(lat, long){
  # get mean temperature and humidity between 2013-2023
  wd <- weatherOz::get_data_drill(
    latitude = lat,
    longitude = long,
    start_date = "20130101",
    end_date = "20231231",
    values = c(
      "max_temp",
      "min_temp",
      "rh_tmax"
    ),
    api_key = Sys.getenv("SILO_API_KEY")
  )
 #calculate 1 month moving average temp in lieu of soil temp at 100cm
 wd <- wd %>% dplyr::mutate(DOY = yday(dmy(paste(day, month, year, sep = "-")))) %>%
    mutate(meanDaily = (air_tmax+air_tmin)/2, soil = zoo::rollmean(meanDaily, k = 30, fill = NA, align = "right")) %>%
    select(DOY, air_tmax, rh_tmax, soil) %>%
    group_by(DOY) %>%
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  
 return(wd)
}

# function defining phi_J as a function of temperature and four parameters
phi_J_temp <- function(temperature, Pmax = 0.99, T_o = 29.5, a_plus = 80, a_minus = 0.15){
  TPC.pshb(temperature, Pmax, T_o, a_plus, a_minus)
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


# returns predicted mean tree temperature each day based on inputs of:
# soil temperature at 1m below (30-day moving average of mean daily temp)
# mean maximum air temperature for that day
# mean relative humidity of that day
# uses model parameters generated in src/temperatures/temperature-prediction-function.R
# gets environmental data from Australia SILO database
tree_temp_prediction <- function(lat = -32.005892, long = 115.896019){
  load("out/tree-temp-model-pars.Rdata")
  locDat <- get_env_data(lat, long)
  newDat <- list(air_tmax = locDat$air_tmax,
       rh_tmax = locDat$rh_tmax,
       ma30 = locDat$soil)
  predict(mod_fit, newdata = newDat)
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
  juv_vec <- population_data[1,]
  pre_adult_vec <- population_data[2,]
  ad_vec <- population_data[3,]
  
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

# Runs a year of population growth at a given location
run_year <- function(lat, long, warmup = 10, survival_threshold = 1e11, make_plot = FALSE){
  # get tree temp
  temps <- tree_temp_prediction(lat = locLat, long = locLong)
  temps <- c(rep(mean(temps), warmup), temps) # add mean temperature for warmup iterations
  
  # Initial population 
  n_initial <- c(0, 0, 1)  # Initial population size
  cumulative_offspring <- 0
  
  # run across warmup iterations to approach stable age distribution
  time_steps <- length(temps)
  population_data <- matrix(0, nrow = 3, ncol = time_steps)
  population_data[, 1] <- n_initial
  
  for (tt in 2:time_steps) {
    step_result <- step_within_population(population_data[, tt - 1], cumulative_offspring, temps[tt], f, phi_A, phi_P, mu = 0, survival_threshold)
    population_data[, tt] <- step_result$n
    cumulative_offspring <- step_result$cum_n
  }
  
  # remove warmup
  population_data <- population_data[, -(1:warmup)]
  temps <- temps[-(1:warmup)]
  
  # calculate mean annual growth rate
  agr <- function(population_data){
    dt <- ncol(population_data)
    diffVec <- log(population_data[,dt]) - log(population_data[,1])
    diffVec/dt
  } 
  growthRate <- agr(population_data)
  
  if (make_plot){
    # Plot all figures using the NvTPlot function
    NvTPlot(temps, population_data)
  }
  
  list(popDat = population_data, temps = temps, growthRate = growthRate)
}

## Function to iterate the within host model over n days
  # Outputs data that can be used for testing inference model
  # Arguments are:
    # initial_n: initial population vector
    # temps: vector of temperature data to iterate over
    # iter: number of days to iterate over
    # threshold: host threshold for cumulative population size
    # note global variables used for phi_A phi_P mu f 
sim_within_host <- function(initial_n, temps, iter, threshold = 1e5, stochastic = FALSE){
  #browser()
  if (length(temps) == 1) temps <- rep(temps, iter)
  if (length(temps) < iter) {
    stop("Temperature data length is less than number of iterations")
  }
  # set up matrix to take results
  out_matrix <- matrix(c(c(initial_n, 0), rep(0, (length(initial_n)+1)*(iter-1))), nrow = length(initial_n)+1, ncol = iter)
  
  # iterate over days
  for (tt in 2:iter){
    step_result <- step_within_population(out_matrix[1:3, tt - 1], 
                                          out_matrix[4, tt - 1], 
                                          temps[tt], 
                                          f, 
                                          phi_A, 
                                          phi_P, 
                                          mu = 0, 
                                          threshold)
    if (stochastic) step_result$n <- rpois(length(step_result$n), step_result$n)
    out_matrix[1:3, tt] <- step_result$n
    out_matrix[4, tt] <- step_result$cum_n
  }
  out_matrix
}

# simulate some timeseries data on preadult counts, with random temperature
# timeseries, random initial condition, and poisson observation noise process
sim_single_preadult_temp_data <- function(n_times = 28,
                                          expected_initial_pop = c(0.01, 0.01, 20)) {
  
  # load real temperatures
  temps <- tree_temp_prediction()
  
  # simulate a random temperature timeseries by randomly sampling a start times
  start_time <- sample.int(length(temps) - n_times, 1)
  times <- start_time + seq_len(n_times) - 1
  temperature <- temps[times]
  
  # simulate a random initial population
  initial_n <- rexp(n_states, 1 / expected_initial_pop)
  
  # simulate some population dynamics; timeseries of preadults at multiple sites
  true_abundance <- sim_within_host(
    initial_n = initial_n,
    temps = temperature,
    iter = n_times,
    threshold = 1e5)
  
  true_preadult_abundance <- true_abundance[2, ]
  preadult_count <- rpois(n_times, true_preadult_abundance)
  
  data.frame(time = seq_len(n_times),
             temperature,
             true_preadult_abundance,
             preadult_count)
}

# simulate multiple timeseries of pre-adult abundances with different
# temperature profiles
sim_preadult_temp_data <- function(n_sites = 5,
                                   n_times = 28,
                                   expected_initial_pop = c(0.01, 0.01, 20)) {
  
  # sample multiple site timeseries
  data_sets <- replicate(n_sites, 
                         sim_single_preadult_temp_data(n_times,
                                                       expected_initial_pop = expected_initial_pop),
                         simplify = FALSE)
  # add an id number
  data_sets <- mapply(FUN = bind_cols,
                      id = seq_along(data_sets),
                      x = data_sets,
                      SIMPLIFY = FALSE)
  
  # combine them
  do.call(bind_rows, data_sets)
  
}


# A simple TPC based on the meeting of two Gaussian functions
# same as TPC.q in TPCFunctions.R, but parameters re-named to match description in .Rmd
TPC.pshb<-function(Tb, Pmax=10, T_o=28, a_plus=9, a_minus=0.5){
  lhs<-Pmax*exp(-(Tb-T_o)^2/(2*a_plus^2))
  rhs<-Pmax*exp(-(Tb-T_o)^2/(2*(a_plus*a_minus)^2))
  ifelse(Tb<T_o, lhs, rhs )
}

# create a masking variable, near zero below lower and above upper, and at 0set x to (near) zero below lower and above upper
bound_mask <- function(x, lower = -Inf, upper = Inf, tol = 0.01, soft = FALSE) {
  # create a mask
  if (soft) {
    lower_mask <- plogis((x - lower) / tol)
    upper_mask <- plogis((upper - x) / tol)
  } else {
    lower_mask <- as.numeric(x > lower)
    upper_mask <- as.numeric(x < upper)
  }
  lower_mask * upper_mask
}

# implemented bounded linear model for transition rates
bounded_linear <- function(temperature, intercept, slope, lower, upper, ...) {
  
  # create a mask to set values to 0 outside the allowed range
  mask <- bound_mask(x = temperature,
                     lower = lower,
                     upper = upper,
                     ...)
  rate <- intercept + slope * temperature
  prob <- 1 - exp(-rate)
  prob * mask
}

# given random variables z (with standard normal distribution a priori), and
# Poisson rate parameter lambda, return a strictly positive continuous random
# variable with the same mean and variance as a poisson random variable with
# rate lambda, by approximating the poisson as a lognormal distribution. This
# has the advantage of decentring the posterior distribution of these random
# variables in MCMC, as well as enabling inference on them with HMC.
# Note annoying two = 2 thing is a hack to workaround the current bug in
# greta.dynamics when defining constants inside a transition function.
lognormal_continuous_poisson <- function(lambda, z) {
  sigma <- sqrt(log1p(lambda / exp(2 * log(lambda))))
  # sigma2 <- log1p(1 / lambda)
  mu <- log(lambda) - sigma^2 / 2
  exp(mu + z * sigma)
}


# Continuous approximation of the Poisson distribution. Model the continuous
# Poisson as a lognormal given lambda, and a standard normal variate z. Do this
# by calculating the mu and sigma to be combined with z such that when
# exponentiated, it has the same mean and variance as the intended poisson RV.

# Working: The lognormal mean and variance should both equal lambda. The
# lognormal mean and variance can both be expressed in terms of the parameters
# mu and sigma.

# mean = lambda = exp(mu + sigma^2 / 2)
# variance = lambda = (exp(sigma ^ 2) - 1) * exp(2 * mu + sigma ^ 2)

# solve to get sigma and mu as a function of lambda:
# mu = log(lambda) - sigma^2 / 2
# lambda = (exp(sigma ^ 2) - 1) * exp(2 * mu + sigma ^ 2)
# lambda = (exp(sigma ^ 2) - 1) * exp(2 * (log(lambda) - sigma^2 / 2) + sigma ^ 2)
# lambda = (exp(sigma ^ 2) - 1) * exp(sigma ^ 2) * exp(2 * (log(lambda) - sigma^2 / 2))
# lambda = (exp(sigma ^ 2) - 1) * exp(sigma ^ 2) * exp(2 * log(lambda) - sigma^2)
# lambda = (exp(sigma ^ 2) - 1) * exp(sigma ^ 2) * exp(2 * log(lambda)) / exp(sigma^2)
# lambda / exp(2 * log(lambda)) = (exp(sigma ^ 2) - 1) * exp(sigma ^ 2) * 1 / exp(sigma^2)
# lambda / exp(2 * log(lambda)) = (exp(sigma ^ 2) - 1)
# log(lambda / exp(2 * log(lambda)) + 1) = sigma ^ 2
# sigma = sqrt(log(lambda / exp(2 * log(lambda)) + 1)) = sigma
# mu = log(lambda) - sigma^2 / 2

# # check these numerically
# library(tidyverse)
# compare <- tibble(
#   lambda = seq(0.01, 1000, length.out = 100)
# ) %>%
#   mutate(
#     sigma = sqrt(log(lambda / exp(2 * log(lambda)) + 1)),
#     mu = log(lambda) - sigma^2 / 2
#   ) %>%
#   mutate(
#     mean = exp(mu + sigma^2 / 2),
#     variance = (exp(sigma ^ 2) - 1) * exp(2 * mu + sigma ^ 2)
#   ) %>%
#   mutate(
#     diff_mean_variance = abs(mean - variance),
#     diff_mean_lambda = abs(mean - lambda),
#     diff_variance_lambda = abs(variance - lambda)
#   ) %>%
#   summarise(
#     across(
#       starts_with("diff"),
#       ~max(.x)
#     )
#   )

# given poisson rate parameter lambda and random uniform deviate u, a continuous
# relaxation of poisson random variable generation is computed using the inverse
# of the incomplete gamma function. ie. igammainv(lambda, 1 - u) is
# approximately equal to qpois(u, lambda) (and exactly equal to qgamma(1 - u,
# lambda) in the R implementation)
gamma_continuous_poisson <- function(lambda, u) {
  # if we want to interpret u as random quantiles of the distribution, it should
  # be this:
  #   igammainv(lambda, 1 - u)
  # but we omit the one minus because it's interacting with a bug in
  # greta.dynamics, and because it doesn't affect inference
  igammainv(lambda, u)
}
# # check:
# lambda <- as_data(pi)
# u <- uniform(0, 1)
# y <- gamma_continuous_poisson(lambda, 1 - u)
# sims <- calculate(y, u, nsim = 1e5)
# max(abs(sims$y - qgamma(1 - sims$u, pi)))
# quantile(round(sims$y))
# quantile(rpois(1e5, pi))

# the inverse incomplete gamma function (the major part of the quantile function
# of a gamma distribution)
igammainv <- function(a, p) {
  op <- greta::.internals$nodes$constructors$op
  op("igammainv", a, p,
     tf_operation = "tf_igammainv"
  )
}
tf_igammainv <- function(a, p) {
  tfp <- greta:::tfp
  tfp$math$igammainv(a, p)
}
