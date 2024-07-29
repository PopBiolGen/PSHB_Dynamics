## ---------------------------
##
## Script name: modelFunctions.R
##
## Purpose of script: Functions for running and plotting numerical solutions to PSHB models
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

## load up our functions into memory

alpha_J_temp <- function(temperature){
  TPC_temp(temperature,
           parameters = list(
             Pmax = 0.044,
             T_o = 29.665,
             a_plus = 6.962,
             a_minus = 0.269
           ))
}

alpha_P_temp <- function(temperature){
  TPC_temp(temperature,
           parameters = list(
             Pmax = 0.118,
             T_o = 29.665,
             a_plus = 6.962,
             a_minus = 0.269
           ))
}


phi_J_temp <- function(temperature){
  TPC_temp(temperature,
           parameters = list(
             Pmax = 0.994,
             T_o = 29.499,
             a_plus = 80,
             a_minus = 0.1497
           ))
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

# a version that applies the TPC function for a given list of parameters (will be
# greta arrays)
TPC_temp <- function(temperature, parameters) {
  TPC.pshb(Tb = temperature,
           Pmax = parameters$Pmax,
           T_o = parameters$T_o,
           a_plus = parameters$a_plus,
           a_minus = parameters$a_minus)
}

# Recursion for the within-host model with cumulative offspring affecting survival
step_within_population <- function(n_t,
                                   cumulative_offspring,
                                   temperature,
                                   f = 0.69,
                                   phi_A = 0.97,
                                   phi_P = 0.97,
                                   mu = mu_est,
                                   survival_threshold) {
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
  
  # Number of Pre-adults dispersing away from host
  n_mu_t <- n_t[2]  * phi_P * (1 - alpha_P) * # Number of surviving and non-transitioning P
    mu # mu = Proportion that DO disperse
  
  # Return both the updated population vector and cumulative offspring
  return(list(n = n_tplus, cum_n = cumulative_offspring, n_mu = n_mu_t))
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

# take the list of prior parameters and making them into gret arrays
define_single_prior <- function(prior_definition) {
  
  # handle NULL case
  if (is.null(prior_definition)) {
    return(NULL)
  }
  
  dist <- prior_definition$Distribution
  args <- prior_definition[names(prior_definition) != "Distribution"]
  do.call(dist, args)
}

define_prior_list <- function(prior_definition_list) {
  priors <- lapply(prior_definition_list,
                   define_single_prior)
  priors
}


# A script for building priors for the PSHB model
# Details are provided in modelDescriptions.Rmd "## Estimating priors"
# This script follows those details but then places the priors on nice supports
# Outputs a list specifying full priors for each parameter

prior_calculator <- function() {
  
  source("src/TPCFunctions.R")
  
  # function returning beta parameters given mean and variance
  # from https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
  estBetaParams <- function(mu, var) {
    alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
    beta <- alpha * (1 / mu - 1)
    return(params = list(shape1 = alpha, shape2 = beta))
  }
  
  # function returning gamma parameters given mean and variance
  estGammaParams <- function(mu, var){
    rate <- mu/var
    shape <- mu^2/var
    return(params = list(shape = shape, rate = rate))
  }
  
  # alpha_J(T) 
  
  # Data from two relevant papers, umeda and walgama
  alpha_j_temp_umeda <- c(18, 20, 25, 30, 32)
  alpha_j_rate_umeda <- c(0.013, 0.020, 0.0258, 0.0421, 0.0206)
  alpha_j_temp_walgama <- c(15, 18, 20, 22, 25, 28, 30, 32)
  egg_dev_time_walgama <- c(Inf, 23.5, 18.5, 13, 7.3, 5.5, 5, 10.9)
  larva_dev_time_walgama <- c(NA, 1/0.0288, 1/0.0399, NA, 1/0.0688, 1/0.140, NA, NA) # extracted from figure
  pupae_dev_time_walgama <- c(Inf, 15, 14, 9, 7.5, 6, 4, 7)
  total_juv_dev_time_walgama <- egg_dev_time_walgama + larva_dev_time_walgama + pupae_dev_time_walgama
  alpha_j_rate_walgama <- 1/total_juv_dev_time_walgama
  alpha_j_rate_walgama[1] <- 0
  
  # Estimate TPC
  temp <- c(alpha_j_temp_umeda, alpha_j_temp_walgama)
  rate <- c(alpha_j_rate_umeda, alpha_j_rate_walgama)
  prob <- 1-exp(-rate) # switch from rate to probability
  TPCmatrix <- na.omit(cbind(temp, prob))
  alpha_J_fit <- TPC.q.fit(TPCmatrix, in.acc = 8, hessian = TRUE)
  
  par_names_TPC <- c("Pmax", "T_o", "a_plus", "a_minus")
  
  alpha_J_pars <- alpha_J_fit$par
  alpha_J_pars_sd <- sqrt(diag(solve(alpha_J_fit$hessian)))
  alpha_J_prior_ests <- cbind(alpha_J_pars, alpha_J_pars_sd)
  rownames(alpha_J_prior_ests) <- par_names_TPC
  
  # Supports
  # lists to take priors
  alpha_J_priors <- vector(mode = "list", length = 4)
  names(alpha_J_priors) <- par_names_TPC
  phi_J_priors <- alpha_P_priors <- alpha_J_priors
  
  # Pmax within (0,1)
  alpha_J_priors[["Pmax"]] <- c(
    Distribution = "beta",
    estBetaParams(alpha_J_prior_ests["Pmax", 1], alpha_J_prior_ests["Pmax", 2]^2)
  )
  # T_o within (-Inf,Inf)
  alpha_J_priors[["T_o"]] <- list(
    Distribution = "normal",
    mean = alpha_J_prior_ests["T_o", 1], 
    sd = alpha_J_prior_ests["T_o", 2]
  )
  # a_plus within (0,Inf)
  alpha_J_priors[["a_plus"]] <- c(
    Distribution = "gamma",
    estGammaParams(alpha_J_prior_ests["a_plus", 1], alpha_J_prior_ests["a_plus", 2]^2)
  )
  # a_minus within (0,1)
  alpha_J_priors[["a_minus"]] <- c(
    Distribution = "beta",
    estBetaParams(alpha_J_prior_ests["a_minus", 1], alpha_J_prior_ests["a_minus", 2]^2)
  )
  
  
  # alpha_P(T) 
  
  alpha_P_prior_ests <- alpha_J_prior_ests
  colnames(alpha_P_prior_ests) <- c("pars", "alpha_P_pars_sd")
  alpha_P_prior_ests["Pmax", 1] <- 1-exp(-1/8) # rate of 1/8 to per day probability
  alpha_P_prior_ests["Pmax", 2] <- sqrt(alpha_P_prior_ests["Pmax", 1]/alpha_J_prior_ests["Pmax", 1]*(alpha_J_prior_ests["Pmax", 2]^2))
  
  # Supports
  # Pmax within (0,1)
  alpha_P_priors[["Pmax"]] <- c(
    Distribution = "beta",
    estBetaParams(alpha_P_prior_ests["Pmax", 1], alpha_P_prior_ests["Pmax", 2]^2)
  )
  # the rest use alpha_J parameters
  # T_o within (-Inf,Inf)
  alpha_P_priors[["T_o"]] <- list(
    Distribution = "normal",
    mean = alpha_J_prior_ests["T_o", 1], 
    sd = alpha_J_prior_ests["T_o", 2]
  )
  # a_plus within (0,Inf)
  alpha_P_priors[["a_plus"]] <- c(
    Distribution = "gamma",
    estGammaParams(alpha_J_prior_ests["a_plus", 1], alpha_J_prior_ests["a_plus", 2]^2)
  )
  # a_minus within (0,1)
  alpha_P_priors[["a_minus"]] <- c(
    Distribution = "beta",
    estBetaParams(alpha_J_prior_ests["a_minus", 1], alpha_J_prior_ests["a_minus", 2]^2)
  )
  
  
  # phi_J(T) 
  
  sDat <- read.csv(file = "dat/walgamaFig2DataExtract.csv")
  sDat$Mortality <- sDat$Mortality/100 # convert to probability of mortality over n days
  sDat$Mortality[sDat$Mortality>1] <- 1 # catch the 1.002s
  mortRate <- -log(1-sDat$Mortality)/sDat$Days # convert to a rate
  mortProb <- 1-exp(-mortRate) # convert to a daily probability
  survProb <- 1-mortProb
  sDat <- cbind(sDat, mortProb, survProb)
  
  # Estimate TPC
  TPCmatrix <- cbind(sDat$Temperature, survProb)
  TPCmatrix <- na.omit(TPCmatrix)
  phi_J_fit <- TPC.q.fit(TPCmatrix, in.acc = 0.3, hessian = TRUE)
  phi_J_pars <- phi_J_fit$par
  phi_J_pars_sd <- sqrt(diag(solve(phi_J_fit$hessian)))
  phi_J_prior_ests <- cbind(phi_J_pars, phi_J_pars_sd)
  rownames(phi_J_prior_ests) <- par_names_TPC
  
  # Supports
  # Pmax within (0,1)
  phi_J_priors[["Pmax"]] <- c(
    Distribution = "beta",
    estBetaParams(phi_J_prior_ests["Pmax", 1], phi_J_prior_ests["Pmax", 2]^2)
  )
  # T_o within (-Inf,Inf)
  phi_J_priors[["T_o"]] <- list(
    Distribution = "normal",
    mean = phi_J_prior_ests["T_o", 1], 
    sd = phi_J_prior_ests["T_o", 2]
  )
  # a_plus within (0,Inf)
  phi_J_priors[["a_plus"]] <- c(
    Distribution = "gamma",
    estGammaParams(phi_J_prior_ests["a_plus", 1], phi_J_prior_ests["a_plus", 2]^2)
  )
  # a_minus within (0,1)
  phi_J_priors[["a_minus"]] <- c(
    Distribution = "beta",
    estBetaParams(phi_J_prior_ests["a_minus", 1], phi_J_prior_ests["a_minus", 2]^2)
  )
  
  # phi_P, phi_A 
  phi_P_priors <- vector(mode = "list", length = 1)
  
  mean_phi <- exp(-1/32)
  var_phi <- 0.03^2
  
  phi_P_priors <- list(
    phi_P = c(
      Distribution = "beta",
      estBetaParams(mean_phi, var_phi)
    )
  )
  
  # phi_A = phi_P
  
  # phi_mu 
  phi_mu_priors <- vector(mode = "list", length = 1)
  
  phi_mu_priors <- list(
    phi_mu = list(
      Distribution = "beta",
      shape1 = 1,
      shape2 = 1
    )
  )
  
  # fecundity 
  
  fecundity <- list(
    fecundity = list(
      Distribution = "normal",
      mean = 0.69,
      sd = 1,
      truncation = c(0, Inf)
    )
  )
  
  #Organise and cleanup 
  
  PSHB_priors <- list(alpha_J = alpha_J_priors, 
                      alpha_P = alpha_P_priors, 
                      phi_J = phi_J_priors, 
                      phi_P = phi_P_priors,
                      phi_mu = phi_mu_priors,
                      fecundity = fecundity)
  
  return(PSHB_priors)
}

# sample random normals within some bounds
random_clamped_normal <- function(mean, sd, min = -Inf, max = Inf, dim = c(1, 1)) {
  x <- rnorm(prod(dim), mean, sd)
  x <- pmin(x, max)
  x <- pmax(x, min)
  dim(x) <- dim
  x
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
  
  P_mu <- matrix(0, nrow = 1, ncol = time_steps) # Matrix for number of dispersing P
  
  for (tt in 2:time_steps) {
    step_result <- step_within_population(n_t = population_data[, tt - 1],
                                          cumulative_offspring = cumulative_offspring,
                                          temperature = temps[tt],
                                          survival_threshold = survival_threshold)

    population_data[, tt] <- step_result$n
    cumulative_offspring <- step_result$cum_n
    P_mu[, tt] <- step_result$n_mu
  }
  
  # remove warmup
  population_data <- population_data[, -(1:warmup)]
  temps <- temps[-(1:warmup)]
  P_mu <- P_mu[, -(1:warmup)]
  
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
  
  list(popDat = population_data, temps = temps, growthRate = growthRate, P_mu = P_mu)
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
    step_result <- step_within_population(n_t = out_matrix[1:3, tt - 1], 
                                          cumulative_offspring = out_matrix[4, tt - 1], 
                                          temperature = temps[tt],
                                          survival_threshold = threshold)
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


# create a masking variable, near zero below lower and above upper
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

# par(mfrow = c(1, 1))
# x <- seq(0, 50, length.out = 1000)
# plot(bound_mask(x, 13.5, 31) ~ x, type = "l")
# lines(bound_mask(x, 13.5, 31, soft = TRUE, tol = 0.1) ~ x, col = "red")

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


# if the value is less thant he boundary, use lower_value, otherwise use
# upper_values. Optionally use a 'soft' version, with a small area of
# interpolation between the two
ifelse_bound_mask <- function(value, boundary, lower_value, upper_value, soft = FALSE, tol = 0.01) {
  # create a mask
  if (soft) {
    lower_mask <- plogis((boundary - value) / tol)
    upper_mask <- plogis((value - boundary) / tol)
  } else {
    lower_mask <- value < boundary
    upper_mask <- 1 - lower_mask
  }
  lower_value * lower_mask + upper_value * upper_mask
}

# A simple TPC based on the meeting of two Gaussian functions
# same as TPC.q in TPCFunctions.R, but parameters re-named to match description in .Rmd
TPC.pshb<-function(Tb, Pmax=10, T_o=28, a_plus=9, a_minus=0.5){
  lhs <- Pmax * exp(-(Tb - T_o)^2 / (2 * a_plus^2))
  rhs <- Pmax * exp(-(Tb - T_o)^2 / (2 * (a_plus * a_minus)^2))
  ifelse_bound_mask(Tb, T_o, lhs, rhs)
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
