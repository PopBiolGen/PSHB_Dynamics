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

## load up our functions into memory


# Modified recursion for the within-population model with cumulative offspring affecting survival
step_within_population <- function(n_t, phi_J, alpha_J, phi_P, alpha_P, mu, f, phi_A, cumulative_offspring, survival_threshold) {
  # Calculate the survival probability based on cumulative offspring
  survival_prob <- ifelse(cumulative_offspring < survival_threshold, 1, 0)
  
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
  return(list(n_tplus, cumulative_offspring))
}

# Parameters
phi_A <- 0.97
phi_P <- 0.97
mu <- 0
f <- 0.69


alpha_J_temp <- function(temperature, lower = 15, upper = 31){
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

alpha_J <- alpha_J_temp(temps)
plot(alpha_J~temps, 
     type = "l", 
     xlab = "Temperature", 
     ylab = quote(italic(alpha[J])),
     bty = "l")

alpha_P <-  alpha_P_temp(temps)
  plot(alpha_P~temps, 
       type = "l", 
       xlab = "Temperature", 
       ylab = quote(italic(alpha[P])),
       bty = "l")
    ifelse(temperature>lower & temperature < upper, prob, 0)
  }

phi_J <- function(temperature){
    TPC.q(temps, rmax = 0.99, Trmax = 29.5, acc = 80, dec.prop = 0.15)
  }


# Initial population 
n_initial <- c(100, 50, 20)  # Sample initial population size

