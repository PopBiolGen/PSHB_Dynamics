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
## ---------------------------

## load up our functions into memory 
source("src/modelFunctions.R")


# Parameters
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
phi_J <- function(temperature){
    TPC.q(temps, rmax = 0.99, Trmax = 29.5, acc = 80, dec.prop = 0.15)
  }
alpha_P <-  alpha_P_temp(temps)
  plot(alpha_P~temps, 
       type = "l", 
       xlab = "Temperature", 
       ylab = quote(italic(alpha[P])),
       bty = "l")
    ifelse(temperature>lower & temperature < upper, prob, 0)
  }

phi_P <- 0.97
phi_J <- function(temperature){
    TPC.q(temps, rmax = 0.99, Trmax = 29.5, acc = 80, dec.prop = 0.15)
  }
phi_A <- 0.97
mu <- 0
f <- 0.69

# Initial population 
n_initial <- c(100, 50, 20)  # Sample initial population size

num_steps <- 30

population_trajectory <- matrix(NA, nrow = num_steps + 1, ncol = 3)
population_trajectory[1, ] <- n_initial

for (t in 1:num_steps) {
  n_t_plus_1 <- step_within_population(population_trajectory[t, ], phi_J, alpha_J, phi_P, alpha_P, mu, f, phi_A)
  
  population_trajectory[t + 1, ] <- n_t_plus_1
}

print("Population Trajectory:")
print(population_trajectory)

# make a plot
matplot(population_trajectory, type = "l", bty = "l")
