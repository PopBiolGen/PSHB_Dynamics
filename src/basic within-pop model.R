## ---------------------------
##
## Script name: basic-within-pop-model.R
##
## Purpose of script: Run and display numerical realisations of the within-population PSHB model
##
##
## Date Created: 2023-11-21
##
## Email: ben.l.phillips@curtin.edu.au; kanishkwalavalkar@gmail.com
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


# Sample parameters
phi_J <- 0.9
alpha_J <- 0.1
phi_P <- 0.8
alpha_P <- 0.2
mu <- 0.05
f <- 2.0
phi_A <- 0.95

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
