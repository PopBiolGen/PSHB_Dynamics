#Code for within-population model
step_within_population <- function(n_t, phi_J, alpha_J, phi_P, alpha_P, mu, f, phi_A) {
  #Transition matrix for within poopulation
  W <- matrix(c(phi_J * (1 - alpha_J), 0, f,
                phi_J * alpha_J, phi_P * (1-alpha_P) * (1 - mu), 0,
                0, phi_P * alpha_P * (1 - mu), phi_A), nrow = 3, byrow = TRUE)
  
  #1 time step for within-population
  n_t_plus_1 <- W %*% n_t
  
  return(n_t_plus_1)
}

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

num_steps <- 10

population_trajectory <- matrix(NA, nrow = num_steps + 1, ncol = 3)
population_trajectory[1, ] <- n_initial

for (t in 1:num_steps) {
  n_t_plus_1 <- step_within_population(population_trajectory[t, ], phi_J, alpha_J, phi_P, alpha_P, mu, f, phi_A)
  
  population_trajectory[t + 1, ] <- n_t_plus_1
}

print("Population Trajectory:")
print(population_trajectory)
