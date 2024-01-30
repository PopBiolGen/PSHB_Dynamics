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


# Basic recursion for the within-population model with cumulative offspring
step_within_population <- function(n_t, phi_J, alpha_J, phi_P, alpha_P, mu, f, phi_A, cumulative_offspring) {
  # Transition matrix for within population
  W <- matrix(c(phi_J * (1 - alpha_J), 0, f,
                phi_J * alpha_J, phi_P * (1 - alpha_P) * (1 - mu), 0,
                0, phi_P * alpha_P * (1 - mu), phi_A), nrow = 3, byrow = TRUE)
  
  # Calculate the number of offspring produced in this time step
  offspring_produced <- sum(W[1, ] * n_t)  # Assumes juveniles contribute to offspring

  # Update cumulative offspring
  cumulative_offspring <- cumulative_offspring + offspring_produced

  # 1 time step for within-population
  n_tplus <- W %*% n_t

  # Return both the updated population vector and cumulative offspring
  return(list(n_tplus, cumulative_offspring))
}
