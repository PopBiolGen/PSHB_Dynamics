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
  survival_prob <- ifelse(cumulative_offspring < survival_threshold, phi_J, 0)
  
  # Transition matrix for within population with density-dependent survival
  W <- matrix(c(survival_prob * (1 - alpha_J), 0, f,
                survival_prob * alpha_J, phi_P * (1 - alpha_P) * (1 - mu), 0,
                0, phi_P * alpha_P * (1 - mu), phi_A), nrow = 3, byrow = TRUE)
  
  # Calculate the number of offspring produced in this time step
  offspring_produced <- f * n_t[3]  # Adults contribute to offspring based on fA
  offspring_produced <- offspring_produced_juveniles + offspring_produced_adults
  
  
  # Update cumulative offspring
  cumulative_offspring <- cumulative_offspring + offspring_produced
  
  # 1 time step for within-population
  n_tplus <- W %*% n_t
  
  # Return both the updated population vector and cumulative offspring
  return(list(n_tplus, cumulative_offspring))
}
