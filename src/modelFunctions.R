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


# Basic recursion for the within-population model
step_within_population <- function(n_t, phi_J, alpha_J, phi_P, alpha_P, mu, f, phi_A) {
  #Transition matrix for within poopulation
  W <- matrix(c(phi_J * (1 - alpha_J), 0, f,
                phi_J * alpha_J, phi_P * (1-alpha_P) * (1 - mu), 0,
                0, phi_P * alpha_P * (1 - mu), phi_A), nrow = 3, byrow = TRUE)
  
  #1 time step for within-population
  W %*% n_t
}