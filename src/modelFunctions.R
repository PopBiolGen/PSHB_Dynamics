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


# Basic recursion for the within-population model with density dependence
step_within_population <- function(n_t, phi_J, alpha_J, phi_P, alpha_P, mu, f, phi_A, K) {
  #Calculate the density dependence term based on current population size and carrying capacity (K)
  density_dependence <- 1-n_t / K
  #Ensure density_dependence has length 3
  density_dependence <- rep(1-n_t / K, each = 3)
  #Ensure the value of density_dependence is between 0 and 1
  density_dependence <- pmax(0, pmin(density_dependence, 1))

  #Modified transition matrix for within poopulation with density dependence
  W <- matrix(c(phi_J * (1 - alpha_J) * density_dependence, 0, f * density_dependence,
                phi_J * alpha_J * density_dependence, phi_P * (1-alpha_P) * (1 - mu) * density_dependece, 0,
                0, phi_P * alpha_P * (1 - mu) * density_dependence, phi_A * density_dependence), nrow = 3, byrow = TRUE)
  
  #1 time step for within-population
  W %*% n_t
}
