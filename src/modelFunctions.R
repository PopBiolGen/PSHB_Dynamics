## ---------------------------
##
## Script name: modelFunctions.R
##
## Purpose of script: Functions for running and plotting numerical solutions to PSHB models
##
##
## Date Created: 2023-11-21
##
## Email: ben.l.phillips@curtin.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 
library(deSolve)
## ---------------------------

## load up our functions into memory

## ---------------------------


# The within gallery system
# pars are list() 
LVComp <- function(t, N, pars){
  list(c(
    pars$r1*N[1]*(1-(N[1]+pars$a12*N[2])/pars$K1),
    pars$r2*N[2]*(1-(N[2]+pars$a21*N[1])/pars$K2)
  )
  )
}

# Solve it
# N0 is a vector of initial N1, N2
# maxTime is maximum number of generations
# # pars are list r1, r2, a12, a21, K
LVSolve <- function(N0, maxTime = 100, pars){
  as.data.frame(ode(y = N0, times = 0:maxTime, func = LVComp, parms = pars))
}