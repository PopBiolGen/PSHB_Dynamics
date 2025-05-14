## ---------------------------
##
## Script name: dispersal-inference.R
##
## Purpose of script: Make an inference of the PSHB dispersal kernel, based on mark-release-recapture data (Owens et al. 2019)
## 
## ---------------------------

library(readr)
library(dplyr)
library(ggplot2)

recap <- read_csv("src/dispersal/owens_recap.csv") # Load mark-recap data (from Owens et al.)

ps <- aggregate(ntrap ~ trapID + dist, data = recap, FUN = sum) # Aggregate across reps
ps <- ps[-(ps$trapID=="RP"),] # Ignore beetles remaining at release point
colnames(ps)[2:3] <- c('d', 'obs')
nr <- sum(subset(recap, trapID=='RP')$nfly) # total n PSHB that dispersed

##### Density function for 2-dimensional Gaussian distribution:

dfun <- function(d, dens0, sigma, radius){

  d.at.d <- (dens0 / (2 * pi * sigma^2)) * exp(-(d^2 / (2 * sigma^2)))
  
  # expected count
  d.at.d * pi * radius^2 # Assume detect all individuals dispersing within a trap's detection radius
}

#### Make Bayesian inference ####

library(greta)

# name data variables
d.obs <- as_data(ps$obs)
d.d <- as_data(ps$d)

# define priors
d.sigma <- uniform(0, 300) # possible dispersal sigmas
t.rad <- uniform(0, 100) # possible radii

# define model
expected.count <- dfun(d.d, nr, sigma = d.sigma, radius = t.rad)
distribution(d.obs) <- poisson(expected.count)

g.mod <- model(d.sigma, t.rad)
plot(g.mod)

# sample the posterior
draws <- mcmc(g.mod, n_samples = 1000, chains = 4)

# did it converge?
coda::gelman.diag(draws)
bayesplot::mcmc_trace(draws)

# How close did we come to input parameter values?
summary(draws)

d.sigma <- 25.63 # THIS IS SIGMA (to estimate diffusion coeff)
t.rad <- 5.35

### Check observed against predicted
ps$lambda <- dfun(ps$d, nr, # Run dfun
                  d.sigma, 
                  t.rad) # with values from summary(draws)
ps$pred <- rpois(nrow(ps), ps$lambda)
# Do predictions fit with observed data?
ggplot()+
  geom_point(data=ps,
             aes(x=d, y=obs), col="blue")+
  geom_point(data=ps,
             aes(x=d, y=pred), col="red")

# Draw smooth curve: 
ps <- data.frame(d = seq(0, 130, by=1))
ps$lambda <- dfun(ps$d, 1e9, # Run dfun
                  d.sigma, 
                  t.rad) # with values from summary(draws)
ps$pred <- rpois(nrow(ps), ps$lambda)
ggplot()+
  geom_line(data=ps,
             aes(x=d, y=pred), col="red")




