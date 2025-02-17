# A script to test a model for making inference form the dispersal data

## Simulate some data

# a grid of sampling points
ps <- expand.grid(list(x = seq(-130, 130, 30), y = seq(-130, 130, 30)))
# distances from the release point, at (0,0)
ps$d <- sqrt(ps$x^2 + ps$y^2)

# describes expected count at distance d according to 2D Gaussian function with some detection radius r around traps
dfun <- function(d, dens0, sigma, radius){
  # density at distance d according to 2D Gaussian function
  d.at.d <- dens0 / sqrt(2*pi*sigma^2) * exp(-(d^2/(2*sigma^2))) / (2*pi*d)
  # expected count
  d.at.d * pi * radius^2
}

# expected counts assuming nr released and sigma = s
nr <- 1000
s <- 47
r <- 10
ps$lambda <- dfun(ps$d, nr, s, r)
# observed counts
ps$obs <- rpois(nrow(ps), ps$lambda)


## Make inference

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
