# do 1D version of stochastic dynamics in greta.dynamics

# simulate a discrete stochastic growth rate population model
set.seed(3)
n_times <- 50
# daily population growth rate
r_true <- 1.01
# initial population (just before timeseries)
pop_init_true <- 25

time <- seq_len(n_times)
pop_true <- rep(NA, n_times)
pop_previous <- pop_init_true
for (i in 1:n_times) {
  pop_true[i] <- rpois(1, pop_previous * r_true)
  pop_previous <- pop_true[i]
}

# add an observation process (binomial with fixed detection probability)
obs_prob <- 0.7
pop_obs <- rbinom(n_times, size = pop_true, prob = obs_prob)

# # plot observed and true populations
# plot(pop_obs ~ time,
#      pch = 16,
#      ylim = range(0, max(pop_true)),
#      ylab = "population")
# # truth
# lines(pop_true)
# 
# # point estimates with detection probability correction
# points(pop_obs / obs_prob ~ time)

# define functions for continuous relaxation and reparameterisation of the
# Poisson distributions

# given random variables z (with standard normal distribution a priori), and
# Poisson rate parameter lambda, return a strictly positive continuous random
# variable with the same mean and variance as a poisson random variable with
# rate lambda, by approximating the poisson as a lognormal distribution. This
# has the advantage of decentring the posterior distribution of these random
# variables in MCMC, as well as enabling inference on them with HMC.
# Note annoying two = 2 thing is a hack to workaround the current bug in
# greta.dynamics when defining constants inside a transition function.
lognormal_continuous_poisson <- function(lambda, z) {
  sigma <- sqrt(log1p(lambda / exp(2 * log(lambda))))
  # sigma2 <- log1p(1 / lambda)
  mu <- log(lambda) - sigma^2 / 2
  exp(mu + z * sigma)
}


# Continuous approximation of the Poisson distribution. Model the continuous
# Poisson as a lognormal given lambda, and a standard normal variate z. Do this
# by calculating the mu and sigma to be combined with z such that when
# exponentiated, it has the same mean and variance as the intended poisson RV.

# Working: The lognormal mean and variance should both equal lambda. The
# lognormal mean and variance can both be expressed in terms of the parameters
# mu and sigma.

# mean = lambda = exp(mu + sigma^2 / 2)
# variance = lambda = (exp(sigma ^ 2) - 1) * exp(2 * mu + sigma ^ 2)

# solve to get sigma and mu as a function of lambda:
# mu = log(lambda) - sigma^2 / 2
# lambda = (exp(sigma ^ 2) - 1) * exp(2 * mu + sigma ^ 2)
# lambda = (exp(sigma ^ 2) - 1) * exp(2 * (log(lambda) - sigma^2 / 2) + sigma ^ 2)
# lambda = (exp(sigma ^ 2) - 1) * exp(sigma ^ 2) * exp(2 * (log(lambda) - sigma^2 / 2))
# lambda = (exp(sigma ^ 2) - 1) * exp(sigma ^ 2) * exp(2 * log(lambda) - sigma^2)
# lambda = (exp(sigma ^ 2) - 1) * exp(sigma ^ 2) * exp(2 * log(lambda)) / exp(sigma^2)
# lambda / exp(2 * log(lambda)) = (exp(sigma ^ 2) - 1) * exp(sigma ^ 2) * 1 / exp(sigma^2)
# lambda / exp(2 * log(lambda)) = (exp(sigma ^ 2) - 1)
# log(lambda / exp(2 * log(lambda)) + 1) = sigma ^ 2
# sigma = sqrt(log(lambda / exp(2 * log(lambda)) + 1)) = sigma
# mu = log(lambda) - sigma^2 / 2

# # check these numerically
# library(tidyverse)
# compare <- tibble(
#   lambda = seq(0.01, 1000, length.out = 100)
# ) %>%
#   mutate(
#     sigma = sqrt(log(lambda / exp(2 * log(lambda)) + 1)),
#     mu = log(lambda) - sigma^2 / 2
#   ) %>%
#   mutate(
#     mean = exp(mu + sigma^2 / 2),
#     variance = (exp(sigma ^ 2) - 1) * exp(2 * mu + sigma ^ 2)
#   ) %>%
#   mutate(
#     diff_mean_variance = abs(mean - variance),
#     diff_mean_lambda = abs(mean - lambda),
#     diff_variance_lambda = abs(variance - lambda)
#   ) %>%
#   summarise(
#     across(
#       starts_with("diff"),
#       ~max(.x)
#     )
#   )

# given poisson rate parameter lambda and random uniform deviate u, a continuous
# relaxation of poisson random variable generation is computed using the inverse
# of the incomplete gamma function. ie. igammainv(lambda, 1 - u) is
# approximately equal to qpois(u, lambda) (and exactly equal to qgamma(1 - u,
# lambda) in the R implementation)
gamma_continuous_poisson <- function(lambda, u) {
  # if we want to interpret u as random quantiles of the distribution, it should
  # be this:
  #   igammainv(lambda, 1 - u)
  # but we omit the one minus because it's interacting with a bug in
  # greta.dynamics, and because it doesn't affect inference
  igammainv(lambda, u)
}
# # check:
# lambda <- as_data(pi)
# u <- uniform(0, 1)
# y <- gamma_continuous_poisson(lambda, 1 - u)
# sims <- calculate(y, u, nsim = 1e5)
# max(abs(sims$y - qgamma(1 - sims$u, pi)))
# quantile(round(sims$y))
# quantile(rpois(1e5, pi))

# the inverse incomplete gamma function (the major part of the quantile function
# of a gamma distribution)
igammainv <- function(a, p) {
  op <- greta::.internals$nodes$constructors$op
  op("igammainv", a, p,
     tf_operation = "tf_igammainv"
  )
}
tf_igammainv <- function(a, p) {
  tfp <- greta:::tfp
  tfp$math$igammainv(a, p)
}


library(greta.dynamics)

# growth rate
r <- normal(1, 0.1, truncation = c(0, Inf))

# latent variable for stochastic transitions
latent_z_vec <- normal(0, 1, dim = n_times)
# latent_u_vec <- uniform(0, 1, dim = n_times)

# initial population size (with mean equal to truth)
init <- exponential(1 / pop_init_true)

# transition function for the population process as a difference equation in
# integer timestep
fun <- function(state, iter, r, latent_z) {
  lambda <- state * r
  state_new <- lognormal_continuous_poisson(lambda, latent_z)
  # state_new <- gamma_continuous_poisson(lambda, latent_u)
  state_new
}

# solve it (integer times)
pop_sim <- iterate_dynamic_function(
  transition_function = fun,
  initial_state = init,
  niter = n_times,
  tol = 0,
  r = r,
  latent_z = latent_z_vec,
  parameter_is_time_varying = "latent_z",
  # clamp the populations to reasonably values to avoid numerical under/overflow
  state_limits = c(1e-3, 1e3)
)

# get the modelled true population
pop_modelled <- t(pop_sim$all_states)
# and the expected value of the observation distribution
pop_obs_expected <- pop_modelled * obs_prob
pop_obs_ga <- as_data(pop_obs)
distribution(pop_obs_ga) <- poisson(pop_obs_expected)

# fit this, reducing the stochasticity when finding the initial values
n_chains <- 4
inits <- replicate(n_chains,
                   initials(
                     init = pop_init_true,
                     latent_z_vec = rnorm(n_times, 0, 0.1)),
                   simplify = FALSE)

m <- model(r, latent_z_vec, init)
draws <- mcmc(m,
              initial_values = inits)

# check convergence
coda::gelman.diag(draws,
                  autoburnin = FALSE,
                  multivariate = FALSE)

# get posterior samples and plot summaries
sims <- calculate(pop_modelled, nsim = 1000, values = draws)
posterior_sims <- sims$pop_modelled[, , 1]
posterior_sims_discrete <- round(posterior_sims)
posterior_est <- colMeans(posterior_sims_discrete)
posterior_ci <- apply(posterior_sims_discrete, 2, quantile, c(0.025, 0.975))

# plot posterior draws, summary stats, and the truth
plot(posterior_est,
     ylim = range(c(posterior_ci, pop_true)),
     type = "n",
     xlab = "day",
     ylab = "population")
for (i in 1:50) {
  lines(posterior_sims_discrete[i, ],
        lwd = 0.1,
        col = grey(0.4))
}
lines(posterior_ci[1, ], lty = 2)
lines(posterior_ci[2, ], lty = 2)
lines(posterior_est,
     lwd = 1.5)
lines(pop_true,
      col = "blue",
      pch = 16,
      cex = 0.5)
# noisy observations, naively adjusted for detection probability
points(pop_obs / obs_prob,
       cex = 0.5)


summary(calculate(r, values = draws))
r_true

# simulate data from posteriors

# there's a bug with Poisson sampling with NaN parameters that make this hang,
# so force to GPU for now
options(greta_gpu_message = FALSE)
n_sims <- 100
sims <- calculate(pop_obs_ga, 
                  nsim = n_sims,
                  values = draws,
                  compute_options = gpu_only())

plot(sims$pop_obs_ga[i, , 1] ~ time,
     type = "n",
     ylim = c(0, max(c(pop_obs, sims$pop_obs_ga))))

for (i in 1:n_sims) {
  points(sims$pop_obs_ga[i, , 1] ~ time,
         pch = 16,
         col = grey(0.4, 0.1),
         cex = 0.5)
}
points(pop_obs,
       pch = 16,
       cex = 0.6,
       col = "blue")



# to do in greta.dynamics:

# add the gamma and lognormal continuous approximations to the Poisson
# distribution as functions and with this 1D simulation as an example.
