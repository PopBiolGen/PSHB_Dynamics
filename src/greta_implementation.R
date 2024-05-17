# fit a continuous version of the model to data, using greta


# additional functions:

# create a masking variable, near zero below lower and above upper, and at 0set x to (near) zero below lower and above upper
bound_mask <- function(x, lower = -Inf, upper = Inf, tol = 0.01, soft = FALSE) {
  # create a mask
  if (soft) {
    lower_mask <- plogis((x - lower) / tol)
    upper_mask <- plogis((upper - x) / tol)
  } else {
    lower_mask <- as.numeric(x > lower)
    upper_mask <- as.numeric(x < upper)
  }
  lower_mask * upper_mask
}

# par(mfrow = c(1, 1))
# x <- seq(0, 50, length.out = 1000)
# plot(bound_mask(x, 13.5, 31) ~ x, type = "l")
# lines(bound_mask(x, 13.5, 31, soft = TRUE, tol = 0.1) ~ x, col = "red")

# implemented bounded linear model for transition rates
bounded_linear <- function(temperature, intercept, slope, lower, upper, ...) {

  # create a mask to set values to 0 outside the allowed range
  mask <- bound_mask(x = temperature,
                     lower = lower,
                     upper = upper,
                     ...)
  rate <- intercept + slope * temperature
  prob <- 1 - exp(-rate)
  prob * mask
}


# note: we need to install the correct experimental branch of greta.dynamics:
# devtools::install_github("greta-dev/greta.dynamics@greta_2")
library(greta.dynamics)
library(tidyverse)

source("src/modelFunctions.R")

n_times <- 7 * 8
n_states <- 3
n_sites <- 5

expected_initial_pop <- c(0.1, 0.1, 30)

data <- sim_preadult_temp_data(n_sites = n_sites,
                               n_times = n_times,
                               expected_initial_pop = expected_initial_pop)

# create a matrix of temperatures (n_times x n_sites)
temperatures <- data %>%
  select(temperature, time, id) %>%
  pivot_wider(names_from = id,
              values_from = temperature) %>%
  select(-time) %>%
  as.matrix()

# create a matrix of observed counts (n_times x n_sites)
obs_preadults <- data %>%
  select(preadult_count, time, id) %>%
  pivot_wider(names_from = id,
              values_from = preadult_count) %>%
  select(-time) %>%
  as.matrix()

# and the true expected abundances (n_times x n_sites)
true_preadults <- data %>%
  select(true_preadult_abundance, time, id) %>%
  pivot_wider(names_from = id,
              values_from = true_preadult_abundance) %>%
  select(-time) %>%
  as.matrix()

# # plot these
# par(mfrow = c(2, 1))
# plot(temperatures[, 1],
#      type = "n",
#      ylim = range(temperatures))
# for(i in seq_len(n_sites)) {
#   lines(temperatures[, i],
#         col = i)
# }
# plot(true_preadults[, 1],
#      type = "n",
#      ylim = range(c(obs_preadults, true_preadults)))
# for(i in seq_len(n_sites)) {
#   lines(true_preadults[, i],
#         col = i)
#   points(obs_preadults[, i],
#          col = i)
# }

# define the model

# J(t+1) &= \phi_J(1-\alpha_J)J(t) + fA(t\\
# P(t+1) &= \phi_J\alpha_JJ(t) + \phi_P(1-\alpha_P)(1-\mu)P(t) + 0 \\
# A(t+1) &= 0 + \phi_P\alpha_P(1-\mu)P(t) + \phi_AA(t)

# use fecundity estimate from modelfunctions.R
fecundity <- normal(f, 1, truncation = c(0, Inf))
# hist(calculate(fecundity, nsim = 10000)[[1]], breaks = 100)

# alphas (transition to next life stage) and phi_J (juvenile survival) are
# temperature dependent, so hard-code these for now (we can re-estimate the
# curves later)

# define the temperatures in the correct dimensions for iterating
temps_array <- temperatures
dim(temps_array) <- c(n_times, n_sites, 1, 1)
stopifnot(near(temps_array[, , 1, 1], temperatures))

alpha_juvenile <- as_data(alpha_J_temp(temps_array))
alpha_preadult <- as_data(alpha_P_temp(temps_array))

# survival
phi_juvenile <- as_data(phi_J_temp(temps_array))

# survival for pre-adults and adults are temperature-independent, so temporally static. Infer these.
phi_preadult <- normal(phi_P, 0.1, truncation = c(0, 1))
phi_adult <- normal(phi_A, 0.1, truncation = c(0, 1))
# hist(calculate(phi_preadult, nsim = 10000)[[1]], breaks = 100)
# hist(calculate(phi_adult, nsim = 10000)[[1]], breaks = 100)

# no dispersal to other host trees for now
mu <- exponential(1 / 1e-5)

# latent N(0, 1) deviates for the stochastic transitions
latent_z_timeseries <- normal(0, 1, dim = c(n_times, n_sites, n_states, 1))

transitions <- function(state, iter,
                        phi_J,
                        alpha_J,
                        fecundity,
                        phi_P,
                        alpha_P,
                        mu,
                        phi_A,
                        latent_z) {
  # J(t+1) &= \phi_J(1-\alpha_J)J(t) + fA(t)\\
  # P(t+1) &= \phi_J \alpha_J J(t) + \phi_P(1-\alpha_P)(1-\mu)P(t) + 0 \\
  # A(t+1) &= 0 + \phi_P\alpha_P(1-\mu)P(t) + \phi_AA(t)
  
  J_old <- state[, 1, ]
  P_old <- state[, 2, ]
  A_old <- state[, 3, ]
  
  # J(t+1) &= \phi_J(1-\alpha_J)J(t) + fA(t)\\
  J <- phi_J * (1 - alpha_J) * J_old + fecundity * A_old
  
  # P(t+1) &= \phi_J \alpha_J J(t) + \phi_P(1-\alpha_P)(1-\mu)P(t) + 0 \\
  P <- phi_J * alpha_J * J_old + phi_P * (1 - alpha_P) * (1 - mu) * P_old
  
  # A(t+1) &= 0 + \phi_P\alpha_P(1-\mu)P(t) + \phi_AA(t)
  A <- phi_P * alpha_P * (1 - mu) * P_old + phi_A * A_old
  
  # do dispersal step here, by matrix-multiplying the P vector by a dispersal
  # matrix:
  #   P <- P %*% dispersal_matrix
  
  # recombine state matrix (sites by states)
  expected_state <- abind(J, P, A, along = 2)
  
  # do stochastic dynamics bit here, by perturbing all states according to (a
  # continuous relaxation of) Poisson noise with precomputed latent N(0, 1)
  # noise:
  state <- lognormal_continuous_poisson(expected_state, latent_z)
  
  state
  
}

# set initial states in the expected dimension
initial_state_expected <- matrix(c(0.01, 0.01, 10),
                                 n_sites,
                                 n_states, byrow = TRUE)
dim(initial_state_expected) <- c(n_sites, n_states, 1)

initial_state <- exponential(1 / initial_state_expected)

states <- iterate_dynamic_function(
  transition_function = transitions,
  initial_state = initial_state,
  niter = n_times,
  tol = 0,
  phi_J = phi_juvenile,
  alpha_J = alpha_juvenile,
  fecundity = fecundity,
  phi_P = phi_preadult,
  alpha_P = alpha_preadult,
  mu = mu,
  phi_A = phi_adult,
  latent_z = latent_z_timeseries,
  parameter_is_time_varying = c("alpha_J",
                                "alpha_P",
                                "phi_J",
                                "latent_z"),
  # clamp the simulated state to reasonable values
  state_limits = c(1e-3, 1e5)
)

# define the likelihood only on the abundance of pre-adults,
# reshaping to match the observation matrix
expected_preadults <- aperm(states$all_states[, 2, ], c(3, 1, 2))
dim(expected_preadults) <- c(n_times, n_sites)
distribution(obs_preadults) <- poisson(expected_preadults)

m <- model(fecundity,
           phi_preadult,
           phi_adult)

# plot(m)

# do inference

n_chains <- 4

# # with the added stochasticity, it's hard for greta to automatically find valid
# # initial values. So we can use some external hacking to define some that should
# # work (this should be implemented in greta some time). This takes a couple of
# # minutes because very few prior sims are valid (have finite gradients)
# inits <- generate_valid_inits(m, n_chains)

random_clamped_normal <- function(mean, sd, min = -Inf, max = Inf, dim = c(1, 1)) {
  x <- rnorm(prod(dim), mean, sd)
  x <- pmin(x, max)
  x <- pmax(x, min)
  dim(x) <- dim
  x
}

# alternately, we can set the stochastic noise at the median value, to
# approximately recover the deterministic behaviour
inits <- replicate(n_chains,
                   initials(
                     fecundity = random_clamped_normal(f,
                                                       0.1,
                                                       min = 1e-3),
                     phi_preadult = random_clamped_normal(phi_P,
                                                          0.1,
                                                          min = 1e-3,
                                                          max = 1 - 1e-3),
                     phi_adult = random_clamped_normal(phi_A,
                                                       0.1,
                                                       min = 1e-3,
                                                       max = 1 - 1e-3),
                     latent_z_timeseries = array(0,
                                                 dim(latent_z_timeseries))),
                   simplify = FALSE)

draws <- mcmc(m,
              chains = n_chains,
              initial_values = inits)

# check convergence
bayesplot::mcmc_trace(draws)
coda::gelman.diag(draws,
                  autoburnin = FALSE,
                  multivariate = FALSE)

summary(draws)

# compare prior and posterior means
prior_sims <- calculate(initial_state[1, 2, 1], fecundity, phi_preadult, phi_adult,
                        nsim = 4000)
posterior_sims <- calculate(initial_state[1, 2, 1], fecundity, phi_preadult, phi_adult,
                            nsim = 4000,
                            values = draws)
sapply(prior_sims, mean)
sapply(posterior_sims, mean)

# correlation in priors and posteriors
priors <- do.call(cbind, lapply(prior_sims, c))
pairs(priors, cex = 0.4, pch = ".")

posteriors <- do.call(cbind, lapply(posterior_sims, c))
pairs(posteriors, cex = 0.4, pch = ".")

# strong correlation in pre-adult and adult survival parameters? Not surprising
# given we only observe one state

# plot prior and posterior estimates and 90% CIs of the expected
# (without Poisson sampling variation) numbers of pre-adults
prior_ests <- calculate(expected_preadults,
                            nsim = 4000)[[1]]
prior_mean <- apply(prior_ests, 2:3, mean)
prior_ci <- apply(prior_ests, 2:3, quantile, c(0.025, 0.975))

posterior_ests <- calculate(expected_preadults,
                            nsim = 4000,
                            values = draws)[[1]]
post_mean <- apply(posterior_ests, 2:3, mean)
post_ci <- apply(posterior_ests, 2:3, quantile, c(0.025, 0.975))

par(mfrow = n2mfrow(n_sites),
    mar = c(3, 4, 2, 2) + 0.1)
for (i in seq_len(n_sites)) {
  plot(prior_mean[, i],
       type = "l",
       ylim = range(c(obs_preadults[, i], post_ci[, , i])),
       col = grey(0.8),
       ylab = "preadults",
       xlab = "")
  lines(prior_ci[1, , i],
        lty = 2,
        col = grey(0.8))
  lines(prior_ci[2, , i],
        lty = 2,
        col = grey(0.8))
  lines(post_mean[, i])
  lines(post_ci[1, , i], lty = 2)
  lines(post_ci[2, , i], lty = 2)
  
  # plot the truth (true expected abundance for this temperature timeseries and
  # initial condition)
  lines(true_preadults[, i],
        lty = 3,
        lwd = 2,
        col = "blue")
  
  # overplot the observed counts
  points(obs_preadults[, i],
         cex = 0.5)
  
}

# to do:

# implement dispersal between host trees

