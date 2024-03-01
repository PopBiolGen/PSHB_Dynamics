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

source("src/modelFunctions.R")

n_times <- 7 * 8
n_states <- 3
n_sites <- 1

# create fake noisy response data
obs_preadults_truth <- population_data[2, 1:n_times] * 10
obs_preadults_obs <- rpois(length(obs_preadults_truth), obs_preadults_truth)

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
temps_array <- array(temps[1:n_times], c(n_times, 1, 1))

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

# FUTURE NICK G - IT'S FAILING BECAUSE YOU PUT CONSTANT 1s IN THE FUNCTION, DUMMY

one_minus_alpha_juvenile <- 1 - alpha_juvenile
one_minus_alpha_preadult <- 1 - alpha_preadult
one_minus_mu <- 1 - mu

transitions <- function(state, iter,
                        phi_J,
                        alpha_J,
                        fecundity,
                        phi_P,
                        alpha_P,
                        mu,
                        phi_A,
                        one_minus_alpha_J,
                        one_minus_alpha_P,
                        one_minus_mu) {
  # J(t+1) &= \phi_J(1-\alpha_J)J(t) + fA(t)\\
  # P(t+1) &= \phi_J \alpha_J J(t) + \phi_P(1-\alpha_P)(1-\mu)P(t) + 0 \\
  # A(t+1) &= 0 + \phi_P\alpha_P(1-\mu)P(t) + \phi_AA(t)
  
  J_old <- state[, 1, ]
  P_old <- state[, 2, ]
  A_old <- state[, 3, ]
  
  # J(t+1) &= \phi_J(1-\alpha_J)J(t) + fA(t)\\
  J <- phi_J * one_minus_alpha_J * J_old + fecundity * A_old
  
  # P(t+1) &= \phi_J \alpha_J J(t) + \phi_P(1-\alpha_P)(1-\mu)P(t) + 0 \\
  P <- phi_J * alpha_J * J_old + phi_P * one_minus_alpha_P * one_minus_mu * P_old
  
  # A(t+1) &= 0 + \phi_P\alpha_P(1-\mu)P(t) + \phi_AA(t)
  A <- phi_P * alpha_P * one_minus_mu * P_old + phi_A * A_old
  
  state <- abind(J, P, A, along = 2)
  state
  
}

initial_state_expected <- array(c(0.01, 0.01, 10),
                                dim = c(n_sites, n_states, 1))
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
  one_minus_alpha_J = one_minus_alpha_juvenile,
  one_minus_alpha_P = one_minus_alpha_preadult,
  one_minus_mu = one_minus_mu,
  parameter_is_time_varying = c("alpha_J", "one_minus_alpha_J",
                                "alpha_P", "one_minus_alpha_P",
                                "phi_J")
)


# define the likelihood only on the abundance of pre-adults
expected_preadults <- states$all_states[, 2, ]
# reshape the observations to match this
dim(obs_preadults_obs) <- c(n_sites, 1, n_times)
distribution(obs_preadults_obs) <- poisson(expected_preadults)

m <- model(fecundity,
           phi_preadult,
           phi_adult)

# plot(m)

# do inference
draws <- mcmc(m)

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

# plot prior and posterior estimates and 90% CIs of the expected
# (without Poisson sampling variation) numbers of pre-adults
prior_ests <- calculate(expected_preadults,
                            nsim = 4000)[[1]][, 1, 1, ]
prior_mean <- colMeans(prior_ests)
prior_ci <- apply(prior_ests, 2, quantile, c(0.025, 0.975))

posterior_ests <- calculate(expected_preadults,
                            nsim = 4000,
                            values = draws)[[1]][, 1, 1, ]
post_mean <- colMeans(posterior_ests)
post_ci <- apply(posterior_ests, 2, quantile, c(0.025, 0.975))
par(mfrow = c(1, 1))
plot(prior_mean,
     type = "l",
     ylim = range(c(obs_preadults_obs, post_ci)),
     col = grey(0.8))
lines(prior_ci[1, ],
      lty = 2,
      col = grey(0.8))
lines(prior_ci[2, ],
      lty = 2,
      col = grey(0.8))
lines(post_mean)
lines(post_ci[1, ], lty = 2)
lines(post_ci[2, ], lty = 2)

# overplot the observed counts
points(obs_preadults_obs)

# to do:

# expand out to multiple host trees

# implement dispersal between host trees

# implement stochastic dispersal/population growth with continuous poisson




transitions(
  state = initial_state,
  iter = 1,
  phi_J = phi_juvenile[1, , ],
  alpha_J = alpha_juvenile[1, , ],
  fecundity = fecundity,
  phi_P = phi_preadult,
  alpha_P = alpha_preadult[1, , ],
  mu = mu,
  phi_A = phi_adult,
  one_minus_alpha_J = one_minus_alpha_juvenile[1, , ],
  one_minus_alpha_P = one_minus_alpha_preadult[1, ,],
  one_minus_mu = one_minus_mu
)
