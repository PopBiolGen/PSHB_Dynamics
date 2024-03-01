# fit a continuous version of the model to data, using greta

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
  
  # do dispersal step here, by matrix-multiplying the P vector by a dispersal
  # matrix:
  #   P <- P %*% dispersal_matrix
  
  # recombine state matrix (sites by states)
  state <- abind(J, P, A, along = 2)
  
  # do stochastic dynamics bit here, by perturbing all states according to (a
  # continuous relaxation of) Poisson noise with precomputed latent N(0, 1)
  # noise:
  #   state <- continuous_poisson(latent, state)
  
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
  one_minus_alpha_J = one_minus_alpha_juvenile,
  one_minus_alpha_P = one_minus_alpha_preadult,
  one_minus_mu = one_minus_mu,
  parameter_is_time_varying = c("alpha_J", "one_minus_alpha_J",
                                "alpha_P", "one_minus_alpha_P",
                                "phi_J")
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
