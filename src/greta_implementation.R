library(greta.dynamics)

source("src/modelFunctions.R")

#\begin{align}
# J(t+1) &= \phi_J(1-\alpha_J)J(t) + fA(t\\
# P(t+1) &= \phi_J\alpha_JJ(t) + \phi_P(1-\alpha_P)(1-\mu)P(t) + 0 \\
# A(t+1) &= 0 + \phi_P\alpha_P(1-\mu)P(t) + \phi_AA(t)
# \end{align}

# * $f(t)$ is the mean fecundity of a female in the population in an interval of time 
# * $\alpha_i(t)$ are transition probabilities, describing transition between states
# * $\phi_i(t)$ are survival probabilities, describing probability of surviving an interval of time in each state 
# * $\mu(t)$ is the probability of dispersing away from the host tree.

# use fecundity estimate from modelfunctions.R
fecundity <- normal(f,
                    1,
                    truncation = c(0, Inf))
# hist(calculate(fecundity, nsim = 10000)[[1]], breaks = 100)

# alpha (transition to next life stage) is temperature dependent, so set a
# temperature and compute point estimates
temperature <- 24

alpha_juvenile <- normal(alpha_J_temp(temperature),
                  0.1,
                  truncation = c(0, 1))
# hist(calculate(alpha_juvenile, nsim = 10000)[[1]], breaks = 100)

alpha_preadult <- normal(alpha_P_temp(temperature),
                  0.1,
                  truncation = c(0, 1))
# hist(calculate(alpha_preadult, nsim = 10000)[[1]], breaks = 100)

# survival

phi_juvenile <- normal(phi_J_temp(temperature),
                       0.1,
                       truncation = c(0, 1))
# hist(calculate(phi_juvenile, nsim = 10000)[[1]], breaks = 100)

phi_preadult <- normal(phi_P,
                       0.1,
                       truncation = c(0, 1))
# hist(calculate(phi_preadult, nsim = 10000)[[1]], breaks = 100)

phi_adult <- normal(phi_A,
                    0.1,
                    truncation = c(0, 1))
# hist(calculate(phi_adult, nsim = 10000)[[1]], breaks = 100)

# no dispersal to other host trees for now
mu <- exponential(1/1e-5)


one_minus_alpha_juvenile <- 1 - alpha_juvenile
one_minus_alpha_preadult <- 1 - alpha_preadult
one_minus_mu <- 1 - mu

# FUTURE NICK G - IT'S FAILING BECAUSE YOU PUT CONSTANT 1s IN THE FUNCTION, DUMMY

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


initial_state_expected <- array(c(0.01, 0.01, 10), dim = c(1, 3, 1))
initial_state <- exponential(1 / initial_state_expected)

n_times <- 7 * 4

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
  one_minus_mu = one_minus_mu
)

# generate some test data
sim_pop <- sim_within_host(initial_n = c(0, 0, 10), 
                           temps = 24, 
                           iter = n_times, 
                           stochastic = TRUE)
obs_preadults_obs <- sim_pop[2, 1:n_times]
dim(obs_preadults_obs) <- c(1, 1, n_times)

expected_preadults <- states$all_states[1, 2, ]
distribution(obs_preadults_obs) <- poisson(expected_preadults)

m <- model(phi_juvenile,
           alpha_juvenile,
           fecundity,
           phi_preadult,
           alpha_preadult,
           phi_adult)

plot(m)

draws <- mcmc(m)

bayesplot::mcmc_trace(draws)
coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)

summary(draws)

prior_sims <- calculate(initial_state[1, 2, 1], phi_juvenile, alpha_juvenile, fecundity, phi_preadult, alpha_preadult, phi_adult,
                        nsim = 4000)

posterior_sims <- calculate(initial_state[1, 2, 1], phi_juvenile, alpha_juvenile, fecundity, phi_preadult, alpha_preadult, phi_adult,
                            nsim = 4000,
                            values = draws)

sapply(prior_sims, mean)
sapply(posterior_sims, mean)

priors <- do.call(cbind, lapply(prior_sims, c))
pairs(priors, cex = 0.4, pch = ".")

posteriors <- do.call(cbind, lapply(posterior_sims, c))
pairs(posteriors, cex = 0.4, pch = ".")
# sims <- calculate(states$all_states,
#                   fecundity, nsim = 1)
# # sims$fecundity[1, , ]
# sims$`states$all_states`[1, , ]
# 
# plot(sims$`states$all_states`[1, 1, ], type = "l")
# lines(sims$`states$all_states`[1, 2, ])
# lines(sims$`states$all_states`[1, 3, ])
# sims$fecundity




# 
# state <- transitions(
#   state = initial_state,
#   iter = 1,
#   phi_J = phi_juvenile,
#   alpha_J = alpha_juvenile,
#   fecundity = fecundity,
#   phi_P = phi_preadult,
#   alpha_P = alpha_preadult,
#   mu = mu,
#   phi_A = phi_adult,
#   one_minus_alpha_J = one_minus_alpha_juvenile,
#   one_minus_alpha_P = one_minus_alpha_preadult,
#   one_minus_mu = one_minus_mu
# )

