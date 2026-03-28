# fit a continuous version of the model to data, using greta

# note: we need to install the correct experimental branch of greta.dynamics:
# devtools::install_github("greta-dev/greta.dynamics@greta_2")
library(greta.dynamics)
library(tidyverse)

# get the model functions
source("src/modelFunctions.R")

# simulate data
n_times <- 7 * 8 # time-steps
n_states <- 3 # life stages
n_sites <- 5 # sites

expected_initial_pop <- c(0.1, 0.1, 30) # Initial population size for each life stgae (I THINK??)

### sim_preadult_temp_data (which calls tree_temp_prediction & step_within_population functions)
# requires lat, long, sf_oz & mu parameters
library(ozmaps)
library(sf)
sf_oz <- subset(ozmap("country"))
locLat <- -31.96165
locLong <- 115.8317
mu_est <- 0; mu_disp_est <- 0; phi_mu_est <- 1


# Simulate pop data ('true' and 'observed')
data <- sim_preadult_temp_data(n_sites = n_sites,
                               n_times = n_times,
                               expected_initial_pop = expected_initial_pop)

# sim_preadult_temp_data runs sim_single_preadult_temp_data

# sim_single_preadult_temp_data calculates tree temp for a location (using tree_temp_prediction) and subsets time-steps equal to n_times (randomly starting at any doy)
#   Then for each life stage (n_states) randomly draw starting pop from exponential distribution (with rate = 1/expected_initial_pop)

# Then run sim_within_host

# sim_within_host runs step_within_population
#   step_within_population is main simulation model (calculates pop parameters based on temp, creates transition matrix, runs matrix multiplication for N over time-step)
# sim_within_host produces N matrix (population size per life stage per t), with final row = cumulative offspring

# sim_single_preadult_temp_data then looks at just preadult abundance (in output matrix) - saved as 'true_preadult_abundance'
#   Then for each t randomly draw from a poisson distribution (with lambda = true preadult n) to give 'preadult_count'
#       (to account for observation noise process)??

# sim_preadult_temp_data runs sim_single_preadult_temp_data multiple times (for each site) (starting pop randomly drawn each time) and combines into one data set



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
 par(mfrow = c(2, 1))
 plot(temperatures[, 1],
      type = "n",
      ylim = range(temperatures))
 for(i in seq_len(n_sites)) {
   lines(temperatures[, i],
         col = i)
 }
 plot(true_preadults[, 1],
      type = "n",
      ylim = range(c(obs_preadults, true_preadults)))
 for(i in seq_len(n_sites)) {
   lines(true_preadults[, i],
         col = i)
   points(obs_preadults[, i],
          col = i)
 }


# define the model

# create greta arrays from prior definitions
 # A script for building priors for the PSHB model
 # Details are provided in modelDescriptions.Rmd "## Estimating priors"
 # This script follows those details but then places the priors on nice supports
 # Outputs a list specifying full priors for each parameter
PSHB_priors <- prior_calculator()

PSHB_priors_list <- lapply(PSHB_priors, define_prior_list) # turn list into list of greta arrays

# use fecundity estimate from modelfunctions.R
fecundity <-  PSHB_priors_list$fecundity$fecundity

# alphas (transition to next life stage) and phi_J (juvenile survival) are
# temperature dependent, so hard-code these for now (we can re-estimate the
# curves later)

# define the temperatures in the correct dimensions for iterating
temps_array <- temperatures
dim(temps_array) <- c(n_times, n_sites, 1, 1)
stopifnot(near(temps_array[, , 1, 1], temperatures))

# create temperature-dependent effects from priors
alpha_juvenile <- TPC_temp(temps_array, PSHB_priors_list$alpha_J) 
alpha_preadult <- TPC_temp(temps_array, PSHB_priors_list$alpha_P)
# TPC_temp calls TPC.pshb - create TPC curve using temperature (for each temp in temps_array) & the parameters (stored as PRIORS in list of greta arrays)


# survival
phi_juvenile <- TPC_temp(temps_array, PSHB_priors_list$phi_J)

# survival for pre-adults and adults are temperature-independent, so temporally
# static. Infer these.
phi_preadult <- PSHB_priors_list$phi_P$phi_P
phi_adult <- phi_preadult

# no dispersal to other host trees (just leaving the known universe)
mu <- PSHB_priors_list$phi_mu$phi_mu

# latent N(0, 1) deviates for the stochastic transitions
latent_z_timeseries <- normal(0, 1, dim = c(n_times, n_sites, n_states, 1))

#
# # example of making a dispersal matrix, incorporating mu (dispersal fraction)
# and extra probability of survival for dispersers (1 - dispersal death
# probability)

 dispersal_range <- lognormal(-3, 0.1)
 hist(calculate(dispersal_range, nsim = 1000)[[1]])
 dispersal_survival <- 0.5
#
 coords <- matrix(runif(n_sites * 2), ncol = 2) 
 distances <- as.matrix(dist(coords)) 
 unnormalised_dispersal <- exp(-distances / dispersal_range) 
 # make the fraction dispersing equal to mu
 diag(unnormalised_dispersal) <- 0 
 sums <- colSums(unnormalised_dispersal)
 unnormalised_dispersal <- sweep(unnormalised_dispersal, 2, sums, FUN = "/")
 normalised_dispersal <- unnormalised_dispersal * mu * dispersal_survival +
   diag(n_sites) * (1 - mu)

 calculate(colSums(normalised_dispersal), nsim = 1)[[1]][1, , ]


# transitions function to be used as transition_function in dynamics function below. From doc:
# "a function taking in the previous population state and the current iteration (and possibly other greta arrays)
# and returning the population state at the next iteration. 
# The first two arguments must be named 'state' and 'iter', the state vector and scalar iteration number respectively. 
# The remaining parameters must be named arguments representing (temporally static) model parameters. Variables and distributions cannot be defined inside the function."


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
  
## This is where I effectively rewrite the population model (?)  
  # Population transitions here life-stage by life-stage rather than combining in matrix multiplication
  # but parameters (phi_J, alpha_J, etc) are actually arrays (time-steps x site)
  
  # J(t+1) &= \phi_J(1-\alpha_J)J(t) + fA(t)\\
  J <- phi_J * (1 - alpha_J) * J_old + fecundity * A_old
  
  # pre-adults that have either left or stayed (incorporate mu parameter in
  # calculation of off-diagonals and make columns sum to 1)
  
  P_disperse <- P_old %*% dispersal_matrix # Need to figure out matrix multiplication with greta arrays of >2 dimensions
  
  #^ This looks like a good way to incorporate movement between sites (actually makes more sense since I'm only interested in P across pops anyway)
  
  # newly graduated juveniles from same tree, plus the previous timestep's
  P <- phi_J * alpha_J * J_old + phi_P * (1 - alpha_P) * P_disperse
  
  # A(t+1) &= 0 + \phi_P\alpha_P(1-\mu)P(t) + \phi_AA(t)
  
  A <- phi_P * alpha_P  * P_disperse + phi_A * A_old
    
  
  # 
  # # P(t+1) &= \phi_J \alpha_J J(t) + \phi_P(1-\alpha_P)(1-\mu)P(t) + 0 \\
  # P <- phi_J * alpha_J * J_old + phi_P * (1 - alpha_P) * (1 - mu) * P_old
  # 
  # # A(t+1) &= 0 + \phi_P\alpha_P(1-\mu)P(t) + \phi_AA(t)
  # A <- phi_P * alpha_P * (1 - mu) * P_old + phi_A * A_old
  
  # do dispersal step here, by matrix-multiplying the P vector by a dispersal
  # matrix:
  #   P <- P %*% dispersal_matrix
  
  # recombine state matrix (sites by states)
  expected_state <- abind(J, P, A, along = 2)
  
  
  # do stochastic dynamics bit here, 
  # by perturbing all states according to (a continuous relaxation of) Poisson noise with precomputed latent N(0, 1) noise:
  state <- lognormal_continuous_poisson(expected_state, latent_z)
  
  state
  
}

# set initial states in the expected dimension
initial_state_expected <- matrix(c(0.01, 0.01, 10), # mean 10 adults per site at start (can't have 0s in exponential distribution below, so make other stages >0)
                                 n_sites,
                                 n_states, byrow = TRUE)
dim(initial_state_expected) <- c(n_sites, n_states, 1)

initial_state <- exponential(1 / initial_state_expected) # for each site, draw starting pops of each stage from exp distribution

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
#  mu = mu,
  phi_A = phi_adult,
  latent_z = latent_z_timeseries,
  parameter_is_time_varying = c("alpha_J", # Temp-dependent variables (& latent_z) change each time-step
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

# alternately, we can set the stochastic noise at the median value, to
# approximately recover the deterministic behaviour
inits <- replicate(n_chains,
                   initials(
                     fecundity = random_clamped_normal(0.69,
                                                       0.1,
                                                       min = 1e-3),
                     phi_preadult = random_clamped_normal(0.97,
                                                          0.1,
                                                          min = 1e-3,
                                                          max = 1 - 1e-3),
                     phi_adult = random_clamped_normal(0.97,
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
