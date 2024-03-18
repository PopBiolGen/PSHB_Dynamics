# demo of stochastic dispersal and growth dynamics


# simulate a discrete stochastic growth rate population model
set.seed(1)
n_times <- 20
n_pops <- 4

# daily population growth rate in each location
r_true <- runif(n_pops, 1, 1.3)

# initial populations (just before timeseries)
pop_init_true <- rep(0, n_pops)
pop_init_true[1] <- round(n_pops * 4)

# population locations and dispersal matrix
coords <- matrix(runif(n_pops * 2), nrow = n_pops)
dispersal_range <- 6
dispersal_weight_raw <- exp(-dispersal_range * as.matrix(dist(coords)))
# add a nugget effect (increased probability of not dispersing)
prob_dispersing <- 0.1
dispersal_weight <- dispersal_weight_raw * prob_dispersing +
  diag(n_pops) * (1 - prob_dispersing)

# plot these
par(mfrow = c(1, 1))
plot(coords,
     type = "n",
     ylab = "",
     xlab = "",
     axes = FALSE)
for (i in 1:n_pops) {
  for (j in i:n_pops) {
    if (i != j) {
      arrows(x0 = coords[i, 1],
             y0 = coords[i, 2],
             x1 = coords[j, 1],
             y1 = coords[j, 2],
             length = 0,
             lwd = 10 * dispersal_weight[i, j])
    }
  }
}
points(coords,
       pch = 21,
       bg = grey(0.8),
       cex = 2)
text(coords[, 1],
     coords[, 2],
     labels = paste("pop", seq_len(n_pops)),
     pos = 3,
     xpd = NA)

# normalise dispersal weights to get dispersal probabilities
dispersal_prob <- sweep(dispersal_weight,
                        1,
                        rowSums(dispersal_weight),
                        FUN = "/")

# simulate stochastic population dynamics and dispersal
time <- seq_len(n_times)
pop_true <- matrix(NA, nrow = n_times, ncol = n_pops)
pop_previous <- pop_init_true
for (i in 1:n_times) {
  # innovate populations
  pop_grown <- rpois(n_pops, pop_previous * r_true)
  # do dispersal, with multinomial randomness
  pop_dispersed <- matrix(NA, n_pops, n_pops)
  for (pop in 1:n_pops) {
    pop_dispersed[pop, ] <- rmultinom(1,
                                   pop_grown[pop],
                                   prob = dispersal_prob[pop, ])
  }
  # collate all the individuals staying, arriving, less those leaving
  pop_new <- colSums(pop_dispersed)
  # store the states
  pop_previous <- pop_true[i, ] <- pop_new

}

# add an observation process (binomial with fixed detection probability)
obs_prob <- 0.8
pop_obs <- pop_true * NA
pop_obs[] <- rbinom(length(pop_true),
                    size = pop_true[],
                    prob = obs_prob)

# plot true (lines) and observed (points) populations across these populations
par(mfrow = n2mfrow(min(n_pops, 20)),
    mar = c(1, 3, 3, 1))
for (i in seq_len(n_pops)) {
  plot(pop_true[, i] ~ time,
       type = "l",
       ylab = "",
       xlab = "",
       ylim = range(pop_true),
       main = paste("pop", i))

  points(pop_obs[, i] ~ time,
       type = "b",
       pch = 21,
       bg = ifelse(pop_true[, i] > 0, grey(0.4, 0.5), NA))
}

# build a greta model to infer latent populations, using stochastic transitions
# but a continuous relaxation of the discrete process
source("src/modelFunctions.R")
library(greta.dynamics)

# define functions for continuous relaxation and reparameterisation of the
# Poisson distributions

# given random variables z (with standard normal distribution a priori), and
# Poisson rate parameter lambda, return a strictly positive continuous random
# variable with the same mean and variance as a poisson random variable with
# rate lambda, by approximating the poisson as a lognormal distribution.
lognormal_continuous_poisson <- function(lambda, z) {
  sigma <- sqrt(log1p(lambda / exp(2 * log(lambda))))
  # sigma2 <- log1p(1 / lambda)
  mu <- log(lambda) - sigma^2 / 2
  exp(mu + z * sigma)
}

# dispersal parameters to be fixed for now, just learn the growth rates and the
# initial populations and transitions
r <- normal(1, 0.25,
            truncation = c(0, Inf),
            dim = n_pops)

# latent variable for stochastic transitions in lognormal approximation to
# Poisson
latent_z_vec <- normal(0, 1, dim = c(n_times, n_pops))

# initial population size - with mean equal to truth, but small, instead of
# zero, elements
pop_init_prior <- pmax(pop_init_true, 1e-3)
init <- exponential(1 / pop_init_prior)

# transition function for the population process as a difference equation in
# integer timestep
fun <- function(state, iter, r, latent_z) {
  # grow the populations
  expected_pop_grown <- state * r
  # disperse the populations
  expected_pop <- t(dispersal_prob) %*% expected_pop_grown
  # add stochasticity
  state_new <- lognormal_continuous_poisson(expected_pop, latent_z)
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
  state_limits = c(1e-3, (max(pop_obs) / obs_prob) * 2)
)

# get the modelled true population
pop_modelled <- t(pop_sim$all_states)
# and the expected value of the observation distribution
pop_obs_expected <- pop_modelled * obs_prob
pop_obs_ga <- as_data(pop_obs)
distribution(pop_obs_ga) <- poisson(pop_obs_expected)

# set the initial values for the population trajectories to be near the
# (deterministic) expected values, so sampling the stochastic values doesn't
# initialise us in very weird parts of parameter space
n_chains <- 10
inits <- replicate(n_chains,
                   initials(
                     init = pop_init_prior,
                     latent_z_vec = matrix(rnorm(n_times * n_pops, 0, 0.1),
                                           n_times,
                                           n_pops)),
                   simplify = FALSE)

m <- model(r, latent_z_vec, init)
run_time <- system.time(
  draws <- mcmc(m,
                chains = n_chains,
                initial_values = inits)
)

# check convergence
summary(coda::gelman.diag(draws,
                          autoburnin = FALSE,
                          multivariate = FALSE)$psrf)

# check growth rate estimates we would not expect the posterior to be
# data-informed at all for populations 2 and 3 where the population doesn't have
# a chance to grow. For 1 and 4 (established populations form near the start, no
# stochastic extinction)) we would expect to to estimate a positive growth rate
# somewhere in the correct ball park
summary(calculate(r, values = draws))$statistics
r_true

# posterior simulations
sims_posterior <- calculate(pop_modelled,
                        values = draws,
                        nsim = 100)

par(mfrow = n2mfrow(min(n_pops, 20)),
    mar = c(1, 1, 3, 1))
for (pop in 1:n_pops) {
  trajectories_posterior <- round(sims_posterior[[1]][, , pop])
  plot(trajectories_posterior[1, ] ~ time,
       type = "n",
       ylim = range(c(trajectories_posterior)),
       ylab = "",
       xlab = "",
       main = paste("pop", pop))
  apply(jitter(trajectories_posterior),
        1,
        lines,
        col = grey(0.4, 0.2),
        lwd = 2)
  lines(pop_true[, pop] ~ time,
        lwd = 2,
        col = "blue")
  points(pop_obs[, pop] / obs_prob ~ time,
         bg = ifelse(pop_true[, pop] > 0, "skyblue", "white"),
         cex = 1,
         pch = 21)
}

run_time["elapsed"]


# pair plot of latent z
post_sims <- calculate(latent_z_vec,
                       nsim = coda::niter(draws) * n_chains,
                       values = draws)[[1]]

pairs(post_sims[, 8:10, 3],
      cex = 0.1)

par(mfrow = c(1, 1),
    mar = c(5, 4, 4, 2) + 0.1)
hist(post_sims[, 9, 3],
     cex = 0.1,
     breaks = 100)
