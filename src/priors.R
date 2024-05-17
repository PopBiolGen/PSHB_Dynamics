# A script for building priors for the PSHB model
  # Details are provided in modelDescriptions.Rmd "## Estimating priors"
  # This script follows those details but then places the priors on nice supports
  # Outputs a list specifying full priors for each parameter

source("src/TPCFunctions.R")

# function returning beta parameters given mean and variance
# from https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(shape1 = alpha, shape2 = beta))
}

# function returning gamma parameters given mean and variance
estGammaParams <- function(mu, var){
  rate <- mu/var
  shape <- mu^2/var
  return(params = list(shape = shape, rate = rate))
}

########### alpha_J(T) ############

# Data from two relevant papers, umeda and walgama
alpha_j_temp_umeda <- c(18, 20, 25, 30, 32)
alpha_j_rate_umeda <- c(0.013, 0.020, 0.0258, 0.0421, 0.0206)
alpha_j_temp_walgama <- c(15, 18, 20, 22, 25, 28, 30, 32)
egg_dev_time_walgama <- c(Inf, 23.5, 18.5, 13, 7.3, 5.5, 5, 10.9)
larva_dev_time_walgama <- c(NA, 1/0.0288, 1/0.0399, NA, 1/0.0688, 1/0.140, NA, NA) # extracted from figure
pupae_dev_time_walgama <- c(Inf, 15, 14, 9, 7.5, 6, 4, 7)
total_juv_dev_time_walgama <- egg_dev_time_walgama + larva_dev_time_walgama + pupae_dev_time_walgama
alpha_j_rate_walgama <- 1/total_juv_dev_time_walgama
alpha_j_rate_walgama[1] <- 0

# Estimate TPC
temp <- c(alpha_j_temp_umeda, alpha_j_temp_walgama)
rate <- c(alpha_j_rate_umeda, alpha_j_rate_walgama)
TPCmatrix <- na.omit(cbind(temp, rate))
alpha_J_fit <- TPC.q.fit(TPCmatrix, in.acc = 8, hessian = TRUE)

par_names_TPC <- c("Pmax", "T_o", "a_plus", "a_minus")

alpha_J_pars <- alpha_J_fit$par
alpha_J_pars_sd <- sqrt(diag(solve(alpha_J_fit$hessian)))
alpha_J_prior_ests <- cbind(alpha_J_pars, alpha_J_pars_sd)
rownames(alpha_J_prior_ests) <- par_names_TPC

# Supports
  # lists to take priors
  alpha_J_priors <- vector(mode = "list", length = 4)
  names(alpha_J_priors) <- par_names_TPC
  phi_J_priors <- alpha_P_priors <- alpha_J_priors
  
  # Pmax within (0,1)
  alpha_J_priors[["Pmax"]] <- c(
    Distribution = "beta",
    estBetaParams(alpha_J_prior_ests["Pmax", 1], alpha_J_prior_ests["Pmax", 2]^2)
  )
  # T_o within (-Inf,Inf)
  alpha_J_priors[["T_o"]] <- list(
    Distribution = "normal",
    mean = alpha_J_prior_ests["T_o", 1], 
    sd = alpha_J_prior_ests["T_o", 2]
  )
  # a_plus within (0,Inf)
  alpha_J_priors[["a_plus"]] <- c(
    Distribution = "gamma",
    estGammaParams(alpha_J_prior_ests["a_plus", 1], alpha_J_prior_ests["a_plus", 2]^2)
  )
  # a_minus within (0,1)
  alpha_J_priors[["a_minus"]] <- c(
    Distribution = "beta",
    estBetaParams(alpha_J_prior_ests["a_minus", 1], alpha_J_prior_ests["a_minus", 2]^2)
  )


########### alpha_P(T) ############
  
alpha_P_prior_ests <- alpha_J_prior_ests
colnames(alpha_P_prior_ests) <- c("pars", "alpha_P_pars_sd")
alpha_P_prior_ests["Pmax", 1] <- 1/8
alpha_P_prior_ests["Pmax", 2] <- sqrt(1/8/alpha_J_prior_ests["Pmax", 1]*(alpha_J_prior_ests["Pmax", 2]^2))

# Supports
  # Pmax within (0,1)
  alpha_P_priors[["Pmax"]] <- c(
    Distribution = "beta",
    estBetaParams(alpha_P_prior_ests["Pmax", 1], alpha_P_prior_ests["Pmax", 2]^2)
  )
  # the rest use alpha_J parameters
  # T_o within (-Inf,Inf)
  alpha_P_priors[["T_o"]] <- list(
    Distribution = "normal",
    mean = alpha_J_prior_ests["T_o", 1], 
    sd = alpha_J_prior_ests["T_o", 2]
  )
  # a_plus within (0,Inf)
  alpha_P_priors[["a_plus"]] <- c(
    Distribution = "gamma",
    estGammaParams(alpha_J_prior_ests["a_plus", 1], alpha_J_prior_ests["a_plus", 2]^2)
  )
  # a_minus within (0,1)
  alpha_P_priors[["a_minus"]] <- c(
    Distribution = "beta",
    estBetaParams(alpha_J_prior_ests["a_minus", 1], alpha_J_prior_ests["a_minus", 2]^2)
  )


########### phi_J(T) ############
  
sDat <- read.csv(file = "dat/walgamaFig2DataExtract.csv")
sDat$Mortality <- sDat$Mortality/100 # convert to probability of mortality over n days
sDat$Mortality[sDat$Mortality>1] <- 1 # catch the 1.002s
mortRate <- -log(1-sDat$Mortality)/sDat$Days # convert to a rate
mortProb <- 1-exp(-mortRate) # convert to a daily probability
survProb <- 1-mortProb
sDat <- cbind(sDat, mortProb, survProb)

# Estimate TPC
TPCmatrix <- cbind(sDat$Temperature, survProb)
TPCmatrix <- na.omit(TPCmatrix)
phi_J_fit <- TPC.q.fit(TPCmatrix, in.acc = 0.3, hessian = TRUE)
phi_J_pars <- phi_J_fit$par
phi_J_pars_sd <- sqrt(diag(solve(phi_J_fit$hessian)))
phi_J_prior_ests <- cbind(phi_J_pars, phi_J_pars_sd)
rownames(phi_J_prior_ests) <- par_names_TPC

# Supports
  # Pmax within (0,1)
  phi_J_priors[["Pmax"]] <- c(
    Distribution = "beta",
    estBetaParams(phi_J_prior_ests["Pmax", 1], phi_J_prior_ests["Pmax", 2]^2)
  )
  # T_o within (-Inf,Inf)
  phi_J_priors[["T_o"]] <- list(
    Distribution = "normal",
    mean = phi_J_prior_ests["T_o", 1], 
    sd = phi_J_prior_ests["T_o", 2]
  )
  # a_plus within (0,Inf)
  phi_J_priors[["a_plus"]] <- c(
    Distribution = "gamma",
    estGammaParams(phi_J_prior_ests["a_plus", 1], phi_J_prior_ests["a_plus", 2]^2)
  )
  # a_minus within (0,1)
  phi_J_priors[["a_minus"]] <- c(
    Distribution = "beta",
    estBetaParams(phi_J_prior_ests["a_minus", 1], phi_J_prior_ests["a_minus", 2]^2)
  )

########### phi_P, phi_A ############
phi_P_priors <- vector(mode = "list", length = 1)
  
mean_phi <- exp(-1/32)
var_phi <- 0.03^2

phi_P_priors <- list(
  phi_P = c(
    Distribution = "beta",
    estBetaParams(mean_phi, var_phi)
  )
)

# phi_A = phi_P

########### phi_mu ############
phi_mu_priors <- vector(mode = "list", length = 1)

phi_mu_priors <- list(
  phi_mu = list(
    Distribution = "beta",
    shape1 = 1,
    shape2 = 1
  )
)

########### Organise and cleanup ############

PSHB_priors <- list(alpha_J = alpha_J_priors, 
                    alpha_P = alpha_P_priors, 
                    phi_J = phi_J_priors, 
                    phi_P = phi_P_priors,
                    phi_mu = phi_mu_priors)

stuff <- ls()
stuff <- c(stuff[!grepl("PSHB", stuff)], "stuff")
rm(list=stuff)
