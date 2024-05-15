## Purpose: to build a model predicting observed tree temperatures from mean daily data on
##    soil temperature, maximum air temperature, and relative humidity

source("src/temperatures/get-calibration-data.R")

library(greta)

# name data variables
merge_temp <- na.omit(merge_temp)
treeTemps <- as_data(merge_temp$mean_d)
rh <- as_data(merge_temp$rh_tmax)
air <- as_data(merge_temp$air_tmax)
soil <- as_data(merge_temp$D100cm)

# describe predictor function
# priors
int <- normal(0, 3)
beta <- normal(0, 3)
sd <- lognormal(0, 3)

p <- ilogit(int + beta*rh) # rh predicts p

mean_temp <- p*air + (1-p)*soil # predicted temp is weighted average of air/soil according to p

# note to future self: might want to build this with RE of tree if/when more data are available

distribution(treeTemps) <- normal(mean_temp, sd)

gMod <- model(int, beta, sd)

plot(gMod)

draws <- mcmc(gMod, n_samples = 1000, chains = 4)
bayesplot::mcmc_trace(draws)

# make a plot of prediction bounds against data
draws_plot <- calculate(mean_temp, 
                        nsim = 1000,
                        values = draws)
pred_temp <- apply(draws_plot[[1]], 2, mean)
ci_temp <- apply(draws_plot[[1]], 2, quantile, prob = c(0.025, 0.975))
plot_data <- cbind(DOY = merge_temp$DOY, pred_temp, t(ci_temp))
plot_data <- plot_data[order(plot_data[,"DOY"]),]

plot(treeTemps~DOY, data = merge_temp)
lines(plot_data[,"2.5%"]~plot_data[,"DOY"], col = "lightblue")
lines(plot_data[,"97.5%"]~plot_data[,"DOY"], col = "lightblue")

# output model parameters
tree_temp_model_pars <- summary(draws)$statistics
save(tree_temp_model_pars, file = "out/tree-temp-model-pars.RData")
