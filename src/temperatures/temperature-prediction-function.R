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


distribution(treeTemps) <- normal(mean_temp, sd)

gMod <- model(int, beta, sd)

plot(gMod)

draws <- mcmc(gMod, n_samples = 1000, chains = 4)
bayesplot::mcmc_trace(draws)

# make a plot of prediction bounds against data
gMod_plot <- model(mean_temp)
draws_plot <- mcmc(gMod_plot, n_samples = 1000, chains = 4)
plot_summ <- summary(draws_plot)$quantiles
plot(treeTemps~DOY, data = merge_temp)
points(plot_summ[,"2.5%"]~merge_temp$DOY, pch = 16, col = "lightblue")
points(plot_summ[,"97.5%"]~merge_temp$DOY, pch = 21, col = "lightblue")
