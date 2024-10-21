## Purpose: to build a model predicting observed tree temperatures from mean daily data on
##    soil temperature, maximum air temperature, and relative humidity

source("src/temperatures/get-calibration-data.R")
#merge_temp <- read.csv('src/temperatures/merge_temp.csv')
library(greta)


# name data variables

merge_temp <- na.omit(merge_temp) # There's a gap in sflow data DOY 180 - 220
    # If we omit the NAs though, greta model doesn't seem to make predictions for this period.. ? (so there's just a big gap)

treeTemps <- as_data(merge_temp$mean_d) # We DO need to omit NAs for this though
rh <- as_data(merge_temp$rh_tmax)
air <- as_data(merge_temp$air_tmax)
soil <- as_data(merge_temp$soil)

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
lines(plot_data[,"2.5%"]~plot_data[,"DOY"], col = "blue")
lines(plot_data[,"97.5%"]~plot_data[,"DOY"], col = "blue")

#### Comparing predictions from lm (that we have been using) against this greta model
library(ggplot2)
source("src/modelFunctions.R")
mod_fit <- lm(mean_d ~ air_tmax*rh_tmax + ma30*rh_tmax, data = merge_temp)
# Pull SILO data
locDat <- get_env_data(lat = -32.005892, long = 115.896019)
  newDat <- list(air_tmax = locDat$air_tmax,
                 rh_tmax = locDat$rh_tmax,
                 ma30 = locDat$soil)

lm_pred <- data.frame(DOY = c(1:366),
                      lm_pred = predict(mod_fit, newdata = newDat))

ggplot()+
  geom_point(data = merge_temp, aes(x=DOY, y=mean_d), col='brown')+ # real sapflow data
  geom_point(data = merge_temp, aes(x=DOY, y=mean_u), col='orange')+ # real sapflow data
  geom_point(data = lm_pred, aes(x=DOY, y=lm_pred), col="red")+ # linear model
  geom_point(data = plot_data, aes(x=DOY, y=pred_temp), col="blue") # greta model

# output model parameters
tree_temp_model_pars <- summary(draws)$statistics
save(tree_temp_model_pars, file = "out/tree-temp-model-pars.RData")


