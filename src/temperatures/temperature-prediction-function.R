## Purpose: to build a model predicting observed tree temperatures from mean daily data on
##    soil temperature, maximum air temperature, and relative humidity

source("src/temperatures/get-calibration-data.R")

# fit model
merge_temp <- na.omit(merge_temp)
mod_fit <- lm(mean_d ~ air_tmax*rh_tmax + ma30*rh_tmax, data = merge_temp)

# summary(mod_fit)



# make a plot of prediction against data
# plot(mean_d~DOY, data = merge_temp)
preds <- predict(mod_fit)
# points(preds ~ merge_temp$DOY, col = "red")
# points(merge_temp$meanDaily ~ merge_temp$DOY, col = "orange")
# points(lm_pred ~ DOY, col = "blue", data=lm_pred)

# plot(merge_temp$mean_d~preds)
# abline(0, 1)


# output model parameters
tree_temp_model_pars <- coef(mod_fit)
# save(mod_fit, tree_temp_model_pars, file = "out/tree-temp-model-pars.RData")
