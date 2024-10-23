

################################

library(dplyr)
library(lubridate)
library(readr)

## Load up the functions
source("src/TPCFunctions.R")
source("src/modelFunctions.R")

# Load data for temperature function (from temperature-prediction-function.R -- spartan can't load Rdata file)
merge_temp <- read.csv('src/temperatures/merge_temp.csv')
mod_fit <- lm(mean_d ~ air_tmax*rh_tmax + ma30*rh_tmax, data = merge_temp)
tree_temp_model_pars <- coef(mod_fit)

model <- "weighted_mean" # if model == 'weighted_mean' use weighted mean model (with greta coeff) to predicts tree temp
# otherwise use 'mod_fit' lm

# Assign mu parameter
mu_est <- 0

city_coords <- read_csv("src/city_coords.csv")

# Looking at plots without manual-scaled axes:
mingrow <- -0.04
maxgrow <- 0.1

# Pick a location
dev.off()
par(mfrow = c(3, 4))
              
for(i in 1:nrow(city_coords)){
#  for(i in 5:10){
  locLat <- city_coords$lat[i]
  locLong <- city_coords$lon[i]
  city_name <- city_coords$city[i]
  
  yearSim_city <- run_year(lat = locLat, long = locLong, 
                      make_plot = FALSE)
  
  population_data <- yearSim_city$popDat
  
  # By stage
  juv_vec <- population_data[1,]
  pre_adult_vec <- population_data[2,]
  ad_vec <- population_data[3,]
  # Calculate growth rates for all life stages
  juv_growth_rate <- diff(log(juv_vec))
  pre_adult_growth_rate <- diff(log(pre_adult_vec))
  ad_growth_rate <- diff(log(ad_vec))
  # Smooth the growth rate curves using a moving average
  window_size <- 5
  juv_growth_rate_smoothed <- stats::filter(juv_growth_rate, rep(1/window_size, window_size), sides = 2)
  pre_adult_growth_rate_smoothed <- stats::filter(pre_adult_growth_rate, rep(1/window_size, window_size), sides = 2)
  ad_growth_rate_smoothed <- stats::filter(ad_growth_rate, rep(1/window_size, window_size), sides = 2)
  
  plot(juv_growth_rate_smoothed, type = "l", 
       main = city_name, 
       xlab = "Day of year", 
       ylab = "Growth rate", 
       col = "grey75", 
       lwd = 1.8, 
       ylim = c(mingrow, maxgrow),
       bty = "l")
  lines(pre_adult_growth_rate_smoothed, col = "grey50", lwd = 1.8)
  lines(ad_growth_rate_smoothed, col = "grey25", lwd = 1.8)
#  legend("bottomright", legend = c("Juveniles", "Pre-adults", "Adults"), 
 #        col = c("grey75", "grey50", "grey25"), 
  #       lty = 1, lwd = 2, cex = 0.75)
  abline(h=0, lty=2)
  axis(side = 1, at = c(seq(0, 300, by=100))) 
  axis(side = 2, at = c(seq(-0.04, 0.1, by=0.02)))
}

# Plot just legend:
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("center", legend = c("Juveniles", "Pre-adults", "Adults"), 
        col = c("grey75", "grey50", "grey25"), 
       lty = 1, lwd = 2, cex = 1.1, bty='n')

dev.off()


##################################
