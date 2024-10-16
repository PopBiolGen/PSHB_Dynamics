library(nasapower)
library(dplyr)
library(tidyr)
library(nasapower)
library(maps)
library(lubridate)
## Load up the functions
source("src/TPCFunctions.R")
source("src/modelFunctions.R")
# Load data for temperature function (from temperature-prediction-function.R -- spartan can't load Rdata file)
merge_temp <- read.csv('src/temperatures/merge_temp.csv')
mod_fit <- lm(mean_d ~ air_tmax*rh_tmax + ma30*rh_tmax, data = merge_temp)
tree_temp_model_pars <- coef(mod_fit)

# Assign mu parameter
mu_est <- 0

# South Africa coords
# Locations from van Rooyen et al. map (https://www.fabinet.up.ac.za/pdf/PSHB/7-PSHB%20distribution%20map%202021-03-04.pdf)
# Matched against records from iNaturalist

George <- c(long = 22.46,
            lat = -33.95)

Durban_Kloof <- c(long = 30.83,
                  lat = -29.76)

Jo <- c(long = 28.06,
            lat = -26.16)

# Need to fix - when running this function, locLong & locLat aren't saved...
# run_loc <- function(loc) {
#  locLong <- loc["long"]
#  locLat <- loc["lat"]
#  run_year(lat = locLat, long = locLong, make_plot = TRUE)
#  }


#George
locLong <- George["long"]
locLat <- George["lat"]
George_sim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)
George_sim$growthRate

# Durban
locLong <- Durban_Kloof["long"]
locLat <- Durban_Kloof["lat"]
Durban_Kloof_sim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)
Durban_Kloof_sim$growthRate

# Johannesburg
locLong <- Jo["long"]
locLat <- Jo["lat"]
Jo_sim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)
Jo_sim$growthRate


##### Grid plot of cities - daily growth rate ####

city_coords <- data.frame(city = c('George', 'Durban', 'Johannesburg'),
                          lat = c(George["lat"], Durban_Kloof["lat"], Jo["lat"]),
                          lon = c(George["long"], Durban_Kloof["long"], Jo["long"]))

# Max & min growth same as Aus cities:
mingrow <- -0.02
maxgrow <- 0.1

# Pick a location
dev.off()
par(mfrow = c(1, 4))

for(i in 1:nrow(city_coords)){
  
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
  axis(side = 2, at = c(15, 50, 75, 100))
}

# Plot just legend:
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend = c("Juveniles", "Pre-adults", "Adults"), 
       col = c("grey75", "grey50", "grey25"), 
       lty = 1, lwd = 2, cex = 1.6, bty='n')


##############################################################
# OVERSEAS weather data
get_env_os <- function(lat, long){
# Data from 'nasapower'
wd <- get_power(
  community = "ag", # Ag sciences community
  pars = c("T2M", "RH2M"), # Temp & relative humidity
  temporal_api = "hourly", # Take hourly records to find rh_tmax 
  lonlat = c(long, lat),
  dates = c("2013-01-01", "2023-12-31")
)
wd <- wd %>% 
  dplyr::mutate(DOY = yday(dmy(paste(DY, MO, YEAR, sep = "-")))) # Calc day of year

wd_min <- wd %>% 
  group_by(DOY, YEAR) %>% 
  slice(which.min(T2M)) %>% # Min daily temp
  select(YEAR, DOY, T2M) %>% 
  rename_with(~'air_tmin', T2M)

wd_max <- wd %>% 
  group_by(DOY, YEAR) %>% 
  slice(which.max(T2M)) %>% # Max daily temp
  select(YEAR, DOY, T2M, RH2M) %>% # & relative humidity at max temp
  rename_with(~c('air_tmax', 'rh_tmax'), c(T2M, RH2M))

wd <- merge(wd_max, wd_min, by = c('YEAR', 'DOY'))

wd <- wd %>% # Match SILO data format
  mutate(meanDaily = (air_tmax + air_tmin)/2, 
         soil = zoo::rollmean(meanDaily, k = 30, fill = NA, align = "right")) %>%
  select(DOY, air_tmax, rh_tmax, soil) %>%
  group_by(DOY) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

return(wd)
}
  
map.where(database="world", locLong, locLat) == "Australia"  
