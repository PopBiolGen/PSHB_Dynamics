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

George <- c(long = 22.46,
            lat = -33.95)

run_loc <- function(loc) {
  locLong <- loc["long"]
  locLat <- loc["lat"]
  run_year(lat = locLat, long = locLong, make_plot = TRUE)
  }


# Run numerical sims
George_sim <- run_loc(George)
George_sim$growthRate





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
