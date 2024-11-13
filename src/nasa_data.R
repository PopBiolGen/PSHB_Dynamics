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

model <- "weighted_mean" # if model == 'weighted_mean' use weighted mean model (with greta coeff) to predicts tree temp
# otherwise use 'mod_fit' lm

city_coords <- read_csv("src/city_coords.csv")
Perth <- c(long = city_coords$lon[city_coords$city == "Perth"],
            lat = city_coords$lat[city_coords$city == "Perth"])

# South Africa coords
# Locations from van Rooyen et al. map &
# https://www.fabinet.up.ac.za/pshb
# Matched against records from iNaturalist

George <- c(long = 22.46,
            lat = -33.95)

#Durban <- c(long = 31.05,
 #               lat = -29.77)
            
Durban <- c(long = 31.01,
            lat = -29.85)

Durban_Kloof <- c(long = 30.83,
                  lat = -29.76)

Jo <- c(long = 28.06,
            lat = -26.16)

Capetown <- c(long = 18.47,
              lat = -33.96)

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

# Durban (Kloof)
locLong <- Durban_Kloof["long"]
locLat <- Durban_Kloof["lat"]
Durban_Kloof_sim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)
Durban_Kloof_sim$growthRate

# Durban
locLong <- Durban["long"]
locLat <- Durban["lat"]
Durban_sim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)
Durban_sim$growthRate

# Johannesburg
locLong <- Jo["long"]
locLat <- Jo["lat"]
Jo_sim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)
Jo_sim$growthRate

# Cape Town
locLong <- Capetown["long"]
locLat <- Capetown["lat"]
Capetown_sim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)
Capetown_sim$growthRate


## California coords
# https://ucanr.maps.arcgis.com/apps/Viewer/index.html?appid=3446e311c5bd434eabae98937f085c80


Laguna_Beach <- c(lat = 33.54, # OC
                           long = -117.74)

San_Marino <- c(lat = 34.13, # LA county
                         long = -118.11)

Santa_Paula <- c(lat = 34.33, # Ventura county
                          long = -119.11)


##### Grid plot of cities - daily growth rate ####

city_coords <- data.frame(city = c('Perth', 'George', 'Durban',
                                   'Johannesburg', 'Cape Town',
                                   'Orange County', 'LA County', 'Ventura County'),
                          lat = c(Perth["lat"], George["lat"], Durban["lat"],
                                  Jo["lat"], Capetown["lat"],
                                  Laguna_Beach["lat"], San_Marino["lat"], Santa_Paula["lat"]),
                          lon = c(Perth["long"], George["long"], Durban["long"], 
                                  Jo["long"], Capetown["long"],
                                  Laguna_Beach["long"], San_Marino["long"], Santa_Paula["long"]))

# Max & min growth same as Aus cities:
mingrow <- -0.02
maxgrow <- 0.1

# Pick a location
dev.off()
par(mfrow = c(3, 3))

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
# Look at OVERSEAS weather data
#get_env_os <- function(lat, long){
# Data from 'nasapower'

long <- Durban["long"]
lat <- Durban["lat"]

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

wd2 <- wd %>% # Match SILO data format
  mutate(meanDaily = (air_tmax + air_tmin)/2, 
         soil = zoo::rollmean(meanDaily, k = 30, fill = NA, align = "right")) %>%
  select(DOY, air_tmax, rh_tmax, soil) %>%
  group_by(DOY) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

durban.temp <- ggplot()+
  geom_line(data = wd, 
            aes(x=DOY, y=air_tmax, group=YEAR), 
            col="pink")+
  geom_line(data = wd, 
            aes(x=DOY, y=air_tmin, group=YEAR), 
            col="skyblue")+
  geom_line(data = wd2, 
            aes(x=DOY, y=air_tmax), 
            col="red")+
  geom_line(data = wd2, 
            aes(x=DOY, y=soil), 
            col="brown")+
  scale_y_continuous(limits=c(0,35))+
  ggtitle("Durban temp")

durban.rh <- ggplot()+
  geom_line(data = wd, 
            aes(x=DOY, y=rh_tmax, group=YEAR),
            col="skyblue")+
  geom_line(data = wd2, 
            aes(x=DOY, y=rh_tmax),
            col="blue")+
  scale_y_continuous(limits=c(0,100))+
  ggtitle('Durban humidity')


durban.all <- ggplot()+
  geom_line(data = wd, 
            aes(x=DOY, y=rh_tmax, group=YEAR),
            col="skyblue")+
  geom_line(data = wd2, 
            aes(x=DOY, y=rh_tmax),
            col="blue")+
  scale_y_continuous(limits=c(0,100))+
  geom_line(data = wd, 
            aes(x=DOY, y=air_tmax, group=YEAR), 
            col="pink")+
  geom_line(data = wd, 
            aes(x=DOY, y=air_tmin, group=YEAR), 
            col="lightblue")+
  geom_line(data = wd2, 
            aes(x=DOY, y=air_tmax), 
            col="red")+
  geom_line(data = wd2, 
            aes(x=DOY, y=soil), 
            col="brown")+
  ggtitle('Durban')

library(egg)
ggarrange(kloof.all, kloof.temp, kloof.rh,
          durban.all, durban.temp, durban.rh,
          nrow=2, ncol=3)

#return(wd)
#}
  
map.where(database="world", locLong, locLat) == "Australia"  
