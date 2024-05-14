## ---------------------------
##
## Script name: treeTempCalibrations.R
##
## Purpose of script: Compare observed temperatures in trees (from sapflow monitors) with
##                      temperatures estimated from nicheMapR soil temperature model
##
## Date Created: 2024-02-09
##
## Email:ben.l.phillips@curtin.edu.au
##
## ---------------------------
##
## Notes:
##   The idea is to use mean weather variables (last decade) at daily time steps to predict 
##    temperature inside trees.  Temperature data come from sapflow probes in trees at King's park
##    
## --------------------------
## load up the packages we will need 
library(dplyr)
library(lubridate)
library(weatherOz)

## ---------------------------

# response variable -- temperature from sapflow probes
# Load some sapflow data
#fList <- list.files("dat/sapflow")
sflow <- read.csv(paste0("dat/sapflow/", fList[1]), skip = 18, header = TRUE) %>%
  select(Date, Time, Max.Td.In...C., Max.Tu.In...C.) %>%
  mutate(Date = dmy(Date), year = year(Date), DOY =yday(Date)) %>%
  #filter(year == 2022) %>%
  group_by(DOY) %>%
  summarise(mean_d = mean(Max.Td.In...C.), mean_u = mean(Max.Tu.In...C.))
  
sflow2 <- read.csv(paste0("dat/sapflow/", fList[ff]), skip = 40, header = TRUE) %>%
  select(Date, Time, Max.Td.In...C., Max.Tu.In...C.) %>%
  mutate(Date = dmy(Date), year = year(Date), DOY =yday(Date)) %>%
  #filter(year == 2022) %>%
  group_by(DOY) %>%
  summarise(mean_d = mean(Max.Td.In...C.), mean_u = mean(Max.Tu.In...C.))

sflow <- rbind(sflow, sflow2)
  

# Load the soil.csv dataset
# downloaded from nicheMapR's soil microclimate hindcaster for 2021-2022
soil <- read.csv("dat/kingsParkSoil.csv")
# Summarise to daily mean temperatures
soil.daily <- soil %>%
  #mutate(year = year(dates)) %>%
  select(-dates, -TIME) %>%
  # filter(year == 2022) %>%
  group_by(DOY) %>%
  summarise(across(everything(), mean))


# Load some weather observations for King's Park
# use weatherOz SILO database
wd <- get_data_drill(
  latitude = -31.961833,
  longitude = 115.833689,
  start_date = "20220101",
  end_date = "20221231",
  values = c(
    "max_temp",
    "min_temp",
    "rain",
    "rh_tmax"
  ),
  api_key = Sys.getenv("SILO_API_KEY")
)

wd <- wd %>% mutate(DOY = yday(dmy(paste(day, month, year, sep = "-")))) %>%
  mutate(meanAirTemp = (air_tmax + air_tmin)/2) %>%
  select(DOY, air_tmax, air_tmin, meanAirTemp, rainfall, rh_tmax)
  

merge_temp <- left_join(left_join(sflow, wd), soil.daily)


# get a few visuals 
# plot(meanAirTemp~DOY, data = merge_temp, col = "blue")
# points(air_tmax~DOY, data = merge_temp, col = "lightblue")
# points(mean_d~DOY, data = merge_temp, col = "green")
# points(D100cm~DOY, data = merge_temp, col = "brown")
# plot(rh_tmax~DOY, data = merge_temp)


