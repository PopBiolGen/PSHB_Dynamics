## ---------------------------
##
## Script name: get-calibration-data.R
##
## Purpose of script: Gather data to predict observed temperatures in trees (from sapflow monitors) with
##                      spatial data from BOM, and from NicheMapper soil data
##
## Date Created: 2024-02-09
##
## Email:ben.l.phillips@curtin.edu.au
##
## ---------------------------
##
## Notes:
##   The idea is to use mean weather variables (last decade) at daily time steps to predict 
##    temperature inside trees.  Temperature data come s from sapflow probes in trees at King's park
##    
## --------------------------
## load up the packages we will need 
library(dplyr)
library(lubridate)
library(weatherOz)

## ---------------------------

# response variable -- temperature from sapflow probes
# Load some sapflow data
fList <- list.files("dat/sapflow")
sflow <- read.csv(paste0("dat/sapflow/", fList[1]), skip = 18, header = TRUE) %>%
  select(Date, Time, Max.Td.In...C., Max.Tu.In...C.) %>%
  mutate(Date = dmy(Date), year = year(Date), DOY =yday(Date)) %>%
  #filter(year == 2022) %>%
  group_by(DOY) %>%
  summarise(mean_d = mean(Max.Td.In...C.), mean_u = mean(Max.Tu.In...C.))
  
sflow2 <- read.csv(paste0("dat/sapflow/", fList[2]), skip = 40, header = TRUE) %>%
  select(Date, Time, Max.Td.In...C., Max.Tu.In...C.) %>%
  mutate(Date = dmy(Date), year = year(Date), DOY =yday(Date)) %>%
  #filter(year == 2022) %>%
  group_by(DOY) %>%
  summarise(mean_d = mean(Max.Td.In...C.), mean_u = mean(Max.Tu.In...C.))

sflow <- rbind(sflow, sflow2)


# Load some weather observations for King's Park
# use weatherOz SILO database
wd <- get_data_drill(
  latitude = -31.961833,
  longitude = 115.833689,
  start_date = "20130101",
  end_date = "20231231",
  values = c(
    "max_temp",
    "min_temp",
    "rain",
    "rh_tmax"
  ),
  api_key = Sys.getenv("SILO_API_KEY")
)

wd <- wd %>% mutate(DOY = yday(dmy(paste(day, month, year, sep = "-")))) %>%
  mutate(meanDaily = (air_tmax + air_tmin)/2, meanAnnTemp = mean(meanDaily),
         soil = zoo::rollmean(meanDaily, k = 30, fill = NA, align = "right"),
         ma30 = zoo::rollmean(meanDaily, k = 30, fill = NA, align = "right"), 
         ma60 = zoo::rollmean(meanDaily, k = 60, fill = NA, align = "right"), 
         ma90 = zoo::rollmean(meanDaily, k = 90, fill = NA, align = "right"),
         ma120 = zoo::rollmean(meanDaily, k = 120, fill = NA, align = "right")
         ) %>%
  select(DOY, air_tmax, air_tmin, meanDaily, meanAnnTemp, ma30, ma60, ma90, ma120, rainfall, rh_tmax, soil) %>%
  group_by(DOY) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

  
merge_temp <- merge(wd, sflow, by="DOY", all.x = T)
#merge_temp <- left_join(sflow, wd)

# cor(merge_temp, use = "complete.obs") #note correlation of 0.96 with soil 100cm and 30-day moving average


rm(sflow, sflow2, wd, fList)
# get a few visuals 
# plot(meanAirTemp~DOY, data = merge_temp, col = "blue")
# points(air_tmax~DOY, data = merge_temp, col = "lightblue")
# points(mean_d~DOY, data = merge_temp, col = "green")
# points(D100cm~DOY, data = merge_temp, col = "brown")
# plot(rh_tmax~DOY, data = merge_temp)


