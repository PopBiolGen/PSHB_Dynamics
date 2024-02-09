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
##   Currently this is just for a single dataset with a partial year match
##
## --------------------------
## load up the packages we will need 
library(dplyr)
library(lubridate)
## ---------------------------

# Load the soil.csv dataset
# downloaded from nicheMapR's soil microclimate hindcaster
soil <- read.csv("dat/kingsParkSoil.csv")
# Summarise to daily mean temperatures
soil.daily <- soil %>%
  mutate(year = year(dates)) %>%
  filter(year == 2022) %>%
  group_by(DOY) %>%
  select(-dates, -TIME) %>%
  summarise(across(everything(), mean))


# Load the sapflow data
sflow <- read.csv("dat/SFM1JC03.CSV", skip = 18, header = TRUE)
sflow <- sflow %>%
            select(Date, Time, Max.Td.Out...C., Max.Tu.Out...C.) %>%
            mutate(Date = dmy(Date), year = year(Date), DOY =yday(Date)) %>%
            filter(year == 2022) %>%
            group_by(DOY) %>%
            summarise(mean_d = mean(Max.Td.Out...C.), mean_u = mean(Max.Tu.Out...C.))
  
merge_temp <- left_join(soil.daily, sflow) %>%
                filter(!is.na(mean_d))

cor(select(merge_temp, -DOY, -year), use="complete.obs")
 # shows that 30cm deep is the best correlation

plot(mean_d~D30cm, data = merge_temp)
