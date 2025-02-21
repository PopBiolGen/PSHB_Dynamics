################################

library(dplyr)
library(lubridate)
library(readr)
library(ozmaps)
library(sf)

## Load up the functions
source("src/TPCFunctions.R")
source("src/modelFunctions.R")

# Load data for temperature function (from temperature-prediction-function.R -- spartan can't load Rdata file)
merge_temp <- read.csv('src/temperatures/merge_temp.csv')
mod_fit <- lm(mean_d ~ air_tmax*rh_tmax + ma30*rh_tmax, data = merge_temp)
tree_temp_model_pars <- coef(mod_fit)

sf_oz <- subset(ozmap("country"))

model <- "weighted_mean" # if model == 'weighted_mean' use weighted mean model (with greta coeff) to predicts tree temp
# otherwise use 'mod_fit' lm

# Assign mu parameter
mu_disp_est <- 0 # estimated mu parameter (proportion P dispersing)
phi_mu_est <- 1 # estimated phi_mu (proportion survival during dispersal)
# mu_est <- mu_disp_est * (1 - phi_mu_est) # new 'mu' estimate is the proportion of P lost through dispersal mortality (assuming net incoming vs outgoing P = 0)
mu_est <- 0

# Aus cities
city_coords <- read_csv("src/city_coords.csv")
# South Africa cities
George <- c(long = 22.46,
            lat = -33.95)
Durban <- c(long = 31.01,
            lat = -29.85)
Jo <- c(long = 28.06,
        lat = -26.16)


mu_est_i <- c(seq(0, 0.95, 0.05))

Perth <- data.frame(mu_est_i = mu_est_i,
                    lat = city_coords$lat[city_coords$city == "Perth"],
                    lon = city_coords$lon[city_coords$city == "Perth"],
                    city = "Perth")

George <- data.frame(mu_est_i = mu_est_i,
                     lat = -33.95,
                     lon = 22.46,
                     city = "George")

Durban <- data.frame(mu_est_i = mu_est_i,
                        lat = -29.85,
                        lon = 31.01,
                        city = "Durban")
                    
Johannesburg <- data.frame(mu_est_i = mu_est_i,
                     lat = -26.16,
                     lon = 28.06,
                     city = "Johannesburg")

# California
# https://ucanr.maps.arcgis.com/apps/Viewer/index.html?appid=3446e311c5bd434eabae98937f085c80

Laguna_Beach <- data.frame(mu_est_i = mu_est_i, # Orange County
                           lat = 33.54,
                           lon = -117.74,
                           city = "Laguna Beach")

San_Marino <- data.frame(mu_est_i = mu_est_i, # LA County
                           lat = 34.13,
                           lon = -118.11,
                           city = "San Marino")

Santa_Paula <- data.frame(mu_est_i = mu_est_i, # Ventura County
                         lat = 34.33,
                         lon = -119.11,
                         city = "Santa Paula")





explore_mu <- rbind(Perth, 
                    George, Durban, Johannesburg,
                    Laguna_Beach, San_Marino, Santa_Paula)
explore_mu$A_growth_i <- 0


  for(i in 1:nrow(explore_mu)){
  
    mu_est <- explore_mu$mu_est_i[i]
  
  locLat <- explore_mu$lat[i]
  
  locLong <- explore_mu$lon[i]
  
  yearSim_city <- run_year(lat = locLat, long = locLong, 
                           make_plot = FALSE)
  
  explore_mu$A_growth_i[i] <- yearSim_city$growthRate[3]
  }

  
write.csv(explore_mu, 'out/explore_mu.csv')

#### SAVE DATA ####
library(RColorBrewer)

explore_mu <- read.csv('out/explore_mu.csv')

explore_mu$location <- factor(explore_mu$location, 
       levels=c("Perth","Johannesburg","Durban","George",
                "Laguna Beach","San Marino","Santa Paula"))

cols <- RColorBrewer::brewer.pal(9, "Set1")[c(1,2,3,4)]
cols <- c(cols[1],cols[2],cols[2],cols[2],cols[3],cols[3],cols[3])

ggplot(explore_mu, aes(x=mu_est_i, y=A_growth_i, col=location,
                       group=interaction(country, location),
                       lty=location))+
  geom_hline(yintercept = 0, lty=2)+
  #  geom_point(size=2)+
  geom_line(lwd=1)+
  scale_color_manual(values=cols,
                     name="Location")+
  scale_linetype_manual(values=c(1,1,2,4,1,2,4),
                        name="Location")+
  scale_x_continuous(breaks=seq(0, 0.95, 0.1),
                     name = expression('Proportion mortality from dispersal ('*mu*')'))+
  scale_y_continuous(name = "Mean daily adult population growth rate")+
  theme(axis.text.x = element_text(size=14), #text(angle = 45, size = 28, vjust=1, hjust=1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size=16, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(size=16, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        #panel.background = element_rect(fill = "white"),
        legend.key= element_rect(fill = NA),
        legend.key.width = unit(2, "line"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
      #  axis.line = element_line(colour = "gray50", size = 0.6, linetype = "solid"))
        panel.border = element_rect(colour = "black", fill=NA,size=1.5))

