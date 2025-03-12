########## Run model for known PSHB locations with different mu values ######################

library(dplyr)
library(lubridate)
library(readr)
library(ozmaps)
library(sf)

## Load up the functions
source("src/TPCFunctions.R")
source("src/modelFunctions.R")
# Need these for model
sf_oz <- subset(ozmap("country"))
model <- "weighted_mean" # if model == 'weighted_mean' use weighted mean model (with greta coeff) to predicts tree temp
mu_disp_est <- 0 # estimated mu parameter (proportion P dispersing)
phi_mu_est <- 1 # estimated phi_mu (proportion survival during dispersal)

# Dataset of sample locations
coords <- read_csv("src/known_PSHB_coords.csv")

mu_est_i <- c(seq(0, 0.95, 0.05)) # mu values
nmu <- length(mu_est_i)*nrow(coords)

explore_mu <- data.frame(city = rep(coords$city, times=nmu),
                         country = rep(coords$country, times=nmu),
                         lat = rep(coords$lat, times=nmu),
                         lon = rep(coords$lon, times=nmu),
                         mu_est_i = rep(mu_est_i, each=nrow(coords)))

# Run model for each location & mu value
for(i in 1:nrow(explore_mu)){
  
  locLat <- explore_mu$lat[i]
  locLong <- explore_mu$lon[i]
  country <- explore_mu$country[i]
  mu_est <- explore_mu$mu_est_i[i]
  
  yearSim_city <- run_year(lat = locLat, long = locLong, 
                           make_plot = FALSE)
  
  explore_mu$A_growth_i[i] <- yearSim_city$growthRate[3]
  }

#### SAVE DATA ####  
write.csv(explore_mu, 'out/explore_mu.csv')

### PLOT ####

library(RColorBrewer)
library(ggplot2)

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

