library(ozmaps)
library(sf)
library(viridis)
library(ggplot2)
library(ggpubr)
library(readr)
library(dplyr)
library(ggnewscale)
sf_oz <- subset(ozmap("country")) # All Australia
#sf_oz <- subset(ozmap("states"))


### Merge separated regions into 1 output file ####
## mu = 0
filenames <- list.files("out/files/mu_0", # FOLDER NAME
                        pattern="*.csv", full.names=TRUE)
# Combine into a single df
outAus <- filenames %>% 
  purrr::map_dfr(readr::read_csv)
outAus <- outAus[-(which(rowSums(outAus)==0 )),] # Remove empty rows (missing values I think, but also means lat & lon = 0)
outAus <- outAus[-(which(is.na(outAus$A_growth))),] # Remove NAs (after filling in missing values with missing_values.R)
write.csv(outAus,
          file = "out/files/mu_0/Aus_mu0.csv", col.names = T, row.names = F )

## mu = 0.2
filenames <- list.files("out/files/mu_0.2", # FOLDER NAME
                        pattern="*.csv", full.names=TRUE)
# Combine into a single df
outAus <- filenames %>% 
  purrr::map_dfr(readr::read_csv)
outAus <- outAus[-(which(rowSums(outAus)==0 )),] # Remove empty rows (missing values I think, but also means lat & lon = 0)
# Rerun model to get missing values:
source("out/files/missing_values.R")
# AFTER filling in missing values:
outAus <- outAus[-(which(is.na(outAus$A_growth))),] # Remove NAs (after filling in missing values with missing_values.R)
write.csv(outAus,
          file = "out/files/mu_0.4/Aus_mu0.4.csv", col.names = T, row.names = F )

## mu = 0.4
mu_est <- 0.4
map.res <- 0.1 
filenames <- list.files("out/files/mu_0.4", # FOLDER NAME
                        pattern="*.csv", full.names=TRUE)
# Combine into a single df
outAus <- filenames %>% 
  purrr::map_dfr(readr::read_csv)
outAus <- outAus[-(which(rowSums(outAus)==0 )),] # Remove empty rows (missing values I think, but also means lat & lon = 0)
# AFTER filling in missing values:
outAus <- outAus[-(which(is.na(outAus$A_growth))),] # Remove NAs (after filling in missing values with missing_values.R)
write.csv(outAus,
          file = "out/files/mu_0.2/Aus_mu0.2.csv", col.names = T, row.names = F )

##### Map from single mu values: ####
mu0 <- read.csv("out/files/mu_0/Aus_mu0.csv")
mu0.2 <- read.csv("out/files/mu_0.2/Aus_mu0.2.csv")
mu0.4 <- read.csv("out/files/mu_0.4/Aus_mu0.4.csv")
cities <- read_csv("src/city_coords.csv")
plot_theme <- theme(panel.background = element_blank(),
                         axis.line = element_blank(), 
                         axis.text = element_blank(), 
                         axis.ticks = element_blank(), 
                         axis.title = element_blank(),
                         plot.title = element_text(size=10))

#### Australia mu = 0 ####
Aus_mu0 <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population growth rate\n(adults)\n",
                     limits=c(-0.026, 
                              0.076),
                     breaks=c(seq(-0.025, 0.075, by=0.025)),
                     labels=c(seq(-0.025, 0.075, by=0.025)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(mu0$lon)-0.1,
                              max(mu0$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0$lat)-0.1,
                              max(mu0$lat)+0.1))+
  geom_point(data=cities, aes(x=lon, y=lat),
             size=1)+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

Aus_mu0
# SAVE

#### mu = 0 Positive growth only ####

Aus_mu0_pos <- ggplot(data = sf_oz) + 
  geom_tile(data = mu0,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult growth rate\n",
                     limits=c(0, 
                              0.076),
                     breaks=c(seq(0, 0.075, by=0.025)),
                     labels=c(seq(0, 0.075, by=0.025)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(outAus$lon)-0.1,
                              max(outAus$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(outAus$lat)-0.1,
                              max(outAus$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())

Aus_mu0_pos

###### Zoom in sw WA ####

swwa <- read.csv('out/files/WA/WA.hi.res_0_sim_1.csv')
QZ <- read_csv("src/QZdf4.csv")

min(swwa$A_growth)
max(swwa$A_growth)

ggplot(data = sf_oz) + 
  geom_sf(fill=NA)+ 
  geom_tile(data = swwa,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult growth rate\n",
                     limits=c(min(swwa$A_growth),
                              0.06),
                     breaks=c(seq(0.03, 0.06, by=0.01)),
                     labels=c(seq(0.03, 0.06, by=0.01)))+
 coord_sf(xlim = c(115,
                   117.5),
          ylim = c(-33.6,
                   -30.5))+
#  scale_x_continuous(limits=c(115,
 #                             117))+ # Fit plot to lat & lon range
 # scale_y_continuous(limits=c(-33.5,
  #                            -30.5))+
  geom_polygon(data = QZ,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
               aes(x=Longitude, y=Latitude),
               fill=NA, col="red", lwd=0.8) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

# QUARANTINE ZONE 
# Quarantine Zone
# https://www.agric.wa.gov.au/borer#:~:text=in%20your%20browser.-,Quarantine%20Area%20(QA),across%2030%20local%20government%20areas.


#  geom_point(aes(x = 115.861258, y = -31.952311),
 #            pch=1, fill=NA, size=4)

# QUARANTINE ZONE 
# Quarantine Zone
# https://www.agric.wa.gov.au/borer#:~:text=in%20your%20browser.-,Quarantine%20Area%20(QA),across%2030%20local%20government%20areas.


###########################################

#### mu = 0.2 ####
mu0.2 <- read.csv("out/files/mu_0.2/Aus_mu0.2.csv")

plot_theme <- theme(panel.background = element_blank(),
                    axis.line = element_blank(), 
                    axis.text = element_blank(), 
                    axis.ticks = element_blank(), 
                    axis.title = element_blank(),
                    plot.title = element_text(size=10))

max(mu0.2$A_growth)
min(mu0.2$A_growth)

Aus_mu0.2 <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0.2,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population growth rate\n(adults)\n",
                     limits=c(-0.03, 
                              0.042),
                     breaks=c(seq(-0.03, 0.04, by=0.01)),
                     labels=c(seq(-0.03, 0.04, by=0.01)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(mu0.2$lon)-0.1,
                              max(mu0.2$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0.2$lat)-0.1,
                              max(mu0.2$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

Aus_mu0.2
# SAVE

#### mu = 0.2 Positive growth only ####

Aus_mu0.2_pos <- ggplot(data = sf_oz) + 
  geom_tile(data = mu0.2,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population growth rate\n(adults)\n",
                     limits=c(0, 
                              0.042),
                     breaks=c(seq(0, 0.04, by=0.01)),
                     labels=c(seq(0, 0.04, by=0.01)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(mu0.2$lon)-0.1,
                              max(mu0.2$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0.2$lat)-0.1,
                              max(mu0.2$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

Aus_mu0.2_pos

#### mu = 0.4 ####
mu0.4 <- read.csv("out/files/mu_0.4/Aus_mu0.4.csv")

plot_theme <- theme(panel.background = element_blank(),
                    axis.line = element_blank(), 
                    axis.text = element_blank(), 
                    axis.ticks = element_blank(), 
                    axis.title = element_blank(),
                    plot.title = element_text(size=10))

max(mu0.4$A_growth)
min(mu0.4$A_growth)

Aus_mu0.4 <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0.4,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population growth rate\n(adults)\n",
                     limits=c(-0.03, 
                              0.02),
                     breaks=c(seq(-0.03, 0.02, by=0.01)),
                     labels=c(seq(-0.03, 0.02, by=0.01)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(mu0.4$lon)-0.1,
                              max(mu0.4$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0.4$lat)-0.1,
                              max(mu0.4$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

Aus_mu0.4
# SAVE

#### mu = 0.4 Positive growth only ####

Aus_mu0.4_pos <- ggplot(data = sf_oz) + 
  geom_tile(data = mu0.4,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population growth rate\n(adults)\n",
                     limits=c(0, 
                              0.02),
                     breaks=c(seq(0, 0.02, by=0.01)),
                     labels=c(seq(0, 0.02, by=0.01)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(mu0.4$lon)-0.1,
                              max(mu0.4$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0.4$lat)-0.1,
                              max(mu0.4$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

Aus_mu0.4_pos

##################
#### Compare mu (same scale) ####
plot_theme_grid <- theme(panel.background = element_blank(),
                         axis.line = element_blank(), 
                         axis.text = element_blank(), 
                         axis.ticks = element_blank(), 
                         axis.title = element_blank(),
                         plot.title = element_text(size=12),
                         legend.position = "none")
# Min and max growth rates across both sims
mingrow <- min(mu0.4$A_growth)
maxgrow <- max(mu0$A_growth)
# Lat & lon limits
minlat <- min(mu0$lat)
maxlat <- max(mu0$lat)
minlon <- min(mu0$lon)
maxlon <- max(mu0$lon)

#### Diff col for neg growth ####

mu_plot <- function(df, title){
ggplot(data = sf_oz) + 
  geom_tile(data = df,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\n growth rate (adults)\n",
                     limits = c(0, maxgrow))+
  new_scale_fill() +
  geom_tile(data=subset(df, A_growth<=0),  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_gradientn(name = "Mean daily population\n growth rate (adults)\n",
                       colours = c("white", "red"),
                       limits=c(mingrow, 0))+
  
  geom_sf(fill=NA)+ 
  ggtitle(title)+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme_grid
}

plot_mu0_pos <- mu_plot(mu0, "A)")
plot_mu0.2_pos <- mu_plot(mu0.2, "B)")
plot_mu0.4_pos <- mu_plot(mu0.4, "C)")

plot_leg <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\n growth rate (adults)\n",
                     limits = c(0, maxgrow))+
  
  new_scale_fill() +
  geom_tile(data=subset(mu0, A_growth<=0),  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_gradientn(name = NULL,
                       colours = c("white", "red"),
                       limits=c(mingrow, 0))+
  theme(legend.title = element_text(size=13),
        legend.text = element_text(size=10))
legend <- as_ggplot(get_plot_component(plot_leg, 
                                       'guide-box-right', return_all = TRUE))
ggarrange(plot_mu0_pos, plot_mu0.2_pos, plot_mu0.4_pos, legend, ncol=4)


#### Same scale ####

plot_mu0 <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\n growth rate (adults)\n",
                     limits = c(mingrow, maxgrow))+
  geom_sf(fill=NA)+ 
  ggtitle("mu = 0")+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme_grid

plot_mu0.2 <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0.2,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\n growth rate (adults)\n",
                     limits = c(mingrow, maxgrow))+
  geom_sf(fill=NA)+ 
  ggtitle("mu = 0.2")+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme_grid

ggarrange(plot_mu0, plot_mu0.2, ncol=2)


######################
#### Map total number of dispersing PA ####

plot_disp <- ggplot(data = sf_oz) + 
  geom_tile(data = mu0.25, 
            aes(x=lon, y=lat, 
                fill=tot_mu)) +
  
  scale_fill_viridis(name = "Total dispersers per year",
                     trans = "log10",
                     limits=c(1, max(mu0.25$tot_mu)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(outAus$lon)-0.1,
                              max(outAus$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(outAus$lat)-0.1,
                              max(outAus$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))
  
plot_disp





##### Extra MAPS to explore results ####
# Plot areas with pop growth >x
ggplot(data = sf_oz) + 
  geom_tile(data= subset(mu0, A_growth>=0.05),  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population growth rate\n(adults)\n",
                     limits=c(-0.026, 
                              0.076),
                     breaks=c(seq(-0.025, 0.075, by=0.025)),
                     labels=c(seq(-0.025, 0.075, by=0.025)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(outAus$lon)-0.1,
                              max(outAus$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(outAus$lat)-0.1,
                              max(outAus$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

# Map positive growth rates only 
plot_mu0_pos <- ggplot(data = sf_oz) + 
  geom_tile(data = subset(mu0, A_growth>0), 
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult growth rate",
                     limits = c(0, 
                                maxgrow))+
  geom_sf(fill=NA)+ 
  ggtitle("mu = 0")+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme
plot_mu0_pos

## Map total number of dispersing PA

plot_mu2_disp <- ggplot(data = sf_oz) + 
  geom_tile(data = mu0.2, 
            aes(x=lon, y=lat, 
                fill=tot_mu)) +
  scale_fill_viridis(name = "Total dispersers per year",
                     trans = "log10",
                     limits=c(1, max(mu0.2$tot_mu)))+
  geom_sf(fill=NA)+ 
  ggtitle("mu = 0.35")+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme

plot_mu2_disp

# Plot final adult population (>1 only)
ggplot(data = sf_oz) + 
  geom_tile(data = subset(mu0.2, n_A_end > 1),  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=n_A_end)) +
  scale_fill_viridis(name = "Mean daily adult growth rate")+
  geom_sf(fill=NA)+ # WA map
  scale_x_continuous(limits=c(min(outAus$lon)-0.1,
                              max(outAus$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(outAus$lat)-0.1,
                              max(outAus$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())

#### Distribution of growth rates: ####
ggplot(outAus, aes(x=A_growth))+
  geom_histogram()


#### DATA BY SEASON ####
# Time series (by season)
# NEED TO UPDATE SIM TO INCLUDE SEASONAL GROWTH RATES...

# Map grid:
map.season <- function(season){
  ggplot(data = sf_oz) + 
    geom_tile(data = outputs_grid,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
              aes(x=lon, y=lat, 
                  fill = outputs_grid[, season])) +
    scale_fill_viridis(name = "Mean daily adult growth rate",
                       limits=c(min(outputs_grid[,7:10]), # Fixed limits same across all seasons
                                max(outputs_grid[,7:10])))+
    geom_sf(fill=NA)+ # WA map
    scale_x_continuous(limits=c(min(lon)-map.res,max(lon)+map.res))+ # Fit plot to lat & lon range
    scale_y_continuous(limits=c(min(lat)-map.res,max(lat)+map.res))+
    theme(panel.background = element_blank(),
          axis.line = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank())
}     
map.summer <- map.season("summer")
map.autumn <- map.season("autumn")
map.winter <- map.season("winter")
map.spring <- map.season("spring")

map_allseas <- ggarrange(map.summer, map.autumn, map.winter, map.spring,
                         nrow=2, byrow=T)

#### Animation: ####
install.packages("reshape2")
library(reshape2)
out_melt <- as.data.frame(outputs_grid[,c("lon","lat",
                                       "summer","autumn","winter","spring")])
out_melt <- melt(out_melt,
                 id = c("lon","lat"),
                 value.name = "growth",
                 variable.name = "season")
# Give numeric value to season
#out_melt$t <- ifelse(out_melt$season=="summer",
 #                    1,
  #                   ifelse(out_melt$season=="autumn",
   #                         2,
    #                        ifelse(out_melt$season=="winter",
     #                              3,
      #                             4)))
install.packages(c("gganimate","gifski")) 
library(gifski)
library(gganimate)

map.anim <- ggplot(data = sf_oz) + 
  geom_tile(data=out_melt, 
            aes(x=lon, y=lat, fill=growth)) + # E.g. Adult growth rate
  geom_sf(fill=NA)+ # WA map
  scale_x_continuous(limits=c(min(out_melt$lon)-map.res,max(out_melt$lon)+map.res))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(out_melt$lat)-map.res,max(out_melt$lat)+map.res))+
  scale_fill_viridis(name = "Mean daily adult growth rate",
                     limits=c(min(out_melt$growth), 
                              max(out_melt$growth)))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())

anim <- map.anim + 
  transition_states(out_melt$season) + # Different frame for each time point
  ggtitle("{closest_state}")

anim <- animate(anim, # Animate frames
                renderer  = gifski_renderer(), 
                duration = 8) # 

anim_save(file = "out/anim_map_perth.gif", anim) # Save
