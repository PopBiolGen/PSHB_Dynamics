library(ozmaps)
library(sf)
library(viridis)
library(ggplot2)
library(ggpubr)
library(readr)
library(dplyr)
sf_oz <- subset(ozmap("country")) # All Australia
#sf_oz <- subset(ozmap("states"))

#### mu = 0 ####
### Merge separated regions into 1 output file ####
filenames <- list.files("out/files/mu_0", # FOLDER NAME
                        pattern="*.csv", full.names=TRUE)
# Combine into a single df
outAus <- filenames %>% 
  purrr::map_dfr(readr::read_csv)
outAus <- outAus[-(which(rowSums(outAus)==0 )),] # Remove empty rows (missing values I think, but also means lat & lon = 0)
outAus <- outAus[-(which(is.na(outAus$A_growth))),] # Remove NAs (after filling in missing values with missing_values.R)
write.csv(outAus,
          file = "out/files/mu_0/Aus_mu0.csv", col.names = T, row.names = F )

##### Map from single mu values: ####
mu0 <- read.csv("out/files/mu_0/Aus_mu0.csv")

plot_theme <- theme(panel.background = element_blank(),
                         axis.line = element_blank(), 
                         axis.text = element_blank(), 
                         axis.ticks = element_blank(), 
                         axis.title = element_blank(),
                         plot.title = element_text(size=10))

Aus_mu0 <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
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

Aus_mu0
# SAVE

#### Positive growth only ####

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

###########
## Zoom in sw WA ##
swwa <- subset(mu0,
               lon < 120 & lat < -28.5)

sw.plot <- ggplot(data = sf_oz) + 
  geom_tile(data = swwa,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult growth rate\n",
                     limits=c(0, 0.06),
                     breaks=c(seq(0, 0.06, by=0.02)),
                     labels=c(seq(0, 0.06, by=0.02)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(swwa$lon)-0.05,
                              max(swwa$lon)+0.05))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(swwa$lat)-0.05,
                              max(swwa$lat)+0.05))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())
sw.plot

QZ <- read_csv("src/QZdf4.csv")

sw.plot+
  geom_polygon(data = QZ,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
               aes(x=Longitude, y=Latitude),
               fill=NA, col="red")

# QUARANTINE ZONE 
# Quarantine Zone
# https://www.agric.wa.gov.au/borer#:~:text=in%20your%20browser.-,Quarantine%20Area%20(QA),across%2030%20local%20government%20areas.


#  geom_point(aes(x = 115.861258, y = -31.952311),
 #            pch=1, fill=NA, size=4)

# QUARANTINE ZONE 
# Quarantine Zone
# https://www.agric.wa.gov.au/borer#:~:text=in%20your%20browser.-,Quarantine%20Area%20(QA),across%2030%20local%20government%20areas.

# Rough corner points from map:
QZ <- as.data.frame(matrix(c(-31.6, 115.7,
                             -31.7, 116.3,
                             -32.4, 116.1,
                             -32.4, 115.7), 
                           nrow=4, ncol=2,
                           byrow = T))
colnames(QZ) <- c("lat", "lon")
QZlatmin <- min(QZ$lat)
QZlatmax <- max(QZ$lat)
QZlonmin <- min(QZ$lon)
QZlonmax <- max(QZ$lon)
QZout <- subset(swwa, 
                lat < QZlatmax & 
                  lat > QZlatmin &
                  lon < QZlonmax &
                  lon > QZlonmin
)
sw.plot +
 # geom_tile(data = QZout,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
  #          aes(x=lon, y=lat), alpha=0.4, fill="red")+
  geom_rect(xmin=115.41, xmax=116.35,
            ymin=-32.46, ymax=-31.65,
            col="red", alpha=0)

###########################################

#### mu = 0.25 ####
### Merge separated regions into 1 output file ####
filenames <- list.files("out/files/mu_0.25", # FOLDER NAME
                        pattern="*.csv", full.names=TRUE)
# Combine into a single df
outAus <- filenames %>% 
  purrr::map_dfr(readr::read_csv)
outAus <- outAus[-(which(rowSums(outAus)==0 )),] # Remove empty rows (missing values I think, but also means lat & lon = 0)
outAus <- outAus[-(which(is.na(outAus$A_growth))),] # Remove NAs (after filling in missing values with missing_values.R)

write.csv(outAus,
          file = "out/files/mu_0.25/Aus_mu0.25.csv", col.names = T, row.names = F )

##### Map from single mu values: ####
mu0.25 <- read.csv("out/files/mu_0.25/Aus_mu0.25.csv")

plot_theme <- theme(panel.background = element_blank(),
                    axis.line = element_blank(), 
                    axis.text = element_blank(), 
                    axis.ticks = element_blank(), 
                    axis.title = element_blank(),
                    plot.title = element_text(size=10))

max(mu0.25$A_growth)
min(mu0.25$A_growth)

Aus_mu0.25 <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0.25,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population growth rate\n(adults)\n",
                     limits=c(-0.03, 
                              0.0351),
                     breaks=c(seq(-0.03, 0.035, by=0.015)),
                     labels=c(seq(-0.03, 0.035, by=0.015)))+
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

Aus_mu0.25
# SAVE

#### Positive growth only ####

Aus_mu0.25_pos <- ggplot(data = sf_oz) + 
  geom_tile(data = mu0.25,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population growth rate\n(adults)\n",
                     limits=c(0, 
                              0.035),
                     breaks=c(seq(0, 0.035, by=0.015)),
                     labels=c(seq(0, 0.035, by=0.015)))+
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

Aus_mu0.25_pos

#########################

##################
#### Compare mu = 0 and mu = 0.35 ####
mu0 <- read.csv("out/files/Aus/mu_0/Aus_mu0.csv")
mu0.25 <- read.csv("out/files/Aus/mu_0.25/Aus_mu0.25.csv")

# Min and max growth rates across both sims
mingrow <- min(mu0.25$A_growth)
maxgrow <- max(mu0$A_growth)
# Lat & lon limits
minlat <- min(mu0$lat)
maxlat <- max(mu0$lat)
minlon <- min(mu0$lon)
maxlon <- max(mu0$lon)

plot_theme_grid <- theme(panel.background = element_blank(),
                         axis.line = element_blank(), 
                         axis.text = element_blank(), 
                         axis.ticks = element_blank(), 
                         axis.title = element_blank(),
                         plot.title = element_text(size=14),
                         legend.position = "top",
                         legend.direction = "horizontal",
                         legend.title = element_text(size=13),
                         legend.text = element_text(size=10, angle = 45, vjust=0.5))

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

plot_mu0.25 <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0.25,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\n growth rate (adults)\n",
                     limits = c(mingrow, maxgrow))+
  geom_sf(fill=NA)+ 
  ggtitle("mu = 0.35")+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme_grid

ggarrange(plot_mu0, plot_mu0.25, ncol=2)

#####
library(ggnewscale)
# DIff col for neg growth
plot_mu0_pos <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth),
            show.legend = FALSE) +
  scale_fill_viridis(name = "Mean daily population\n growth rate (adults)\n",
                     limits = c(0, maxgrow))+
  
  new_scale_fill() +
  geom_tile(data=subset(mu0, A_growth<=0),  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_gradientn(name = "Mean daily population\n growth rate (adults)\n",
                       colours = c("white", "red"),
                       limits=c(mingrow, 0))+
  
  geom_sf(fill=NA)+ 
  ggtitle("dispersal mortality = 0")+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme_grid

plot_mu0.25_pos <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0.25,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\n growth rate (adults)\n",
                     limits = c(0, maxgrow))+
  
  new_scale_fill() +
  geom_tile(data=subset(mu0.25, A_growth<=0),  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth),
            show.legend = FALSE) +
  scale_fill_gradientn(name = "Mean daily population\n growth rate (adults)\n",
                       colours = c("white", "red"),
                       limits=c(mingrow, 0))+
  
  geom_sf(fill=NA)+ 
  ggtitle("dispersal mortality = 0.25")+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme_grid

ggarrange(plot_mu0_pos, plot_mu0.25_pos, ncol=2)

####
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





########### exploration plots
# Plot negative pop growth
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




############################

#### Map positive growth rates only ####

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

# mu = 0.35
plot_mu35_pos <- ggplot(data = sf_oz) + 
  geom_tile(data = subset(mu35, A_growth>0), 
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult growth rate",
                     limits = c(0, 
                                max(mu35$A_growth)))+
  geom_sf(fill=NA)+ 
  ggtitle("mu = 0.35")+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme
plot_mu35_pos

#### Map total number of dispersing PA ####

plot_mu35_disp <- ggplot(data = sf_oz) + 
  geom_tile(data = mu35, 
            aes(x=lon, y=lat, 
                fill=tot_mu)) +
  scale_fill_viridis(name = "Total dispersers per year",
                     trans = "log10",
                     limits=c(1, max(mu35$tot_mu)))+
  geom_sf(fill=NA)+ 
  ggtitle("mu = 0.35")+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme

plot_mu35_disp



# Plot final adult population (>1 only)
ggplot(data = sf_oz) + 
  geom_tile(data = subset(outAus, n_A_end > 1),  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
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
################

# Distribution of growth rates:
ggplot(outAus, aes(x=A_growth))+
  geom_histogram()

neg_growth <- subset(outAus, A_growth <= 0)
ggplot(data = sf_oz) + 
  geom_tile(data = neg_growth,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult growth rate")+
  geom_sf(fill=NA)+ # WA map
  scale_x_continuous(limits=c(min(neg_growth$lon)-0.1,
                              max(neg_growth$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(neg_growth$lat)-0.1,
                              max(neg_growth$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())




# DATA BY SEASON

# Time series (by season)

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

#### Map positive growth rates only ####
map.plot_pos <- ggplot(data = sf_oz) + 
  geom_tile(data=WA,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult growth rate")+
  geom_tile(data=subset(WA, A_growth<=0), # Then overlay negative values in white
            aes(x=lon, y=lat),
            fill="white")+
  geom_sf(fill=NA)+ # WA map
  scale_x_continuous(limits=c(min(WA$lon)-map.res,max(WA$lon)+map.res))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(WA$lat)-map.res,max(WA$lat)+map.res))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())
map.plot_pos
ggsave(map.plot_pos,
       file = "out/WA_map_mu.35_pos.png", 
       #   width = 10, height = 20, dpi = 1000, units = "in", 
       device='png')

  