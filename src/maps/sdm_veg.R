### Filter mechanistic SDM by vegetation type
library(readr)
library(ggplot2)
library(raster)
library(terra)
library(tidyterra)
library(ozmaps)
library(sf)
library(viridis)
library(ggpubr)
sf_oz <- subset(ozmap("country"))

sdm <- read_csv("out/files/mu_0/Aus_mu0.csv") # SDM from prev modelling
sdm <- read_csv("out/files/mu_0.4/Aus_mu0.4.csv") # SDM from prev modelling

# Upload Veg raster
veg <- rast("src/maps/vegtype.tif")
plot(veg)
veg <- project(veg, "EPSG:4283") # Ozmaps crs
plot(veg)
# Check coords alright
ggplot() +
  geom_spatraster(data = veg)+
  geom_point(aes(y = -31.96165, # drop point in king's park
                 x = 115.8317))

# Grid coords for sim

coords <- sdm[,c("lon","lat")]
coords.veg <- cbind(coords, 
                    extract(x=veg, y=coords)) # Extract veg values from raster for each coord
coords.veg <- rename(coords.veg, veg = sum)
coords.veg$veg <- round(coords.veg$veg) # Some cells not 0 or 1

# Still getting duplicates, even when I 
#coords.veg$lon <- round(coords.veg$lon, digits=1)
#coords.veg$lat <- round(coords.veg$lat, digits=1)

#coords.veg %>%
#  mutate_at(vars(veg), funs(round)) %>%
#  mutate_at(vars(lon, lat), funs(round(., 1))) %>%
#  select(lon, lat, veg)

coords.veg <- coords.veg[, c("lon","lat","veg")]

ggplot(coords.veg,
       aes(x=lon, y=lat, fill=veg))+
  geom_tile()

sdm.veg <- left_join(sdm, coords.veg)

# Still getting duplicates...
#sdm$lon <- round(sdm$lon, digits=1)
#sdm$lat <- round(sdm$lat, digits=1)
#sdm.veg %>% distinct(lon, lat, .keep_all = TRUE)


# Filter SDM by habitat type
ggplot(data = sf_oz) + 
  geom_tile(data = sdm.veg,
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\ngrowth rate (adults)\n",
                     option= "inferno",
                     limits=c(-0.026, 
                              0.076),
                     breaks=c(seq(-0.025, 0.075, by=0.025)),
                     labels=c(seq(-0.025, 0.075, by=0.025)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(sdm.veg$lon)-0.1,
                              max(sdm.veg$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(sdm.veg$lat)-0.1,
                              max(sdm.veg$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))


# Veg type
ggplot(data = sf_oz) + 
  geom_tile(data = sdm.veg,
            aes(x=lon, y=lat, fill=veg)) +
  scale_fill_viridis(name = "Mean daily population\ngrowth rate (adults)\n",
                     option= "inferno")+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(sdm.veg$lon)-0.1,
                              max(sdm.veg$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(sdm.veg$lat)-0.1,
                              max(sdm.veg$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

sdm.veg$grow.suit <- ifelse(sdm.veg$veg == 1,
                            sdm.veg$A_growth,
                            NA)

sdm_veg <- ggplot(data = sf_oz) + 
  geom_tile(data = sdm.veg,
            aes(x=lon, y=lat, fill=grow.suit)) +
  scale_fill_viridis(name = "Mean daily population\ngrowth rate (adults)\n",
                     option= "inferno",
                     limits=c(-0.026, 
                              0.076),
                     breaks=c(seq(-0.025, 0.075, by=0.025)),
                     labels=c(seq(-0.025, 0.075, by=0.025)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(sdm.veg$lon)-0.1,
                              max(sdm.veg$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(sdm.veg$lat)-0.1,
                              max(sdm.veg$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))
sdm_veg
ggsave(sdm_veg, file='out/SDM_veg.png')

#
## Ignoring all negative growth:

sdm_veg_pos <- ggplot(data = sf_oz) + 
  geom_tile(data = sdm.veg,
            aes(x=lon, y=lat, fill=grow.suit)) +
  scale_fill_viridis(name = "Mean daily population\ngrowth rate (adults)\n",
                     option= "inferno",
                     limits=c(0, 
                              max(na.omit(sdm.veg$grow.suit))+0.01),
                     labels=c(seq(0, max(na.omit(sdm.veg$grow.suit))+0.01, by=0.02)),
                     breaks=c(seq(0, max(na.omit(sdm.veg$grow.suit))+0.01, by=0.02)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(sdm.veg$lon)-0.1,
                              max(sdm.veg$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(sdm.veg$lat)-0.1,
                              max(sdm.veg$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=15),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=18))
sdm_veg_pos

ggsave(sdm_veg_pos, file='out/SDM_veg_mu0_pos.pdf')
