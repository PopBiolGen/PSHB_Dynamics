library(ggplot2)
library(readr)
library(raster)
library(terra)
library(tidyterra)
library(ozmaps)
library(sf)
library(viridis)
library(ggpubr)
sf_oz <- subset(ozmap("country"))


### See sdm_veg to filter SDM with vegetation type raster

# Population size

pop <- rast("src/maps/pop_2018.tif")
plot(pop)
pop <- project(pop, "EPSG:4283") # Ozmaps crs
plot(pop)
# Check coords alright
ggplot() +
  geom_spatraster(data = pop)+
  geom_point(aes(y = -31.96165, # drop point in king's park
                 x = 115.8317))

# Grid coords for sim
perth <- read_csv("src/grid_coords_perth.csv")
perth <- perth[,c("lon","lat")]
perth.pop <- cbind(perth, 
                    extract(x=pop, y=perth)) # Extract pop values from raster for each coord
perth.pop <- rename(perth.pop, pop = pop_2018)

ggplot(perth.pop,
       aes(x=lon, y=lat, fill=pop))+
  geom_tile()+
  geom_point(aes(x=115.861019, y=-31.950706), # CBD
             pch=4, col="red", size=4)


# Calculate distance between source (e.g. King's Park) and each point

##### I'm having trouble with this...
# Easiest might be to just wait for distance matrix to finish,
# Then can just subset any given column out.

perth$points <- st_as_sf(perth, coords=1:2, 
                        crs=st_crs(sf_oz))
perth$points <- st_transform(perth$points, 3577)


source.point <- data.frame(lon = rep(115.8317, times=nrow(perth.pop)),
                              lat = rep(-31.96165, times=nrow(perth.pop)))
source.point$source_points <- st_as_sf(source.point, coords=1:2, 
                         crs=st_crs(sf_oz))
source.point$source_points <- st_transform(source.point$source_points, 3577)

perth.d <- cbind(perth, source.point)

perth.d$dist <- 1
for(i in 1:nrow(perth.d)){
  perth.d[i, 7] <- dist(perth.d[i, 3], perth.d[i, 6])[1]
}

################################

# Upload NDVI raster

ndvi <- rast("src/maps/NDVI.tif")
plot(ndvi)
ndvi <- project(ndvi, "EPSG:4283") # Ozmaps crs
plot(ndvi)
# Check coords alright
ggplot() +
  geom_spatraster(data = ndvi)+
  geom_point(aes(y = -31.96165, # drop point in king's park
             x = 115.8317))

# Grid coords for sim
perth <- read_csv("src/grid_coords_perth.csv")
perth <- perth[,c("lon","lat")]
perth.ndvi <- cbind(perth, 
                  extract(x=ndvi, y=perth)) # Extract ndvi values from raster for each coord
perth.ndvi <- rename(perth.ndvi, ndvi = sum)

ggplot(perth.ndvi,
       aes(x=lon, y=lat, fill=ndvi))+
  geom_tile()

ggplot(subset(perth.ndvi, ndvi>0),
       aes(x=lon, y=lat, fill=ndvi))+
  geom_tile()


########################################################

# Upload our model results
mu0 <- read.csv("out/files/mu_0/Aus_mu0.csv")
mu0.4 <- read.csv("out/files/mu_0.4/Aus_mu0.4.csv")

### Li et al. 2025 (Fuzzy logic, GAM)

rast("src/maps/GAM_Favorability.tiff")
rr <- rast("src/maps/GAM_Favorability.tiff")
res(rr) # Resoluation
crs(rr, proj=TRUE)
crs(rr) <- "+proj=lcc +lon_0=135 +lat_0=-30 +lat_1=-10 +lat_2=-45 +datum=WGS84" # Ozmaps CRS
crs(rr)
plot(rr)

Li.plot <- ggplot() +
  geom_spatraster(data = rr)+
  scale_fill_viridis(name = "Climate suitability score\n",
                     option= "inferno",
                     limits=c(0, 
                              1),
                     breaks=c(seq(0, 1, by=.2)),
                     labels=c(seq(0, 1, by=.2)),
                     na.value=NA)+
  coord_sf(
    xlim = c(min(mu0$lon),
             max(mu0$lon)),
    ylim = c(min(mu0$lat),
             max(mu0$lat))) +
  ggtitle("B)")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        plot.title = element_text(size=19),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))
Li.plot

### Our model

mech.plot <- ggplot(data = sf_oz) + 
  geom_tile(data = mu0,
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\ngrowth rate (adults)\n",
                     option= "inferno",
                     limits=c(-0.026, 
                              0.076),
                     breaks=c(seq(-0.025, 0.075, by=0.025)),
                     labels=c(seq(-0.025, 0.075, by=0.025)))+
  
  coord_sf(
    xlim = c(min(mu0$lon),
             max(mu0$lon)),
    ylim = c(min(mu0$lat),
             max(mu0$lat))) +
  ggtitle("A)")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        plot.title = element_text(size=19),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))
mech.plot


ggarrange(mech.plot, Li.plot, ncol=2)

### BioSecurity Commons (Climatch, records from GBIF - note that these are not the records used in Warnakula)

rr <- rast("src/maps/biocomms_sdm.tif")
res(rr) # Resoluation
crs(rr, proj=TRUE)
crs(rr) <- "+proj=lcc +lon_0=135 +lat_0=-30 +lat_1=-10 +lat_2=-45 +datum=WGS84" # Ozmaps CRS
crs(rr)
plot(rr)

rr <- crop(rr, sf_oz)
rr <- (mask(rr, sf_oz))

Bio.plot <- ggplot() +
  geom_spatraster(data = rr,
                  interpolate = T)+
  scale_fill_viridis(name = "Climate suitability score\n",
                     option= "inferno",
                     limits=c(0, 
                              10),
                     breaks=c(seq(0, 10, by=2)),
                     labels=c(seq(0, 10, by=2)),
                     na.value=NA)+
  coord_sf(
    xlim = c(min(mu0$lon),
             max(mu0$lon)),
    ylim = c(min(mu0$lat),
             max(mu0$lat))) +
  
#  ggtitle("B)")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        plot.title = element_text(size=19),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))
Bio.plot

#### Look at GBIF data #####
gbif <- read_csv("src/maps/gbif_records.csv")

library(mapdata)
mapdata <- map_data(map='world')

ggplot(data = mapdata) + 

  geom_polygon(data = mapdata,
               aes(x = long, y = lat, group=group),
               col = "black", fill=NA, lwd=1) +
  
  geom_point(data=gbif, aes(x=lon, y=lat, col=country_code), 
             size=2)+
  geom_text(data=gbif, aes(x=lon, y=lat, label=country_code), 
             size=2)
  
nrow(gbif) # Have 329 of the 334 records after fixing dataframe
# But lots of duplicates

gbif2 <- gbif[!duplicated(gbif[, c("lat","lon")]), ]
nrow(gbif2) # 267 after removing duplicates
table(gbif2$country_code)

write.csv(gbif2, 'src/maps/pshb_occ.csv')

###### Run Climatch again with edited/cleaned dataset #####
# GBIF with duplicates removed, plus extra locations:
# South America (Ceriani, Covre)
# China, Vietnam & Taiwan (Smith)
# 1 for Vietnam (Liu)
# These latter through Warnakula

rr <- rast("src/maps/biocomms_sdm_v2.tif")
res(rr) # Resoluation
crs(rr, proj=TRUE)
crs(rr) <- "+proj=lcc +lon_0=135 +lat_0=-30 +lat_1=-10 +lat_2=-45 +datum=WGS84" # Ozmaps CRS
crs(rr)
plot(rr)

rr <- crop(rr, sf_oz)
rr <- (mask(rr, sf_oz))

Bio.plot_v2 <- ggplot() +
  geom_spatraster(data = rr,
                  interpolate = T)+
  scale_fill_viridis(name = "Climate suitability score\n",
                     option= "inferno",
                     limits=c(0, 
                              10),
                     breaks=c(seq(0, 10, by=2)),
                     labels=c(seq(0, 10, by=2)),
                     na.value=NA)+
  coord_sf(
    xlim = c(min(mu0$lon),
             max(mu0$lon)),
    ylim = c(min(mu0$lat),
             max(mu0$lat))) +
  
  #  ggtitle("B)")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        plot.title = element_text(size=19),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))
Bio.plot_v2

ggarrange(Bio.plot, Bio.plot_v2, ncol=2)

# 7 or above (highly suitable)

ggplot() +
  geom_spatraster(data = rr,
                  interpolate = T)+
  scale_fill_viridis(name = "Climate suitability score\n",
                     option= "inferno",
                     limits=c(7, 
                              10),
                     breaks=c(seq(7, 10, by=1)),
                     labels=c(seq(7, 10, by=1)),
                     na.value=NA)+
  coord_sf(
    xlim = c(min(mu0$lon),
             max(mu0$lon)),
    ylim = c(min(mu0$lat),
             max(mu0$lat))) +
  
  #  ggtitle("B)")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        plot.title = element_text(size=19),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

### From Climatch

infile <- "dat/target.asc"
#infile <- "dat/target.txt"

ncols <- 200
nrows <- 192

data <- as.matrix(read.table(infile, skip = 6))
ncol(data)

m.dat <- matrix(c(data[1,]), 
                ncol=ncols, nrow=nrows, byrow=TRUE)

m.dat[m.dat == -9999] = NA

rr <- raster(m.dat,
             crs = "+proj=lcc +lon_0=135 +lat_0=-30 +lat_1=-10 +lat_2=-45 +datum=WGS84")

plot(rr)

mu0 <- read.csv("out/files/mu_0/Aus_mu0.csv")
minlon <- min(mu0$lon)
maxlon <- max(mu0$lon)
minlat <- min(mu0$lat)
maxlat <- max(mu0$lat)

extent(rr) = c(minlon, maxlon, minlat, maxlat)
res(rr)
crs(rr) <- "+proj=lcc +lon_0=135 +lat_0=-30 +lat_1=-10 +lat_2=-45 +datum=WGS84"
plot(rr)


## ggplot

test_spdf <- as(rr, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

#st_as_sf(grid, coords=1:2, # Convert coords to sf object
#         crs=st_crs(sf_oz))


ggplot() + 
#  geom_sf(data=sf_oz,
 #         fill=NA, lwd=0.75, col="black")+
  geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8)+
  scale_fill_viridis(name = "Climate suitability score",
                     option= "inferno",
                     limits=c(0, 
                              8),
                     breaks=c(seq(0, 8, by=2)),
                     labels=c(seq(0, 8, by=2)))+
  scale_x_continuous(limits=c(min(mu0$lon)-0.1,
                              max(mu0$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0$lat)-0.1,
                              max(mu0$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

  #  coord_sf(xlim = c(112,
   #                 159),
    #       ylim = c(-44,
     #               -10))




ggplot() + 
  geom_spatraster(data = rast(m.dat))
  

#### Li TIFF... other method

# TIFF
infile <- "src/maps/GAM_Favorability.tiff"
# From TIFF summary:
ncols <- 277
nrows <- 197
r <-  raster(ncols=ncols, nrows=nrows)
r[] <- 1
tmp <- paste(tempdir(), infile, sep = "/")
writeRaster(r, tmp)

## read now

rasterToPoints(ndvi, spatial=TRUE)

m.dat <- matrix(c(data[1,]), 
                ncol=ncols, nrow=nrows, byrow=TRUE)

m.dat[m.dat == -9999] = NA

rr <- raster(m.dat,
             crs = "+proj=lcc +lon_0=135 +lat_0=-30 +lat_1=-10 +lat_2=-45 +datum=WGS84")



