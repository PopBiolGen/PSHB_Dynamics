library(ggplot2)
library(raster)
library(terra)
library(tidyterra)
library(ozmaps)
library(viridis)
library(ggpubr)
sf_oz <- subset(ozmap("country"))

# Upload our model results
mu0 <- read.csv("out/files/mu_0/Aus_mu0.csv")
mu0.4 <- read.csv("out/files/mu_0.4/Aus_mu0.4.csv")

### Li et al. 2025 (Fuzzy logic, GAM)

library(tidyterra)

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
r <- raster(tmp)



m.dat <- matrix(c(data[1,]), 
                ncol=ncols, nrow=nrows, byrow=TRUE)

m.dat[m.dat == -9999] = NA

rr <- raster(m.dat,
             crs = "+proj=lcc +lon_0=135 +lat_0=-30 +lat_1=-10 +lat_2=-45 +datum=WGS84")



