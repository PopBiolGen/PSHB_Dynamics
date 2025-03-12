library(ggplot2)
library(raster)
library(terra)
library(tidyterra)
library(ozmaps)
library(viridis)
sf_oz <- subset(ozmap("country"))

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
  




