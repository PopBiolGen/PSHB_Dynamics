
### RUN THIS ON BASE R - gglocator doesn't work on R Studio ####
library(ozmaps)
library(sf)
library(viridis)
library(ggplot2)
library(ggpubr)
library(readr)
library(dplyr)
sf_oz <- subset(ozmap("country")) # All Australia
#install.packages('rasterVis')
library(rasterVis)
library(raster)
#install.packages('ggmap')
library(ggmap)

IMA <- raster('src/QZ.png')
levelplot(IMA,col.regions = viridis::viridis(72))

image2 = stretch(IMA)
levelplot(image2,col.regions = viridis::viridis(72))

# Find coord limits of image?
# i.e. extent of QZ
# Rottnest -32.024700°S 115.447895°E
# NW -31.455313, 115.560859 // -31.455268, 115.604129
# East -31.910593, 116.414654
# South -32.482301, 115.817871

#(lon, lat)
extent(image2) =c(115.448, 116.415, -32.482, -31.455)

projection(image2) = crs=st_crs(sf_oz)

image3 = stack(image2,image2,image2)
DF = as.data.frame(image3, xy = TRUE)
head(DF)
colnames(DF)  = c('x', 'y', 'Red', 'Green', 'Blue') 
head(DF)

ggplot(data = DF, aes(x = x, y =y))+                   #plot map
  geom_raster(fill = rgb(DF$Red/255, DF$Green/255, DF$Blue/255)) +
  scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) +
  theme(text= element_text(size=25, family = "Arial", colour = "black")) + 
  labs(x = 'Longitude', y = 'Latitude') +
  theme_classic()

raster_df = as.data.frame(image2, xy = TRUE)
world = map_data("world")

writeRaster(image2,'src/QZRF.tif',
            format = "GTiff", overwrite = TRUE)
img = brick('src/QZRF.tif')
img
img2 = stack(img,img,img)
img2
DF2 = as.data.frame(img2, xy = TRUE)
head(DF2)
colnames(DF2)  = c('x', 'y', 'Red', 'Green', 'Blue') 
head(DF2)

ggplot(data = DF2, aes(x = x, y =y))+                   #plot map
  geom_raster(fill = rgb(DF2$Red/255, DF2$Green/255, DF2$Blue/255)) +
  scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) +
  theme(text= element_text(size=25, family = "Arial", colour = "black")) + 
  labs(x = 'Longitude', y = 'Latitude') +
  theme_bw()

# SELECT POINTS
# Click on points on map (to trace shape of QZ) 

df1 <- gglocator(n = 80) # Doesn't save points if you exit locator before using all n points
            # Just repeatedly click at last location until you use up all the points (can delete later from dataframe)

df1 = data.frame(x1 = df1['x'],
                 y1 = df1['y']) 
colnames(df1) = c('Longitude', 'Latitude')
head(df1)

ggplot(data = sf_oz) + 
  geom_polygon(data = df1,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
               aes(x=Longitude, y=Latitude),
               fill=NA, col="red") +
  geom_sf(fill=NA)+
  scale_x_continuous(limits=c(115,116.5))+
  scale_y_continuous(limits=c(-33, -30.5))

write.csv(df1, 'PSHB/PSHB_Dynamics/src/QZdf4.csv')

