# Need these for weatherOz to run
library(httpcode)
library(urltools)
# Plotting
library(ggplot2)
library(ozmaps)
library(sf)
library(viridis)
# From 'basic within-pop models.R'
library(dplyr)
library(lubridate)
## Load up the functions
source("src/TPCFunctions.R")
source("src/modelFunctions.R")

# WA map
sf_oz <- subset(ozmap("states"), NAME=="Western Australia")

# For now, manually select coordinate ranges to construct a retangular grid covering whole area
# SILO data resolution = 0.05 x 0.05 degrees
lat <- c(seq(-33.3, -31, by=0.05)) # Latitude range
lon <- c(seq(115.2, 116.8, by=0.05)) # Longitude range
grid <- (expand.grid(lon, lat)) # Grid containing each lat & lon combination
colnames(grid) <- c("lon", "lat")
# Check on map
ggplot(data = sf_oz) +
  geom_sf()+
  geom_point(data=grid,
             aes(x=lon, y=lat))+
  scale_x_continuous(limits=c(min(lon),max(lon)))+
  scale_y_continuous(limits=c(min(lat),max(lat)))+
  theme(panel.background = element_blank())

## Need to subset only the coordinates that fall on land (ignore ocean)

grid$points <- st_as_sf(grid, coords=1:2, # Convert coords to sf object
                        crs=st_crs(sf_oz)) # Coordinate reference system

# Determine whether points are for land or ocean
grid$land <- !is.na(as.numeric(st_intersects(grid$points, sf_oz))) # Land (TRUE) when coords match with map polygon
grid <- grid[!(grid$land %in% "FALSE"),] # Remove ocean coords

# Check on map
ggplot(data = sf_oz) +
  geom_sf()+
  geom_point(data=grid,
             aes(x=lon, y=lat, col=land))+
  scale_x_continuous(limits=c(min(lon),max(lon)))+
  scale_y_continuous(limits=c(min(lat),max(lat)))

# Create output matrix
grid2 <- as.matrix(grid[,-c(3,4)]) # Subset just lat & lon, convert to matrix
outputs_grid <- matrix(0, nrow=nrow(grid), ncol=5)
outputs_grid[c(1:nrow(outputs_grid)),c(1:2)] <- grid2 # Insert lat & lon for each coord
colnames(outputs_grid)<- c("lon","lat","J","P","A") # Leave remaining columns to insert J, P & A growth rates

## Run model for each coordinate:

for(i in 1:nrow(outputs_grid)){
  locLong <- outputs_grid[i, "lon"]
  locLat <- outputs_grid[i, "lat"]
  yearSim <- run_year(lat = locLat, long = locLong, make_plot = FALSE) # FROM 'basic within-pop model.R'
  rate_grid <- yearSim$growthRate # Calc mean growth rates
  outputs_grid[i,c(3:5)] <- rate_grid # Insert growth rates into output matrix (with corresponding coords)
}

# Plot output
ggplot(data = sf_oz) + 
  geom_tile(data=outputs_grid, 
            aes(x=lon, y=lat, fill=A)) + # E.g. Adult growth rate
  geom_sf(fill=NA)+ # WA map
  scale_x_continuous(limits=c(min(lon)-.05,max(lon)+.05))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(lat)-.05,max(lat)+.05))+
  scale_fill_viridis()+
  theme(panel.background = element_blank())

write.csv(outputs_grid, # Save output
          file = "out/perth_grid.csv", col.names = T )

