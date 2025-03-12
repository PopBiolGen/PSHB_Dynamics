library(dplyr)
library(lubridate)
library(maps)
library(ozmaps)
library(sf)
sf_oz <- subset(ozmap("country"))

map.res <- 0.1
latall <- c(seq(-45, -10, by=map.res))
lonall <- c(seq(110, 160, by=map.res))

grid <- (expand.grid(lonall, latall)) # Grid containing each lat & lon combination
colnames(grid) <- c("lon", "lat")

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
             aes(x=lon, y=lat, col=land))

grid_coords <- as.matrix(grid[,c("lon","lat")]) # Subset just lat & lon, convert to matrix

write.csv(grid_coords, 'src/grid_coords_Aus.csv')
