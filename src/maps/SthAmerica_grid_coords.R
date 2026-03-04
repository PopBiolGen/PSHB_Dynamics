library(ggplot2)
library(maps)
library(mapdata)
library(readr)
library(viridis)
library(sf)
library(mapdata)

#install.packages('rnaturalearth')
library(rnaturalearth)

# South America coords grid
mapsa <- map(fill=TRUE) # Can just use complete world map

map.res <- 0.5 # May as well just run for same resolution as POWER weather data
latsa <- c(seq(-55.7, 12.5, by=map.res))
lonsa <- c(seq(-81.2, -33.8, by=map.res))
gridsa <- (expand.grid(lonsa, latsa)) # Grid containing each lat & lon combination
colnames(gridsa) <- c("lon", "lat")

# Check for land with polygon
gridsa$land <- !is.na(as.numeric(map.where(database=mapsa, gridsa$lon, gridsa$lat) == "South America")) # Land (TRUE) when coords match with map polygon

ggplot(gridsa)+
  geom_point(aes(x=lon, y=lat, col=land))

gridsa <- gridsa[!(gridsa$land %in% "FALSE"),] # Remove ocean coords
gridsa <- gridsa[,c(1,2)]


  
# with sf map
#grid <- gridsa
#mapsa <- ne_countries(continent = "South America", 
#                      returnclass = "sf", scale = "large")
#grid$points <- st_as_sf(grid, coords=1:2, # Convert coords to sf object
#                        crs=st_crs(mapsa)) # Coordinate reference system
#grid$points <- st_transform(grid$points, st_crs(mapsa))
#grid$land <- !is.na(as.numeric(st_intersects(grid$points, mapsa))) # Land (TRUE) when coords match with map polygon
#grid <- grid[!(grid$land %in% "FALSE"),] # Remove ocean coords

write.csv(gridsa, 'src/grid_coords_Sth_America.csv',
          col.names = T, row.names = F )


#### Plot outputs ####

outputs_grid <- read_csv("out/files/pawsey_South America_0_sim_NA.csv")

mapdata <- map_data(map='world', region="South Africa")

outputs_grid <- as.data.frame(outputs_grid)
min.growth <- min(outputs_grid$A_growth)
max.growth <- max(outputs_grid$A_growth)

cities <- read_csv("src/known_PSHB_coords.csv")
cities <- subset(cities, country == "South America")

mapsa <- ne_countries(continent = "South America", 
                      returnclass = "sf", scale = "large")


map.sam <- ggplot() + 
  geom_sf(data = mapsa,
               col = "black", fill="grey80", lwd=0.01) +
 # geom_raster(data=outputs_grid, # Save from matrix to dataframe
  #          aes(x=lon, y=lat, fill=A_growth),
   #         interpolate = T) + #??
   geom_tile(data=outputs_grid, # Save from matrix to dataframe
            aes(x=lon, y=lat, fill=A_growth)) + 
  
  geom_sf(data = mapsa,
          col = "black", fill=NA, lwd=0.6) +
  
  geom_point(data=cities, aes(x=lon, y=lat),
             size=2.2, pch=21, stroke=1, fill="white")+
  
  scale_fill_viridis(name = "Mean daily adult\ngrowth rate\n",
                     option= "inferno",
                     limits=c(min.growth,
                              max.growth+0.01),
                     breaks = seq(-0.02, 0.08, by=0.02))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=16),
        legend.position = 'left')+
  
  coord_sf(xlim = c(min(gridsa$lon), 
                     max(gridsa$lon)), 
            ylim = c(min(gridsa$lat), 
                     max(gridsa$lat)))
map.sam
ggsave(map.sam,
       file = "out/plots/map_South_America.png", 
       #   width = 10, height = 20, dpi = 1000, units = "in", 
       device='png')
##
### Zoom in ###
mapdata <- map_data(map='world', region="Argentina")
mapdata <- map_data(map='world', region="Uruguay")
mapdata <- map_data(map='world', region="Brazil")

ggplot() + 
  geom_tile(data=outputs_grid, # Save from matrix to dataframe
            aes(x=lon, y=lat, fill=A_growth)) + 
  
  geom_polygon(data = mapdata,
               aes(x = long, y = lat, group=group),
               col = "black", fill=NA, lwd=1) +
  
  geom_point(data=cities, aes(x=lon, y=lat),
             size=2.2, pch=21, stroke=1, fill="white")+
  
  scale_fill_viridis(name = "Mean daily adult\ngrowth rate\n",
                     option= "inferno",
                     limits=c(0,
                              max.growth+0.01),
                     breaks = seq(0, 0.08, by=0.02))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=16))+
  coord_sf(xlim = c(min(mapdata$long), 
                    max(mapdata$long)), 
           ylim = c(min(mapdata$lat), 
                    max(mapdata$lat)))


