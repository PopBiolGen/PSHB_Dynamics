library(ggplot2)
library(maps)
install.packages('usmap')
library(usmap)
library(mapdata)
# South Africa coords grid

42.534698, -126.130686
42.728687, -118.747874
31.809880, -119.781160
32.151803, -112.824534


latus <- c(seq(31.8, 42.8, by=map.res))
lonus <- c(seq(-126.2, -112.8, by=map.res))

gridus <- (expand.grid(lonus, latus)) # Grid containing each lat & lon combination
colnames(gridus) <- c("lon", "lat")

# Determine whether points are for land or ocean
mapus <- map("state", fill=TRUE)

map.where(database=mapus, 
          gridus$lon[14985], gridus$lat[14985])

gridus$land <- !is.na(as.numeric(map.where(database=mapus, 
                                           gridus$lon, gridus$lat) == "california"))

gridus$ca <- as.numeric(map.where(database=mapus, 
                                  gridus$lon, gridus$lat) == "california")

gridus <- gridus[!(gridus$land %in% "FALSE"),] # Remove ocean coords
gridus <- subset(gridus, ca==1)
gridus <- gridus[,c(1,2)]

ggplot() +
  geom_point(data=gridus,
             aes(x=lon, y=lat))


write.csv(gridus, 'src/grid_coords_Cal.csv',
          col.names = T, row.names = F )

#########################

# Plot output

options(bitmapType='cairo') # To save png correctly

dev.off()

library(mapdata)
mapdata <- map_data(map='world', region="South Africa")

outputs_grid <- as.data.frame(outputs_grid)
min.growth <- min(outputs_grid$A_growth)
max.growth <- max(outputs_grid$A_growth)

cities <- read_csv("src/known_PSHB_coords.csv")
cities <- subset(cities, country == "South Africa")

map.plot.sa <- ggplot(data = mapdata) + 
  coord_map(xlim = c(min(outputs_grid$lon)-.05, 
                     max(outputs_grid$lon)+.05), 
            ylim = c(min(outputs_grid$lat)-.05, 
                     max(outputs_grid$lat)+.05))+
  geom_tile(data=outputs_grid, # Save from matrix to dataframe
            aes(x=lon, y=lat, fill=A_growth)) + # E.g. Adult growth rate
  geom_polygon(data = mapdata,
               aes(x = long, y = lat, group=group),
               col = "black", fill=NA) +
  scale_fill_viridis(name = "Mean daily adult growth rate",
                     limits=c(round(min.growth, digits=3),
                              round(max.growth, digits=3)))+
  
  geom_point(data=cities, aes(x=lon, y=lat),
             size=1.5)+
  geom_text(data=cities, aes(x=lon, y=lat,
                             label=city),
            size=3.5, vjust=-0.75, hjust=0.05)+
  
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())
map.plot.sa
ggsave(map.plot.sa,
       file = "out/map_Sth_Africa_mu0_cities.png", 
       #   width = 10, height = 20, dpi = 1000, units = "in", 
       device='png')

#
ggplot(SA, aes(x=A_growth))+
  geom_histogram()
