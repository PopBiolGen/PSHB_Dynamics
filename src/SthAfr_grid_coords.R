library(ggplot2)
library(maps)
library(mapdata)
# South Africa coords grid

latsa <- c(seq(-35.5, -21.5, by=map.res))
lonsa <- c(seq(15.3, 33.5, by=map.res))

gridsa <- (expand.grid(lonsa, latsa)) # Grid containing each lat & lon combination
colnames(gridsa) <- c("lon", "lat")

# Determine whether points are for land or ocean
mapsa <- map(region="South Africa", fill=TRUE)
gridsa$land <- !is.na(as.numeric(map.where(database=mapsa, gridsa$lon, gridsa$lat) == "South Africa")) # Land (TRUE) when coords match with map polygon

gridsa <- gridsa[!(gridsa$land %in% "FALSE"),] # Remove ocean coords
gridsa <- gridsa[,c(1,2)]

ggplot() +
  geom_point(data=gridsa,
             aes(x=lon, y=lat))


write.csv(gridsa, 'src/grid_coords_Sth_Africa.csv',
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
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())

ggsave(map.plot.sa,
       file = "out/map_Sth_Africa_mu0.png", 
       #   width = 10, height = 20, dpi = 1000, units = "in", 
       device='png')
