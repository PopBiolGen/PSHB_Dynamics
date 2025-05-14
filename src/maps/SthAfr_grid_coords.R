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
outputs_grid <- read_csv("out/files/Sth_Africa_0_sim_1.csv")
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
  coord_map(xlim = c(min(outputs_grid$lon)-1.5, 
                     max(outputs_grid$lon)+2.2), 
            ylim = c(min(outputs_grid$lat)-1.5, 
                     max(outputs_grid$lat)+1.5))+
  geom_tile(data=outputs_grid, # Save from matrix to dataframe
            aes(x=lon, y=lat, fill=A_growth)) + # E.g. Adult growth rate
  geom_polygon(data = mapdata,
               aes(x = long, y = lat, group=group),
               col = "black", fill=NA) +
  scale_fill_viridis(name = "Mean daily adult growth rate",
                     option= "inferno",
                     limits=c(round(min.growth, digits=3),
                              round(max.growth, digits=3)))+
  
  geom_point(data=cities, aes(x=lon, y=lat),
             size=2.2, pch=21, stroke=1, fill="white")+
#  geom_text(data=cities, aes(x=lon, y=lat,
#                             label=city),
#            size=4, 
#            vjust= c(3, 3, -4.8, 3.9), # G, D, J, CT
#            hjust= c(0, -0.1, 1.3, 0.8))+
  
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=16))

map.plot.sa
ggsave(map.plot.sa,
       file = "out/map_Sth_Africa_mu0_cities.png", 
       #   width = 10, height = 20, dpi = 1000, units = "in", 
       device='png')

#
ggplot(SA, aes(x=A_growth))+
  geom_histogram()

##

map.plot.sa2 <- map.plot.sa +
  theme(#legend.justification = "top",
    plot.margin = unit(c(2.5,0,3.1,3.7), "cm"))

scale<-0.3

blowplot <- ggdraw() +
  draw_plot(map.plot.sa2)+
  draw_image("out/plots/cities/Cape Town.png",  x = -0.39, y = -0.05, scale = scale)+ 
  draw_image("out/plots/cities/George.png",   x = -0.15, y = -0.36, scale = 0.27)+ 
  draw_image("out/plots/cities/Durban.png",  x = 0.1, y = -0.35, scale = scale)+
  draw_image("out/plots/cities/Johannesburg.png",  x = -0.22, y = 0.35, scale = scale)+
  
  draw_image("out/plots/cities/stage_leg.jpeg",  x = 0.32, y = -0.36, scale = 0.2)

ggsave(blowplot, filename="out/plots/blowplot_SA_2.pdf",
       width=9, height=6)
