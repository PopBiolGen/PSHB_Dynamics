library(ggplot2)
library(maps)
install.packages('usmap')
library(usmap)
library(readr)
library(mapdata)
library(viridis)
# South Africa coords grid


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

# Output data
outputs_grid <- read_csv("out/files/Cali_0_sim_1.csv")

options(bitmapType='cairo') # To save png correctly
dev.off()

mapdata <- map_data(map='state', region="california")

outputs_grid <- as.data.frame(outputs_grid)
min.growth <- min(outputs_grid$A_growth)
max.growth <- max(outputs_grid$A_growth)

cities <- read_csv("src/known_PSHB_coords.csv")
cities <- subset(cities, country == "USA")

cities$city <- factor(cities$city, levels=c("Santa Paula", "San Marino", "Laguna Beach"))

map.plot.us <- ggplot(data = mapdata) + 
  coord_map(xlim = c(min(outputs_grid$lon)-.05, 
                     max(outputs_grid$lon)+.05), 
            ylim = c(min(outputs_grid$lat)-.05, 
                     max(outputs_grid$lat)+.05))+
  geom_tile(data=outputs_grid, # Save from matrix to dataframe
            aes(x=lon, y=lat, fill=A_growth)) + # E.g. Adult growth rate
  geom_polygon(data = mapdata,
               aes(x = long, y = lat, group=group),
               col = "black", fill=NA) +
  scale_fill_viridis(name = "Mean daily growth rate\n",
                     option= "inferno",
                     limits=c(round(min.growth, digits=3),
                              0.05),
                     breaks = seq(-0.01, 0.05, by=0.01))+#round(max.growth, digits=3)))+
  
  geom_point(data=cities, aes(x=lon, y=lat),
             size=2, pch=21, stroke=1, fill="white")+
#  geom_text(data=cities, aes(x=lon, y=lat,
#                             label=city),
#            size=4, 
#            vjust= c(2.9, 2.8, 1.4), 
#            hjust= c(1.2, 1.8, 2))+ # Lag, Marin, Paul
  
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

map.plot.us
ggsave(map.plot.us,
       file = "out/map_California_mu0_cities.png", 
       #   width = 10, height = 20, dpi = 1000, units = "in", 
       device='png')

##### With city blow outs ####
scale <- 0.31

map.plot.us2 <- map.plot.us +
  theme(#legend.justification = "top",
        plot.margin = unit(c(0,0,5,3), "cm"))

blowplot <- ggdraw() +
  draw_plot(map.plot.us2)+
  draw_image("out/plots/cities/Laguna Beach.png",  x = 0, y = -0.33, scale = scale)+ 
  draw_image("out/plots/cities/San Marino.png",  x = -0.28, y = -0.33, scale = scale)+ 
  draw_image("out/plots/cities/Santa Paula.png",  x = -0.28, y = 0, scale = scale)+
  
  draw_image("out/plots/cities/stage_leg.jpeg",  x = 0.24, y = -0.33, scale = 0.21)

ggsave(blowplot, filename="out/plots/blowplot_Cali_2.pdf",
       width=9, height=6)


######

#map.plot.us1 <- map.plot.us +
#  theme(legend.justification = "bottom")

#blowplot <- ggdraw() +
#  draw_plot(map.plot.us1)+
#  draw_image("out/plots/cities/Laguna Beach.png",  x = 0.28, y = 0.03, scale = scale)+
#  draw_image("out/plots/cities/San Marino.png",  x = 0.28, y = 0.33, scale = scale)+
#  draw_image("out/plots/cities/Santa Paula.png",  x = 0, y = 0.33, scale = scale)
#ggsave(blowplot, filename="out/plots/blowplot_Cali.pdf",
#       width=9, height=6)


