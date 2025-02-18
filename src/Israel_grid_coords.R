library(ggplot2)
library(maps)
library(mapdata)
library(readr)
library(viridis)

# 33.402722, 34.841143

latis <- c(seq(29.2, 33.7, by=map.res)) 
lonis <- c(seq(33.7, 36, by=map.res))

gridis <- (expand.grid(lonis, latis)) # Grid containing each lat & lon combination
colnames(gridis) <- c("lon", "lat")

# Determine whether points are for land or ocean
mapis <- map(region="Israel", fill=TRUE)
gridis$land <- !is.na(as.numeric(map.where(database=mapis, gridis$lon, gridis$lat) == "South Africa")) # Land (TRUE) when coords match with map polygon

gridis <- gridis[!(gridis$land %in% "FALSE"),] # Remove ocean coords
gridis <- gridis[,c(1,2)]

ggplot(data = mapis) +
  geom_point(data=gridis,
             aes(x=lon, y=lat))


write.csv(gridis, 'src/grid_coords_Israel.csv',
          col.names = T, row.names = F )

#########################

# Plot output
outputs_grid <- read_csv("out/files/Israel_0_sim_1.csv")
outputs_grid <- read_csv("out/files/Israel_upd.csv")

library(mapdata)
library(cowplot)
library(magick)

mapdata <- map_data(map='world', region="Israel")

cities <- read_csv("src/known_PSHB_coords.csv")
cities <- subset(cities, country == "Israel")

outputs_grid <- as.data.frame(outputs_grid)
min.growth <- min(outputs_grid$A_growth)
max.growth <- max(outputs_grid$A_growth)

map.plot.is <- ggplot(data = mapdata) + 
  coord_map(xlim = c(min(outputs_grid$lon), 
                     max(outputs_grid$lon)), 
            ylim = c(min(outputs_grid$lat), 
                     max(outputs_grid$lat)))+
  geom_tile(data=outputs_grid, # Save from matrix to dataframe
            aes(x=lon, y=lat, fill=A_growth)) + # E.g. Adult growth rate
  geom_polygon(data = mapdata,
               aes(x = long, y = lat, group=group),
               col = "black", fill=NA) +
  scale_fill_viridis(name = "Mean daily adult growth rate",
                     option= "inferno",
                     limits=c(0,
                              round(max.growth, digits=3)))+
  
  geom_point(data=cities, aes(x=lon, y=lat),
             size=2.4, pch=21, stroke=1.2, fill="white")+
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

map.plot.is
ggsave(map.plot.is,
       file = "out/plots/map_Israel.pdf", 
       #   width = 10, height = 20, dpi = 1000, units = "in", 
       device='pdf')



map.plot.is2 <- map.plot.is +
  theme(#legend.justification = "top",
    plot.margin = unit(c(0,0,0,0), "cm"))

scale<-0.32

blowplot <- ggdraw() +
  draw_plot(map.plot.is2)+
  draw_image("out/plots/cities/Tel-Aviv.png",  x = -0.38, y = 0.32, scale = scale)+ 
  
  draw_image("out/plots/cities/stage_leg.jpeg",  x = -0.39, y = 0.03, scale = 0.21)

ggsave(blowplot, filename="out/plots/blowplot_Israel2.pdf",
       width=9, height=6)
