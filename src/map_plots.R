# OUTPUT DATA = outputs_grid

# Map positive growth rates only
map.plot_pos <- ggplot(data = sf_oz) + 
  geom_tile(data=WA,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult growth rate")+
  geom_tile(data=subset(WA, A_growth<=0), # Then overlay negative values in white
            aes(x=lon, y=lat),
            fill="white")+
  geom_sf(fill=NA)+ # WA map
  scale_x_continuous(limits=c(min(WA$lon)-map.res,max(WA$lon)+map.res))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(WA$lat)-map.res,max(WA$lat)+map.res))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())
map.plot_pos
ggsave(map.plot_pos,
       file = "out/WA_map_mu.35_pos.png", 
       #   width = 10, height = 20, dpi = 1000, units = "in", 
       device='png')

################

# Time series (by season)

# Map grid:
map.season <- function(season){
  ggplot(data = sf_oz) + 
    geom_tile(data = outputs_grid,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
              aes(x=lon, y=lat, 
                  fill = outputs_grid[, season])) +
    scale_fill_viridis(name = "Mean daily adult growth rate",
                       limits=c(min(outputs_grid[,7:10]), # Fixed limits same across all seasons
                                max(outputs_grid[,7:10])))+
    geom_sf(fill=NA)+ # WA map
    scale_x_continuous(limits=c(min(lon)-map.res,max(lon)+map.res))+ # Fit plot to lat & lon range
    scale_y_continuous(limits=c(min(lat)-map.res,max(lat)+map.res))+
    theme(panel.background = element_blank(),
          axis.line = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank())
}     
map.summer <- map.season("summer")
map.autumn <- map.season("autumn")
map.winter <- map.season("winter")
map.spring <- map.season("spring")

map_allseas <- ggarrange(map.summer, map.autumn, map.winter, map.spring,
                         nrow=2, byrow=T)

####################

# Animation:
install.packages("reshape2")
library(reshape2)
out_melt <- as.data.frame(outputs_grid[,c("lon","lat",
                                       "summer","autumn","winter","spring")])
out_melt <- melt(out_melt,
                 id = c("lon","lat"),
                 value.name = "growth",
                 variable.name = "season")
# Give numeric value to season
#out_melt$t <- ifelse(out_melt$season=="summer",
 #                    1,
  #                   ifelse(out_melt$season=="autumn",
   #                         2,
    #                        ifelse(out_melt$season=="winter",
     #                              3,
      #                             4)))
install.packages(c("gganimate","gifski")) 
library(gifski)
library(gganimate)

map.anim <- ggplot(data = sf_oz) + 
  geom_tile(data=out_melt, 
            aes(x=lon, y=lat, fill=growth)) + # E.g. Adult growth rate
  geom_sf(fill=NA)+ # WA map
  scale_x_continuous(limits=c(min(out_melt$lon)-map.res,max(out_melt$lon)+map.res))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(out_melt$lat)-map.res,max(out_melt$lat)+map.res))+
  scale_fill_viridis(name = "Mean daily adult growth rate",
                     limits=c(min(out_melt$growth), 
                              max(out_melt$growth)))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())

anim <- map.anim + 
  transition_states(out_melt$season) + # Different frame for each time point
  ggtitle("{closest_state}")

anim <- animate(anim, # Animate frames
                renderer  = gifski_renderer(), 
                duration = 8) # 

anim_save(file = "out/anim_map_perth.gif", anim) # Save


  