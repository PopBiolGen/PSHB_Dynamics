library(ggplot2)
library(cowplot)
library(magick)

cities <- read_csv("src/city_coords.csv")
Aus2 <- ggplot(data = sf_oz) + 
  geom_tile(data = subset(aus, mu==0),
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\ngrowth rate (adults)\n",
                     option= "inferno",
                     limits=c(-0.026, 
                              0.076),
                     breaks=c(seq(-0.025, 0.075, by=0.025)),
                     labels=c(seq(-0.025, 0.075, by=0.025)))+
  geom_sf(fill=NA)+ 
  
  geom_point(data=cities, aes(x=lon, y=lat),
             size=1.6, pch=21, stroke=0.6, fill="white")+
  
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))+
  
  scale_x_continuous(limits=c(min(mu0$lon)+0.5,
                              max(mu0$lon)+1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0$lat)-1,
                              max(mu0$lat)+1))+
  theme(plot.margin = unit(c(3,3,3,1), "cm"),
        legend.text = element_text(size=9),
        legend.key.size = unit(0.6, 'cm'),
        legend.title = element_text(size=10),
        legend.position = "left")

scale <- 0.22

scale <- 0.24 #new


#### Grid ####


blowplot <- ggdraw() +
  draw_plot(Aus2)+
  
  
  draw_image("out/plots/cities/Broome.png",  x = -0.22, y = 0.38, scale = scale)+
  draw_image("out/plots/cities/Darwin.png",  x = -0.02, y = 0.38, scale = scale)+
  draw_image("out/plots/cities/Cairns.png",  x = 0.18, y = 0.38, scale = scale)+
  
  draw_image("out/plots/cities/Perth.png",  x = -0.28, y = -0.38, scale = scale)+
  draw_image("out/plots/cities/Adelaide.png",  x = -0.1, y = -0.38, scale = scale)+
  draw_image("out/plots/cities/Melbourne.png",  x = 0.08, y = -0.38, scale = scale)+
  draw_image("out/plots/cities/Hobart.png",  x = 0.26, y = -0.38, scale = scale)+
  
  draw_image("out/plots/cities/Brisbane.png",  x = 0.39, y = 0.3, scale = scale)+
  draw_image("out/plots/cities/Sydney.png",  x = 0.39, y = 0.05, scale = scale)+
  draw_image("out/plots/cities/Canberra.png",  x = 0.39, y = -0.2, scale = scale)+
  
  draw_image("out/plots/cities/stage_leg.jpeg",  x = -0.38, y = 0.39, scale = 0.14)

ggsave(blowplot, filename="out/plots/blowplot_Aus_resize.pdf",
       width=9.4, height=6.2)

#### Scattered ####

blowplot <- ggdraw() +
  draw_plot(Aus2)+
  draw_image("out/plots/cities/Perth.png",  x = -0.24, y = -0.26, scale = scale)+
  
  draw_image("out/plots/cities/Broome.png",  x = -0.2, y = 0.29, scale = scale)+
  draw_image("out/plots/cities/Darwin.png",  x = -0.05, y = 0.4, scale = scale)+
  
  draw_image("out/plots/cities/Cairns.png",  x = 0.28, y = 0.37, scale = scale)+
  draw_image("out/plots/cities/Brisbane.png",  x = 0.37, y = 0.17, scale = scale)+
  
  draw_image("out/plots/cities/Adelaide.png",  x = -0.06, y = -0.24, scale = scale)+
  
  draw_image("out/plots/cities/Melbourne.png",  x = 0.1, y = -0.37, scale = scale)+
  draw_image("out/plots/cities/Hobart.png",  x = 0.27, y = -0.4, scale = scale)+
  
  draw_image("out/plots/cities/Sydney.png",  x = 0.38, y = -0.06, scale = scale)+
  draw_image("out/plots/cities/Canberra.png",  x = 0.38, y = -0.25, scale = scale)

ggsave(blowplot, filename="out/plots/blowplot2.pdf",
       width=9.4, height=6.2)


leg.plot <- plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend = c("Juveniles", "Pre-oviposition", "Adults"), 
       col = c("grey75", "grey50", "grey25"), 
       lty = 1, lwd = 5, cex = 1.6, bty='n')
