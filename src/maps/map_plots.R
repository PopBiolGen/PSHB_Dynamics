library(ozmaps)
library(sf)
library(viridis)
library(ggplot2)
library(ggpubr)
library(readr)
library(dplyr)
library(ggnewscale)
library(glue)
sf_oz <- subset(ozmap("country")) # All Australia
#sf_oz <- subset(ozmap("states"))


### Merge separated regions into 1 output file ####
## mu = 0
filenames <- list.files("out/files/mu_0", # FOLDER NAME
                        pattern="*.csv", full.names=TRUE)
# Combine into a single df
outAus <- filenames %>% 
  purrr::map_dfr(readr::read_csv)
outAus <- outAus[-(which(rowSums(outAus)==0 )),] # Remove empty rows (missing values I think, but also means lat & lon = 0)
outAus <- outAus[-(which(is.na(outAus$A_growth))),] # Remove NAs (after filling in missing values with missing_values.R)
write.csv(outAus,
          file = "out/files/mu_0/Aus_mu0.csv", col.names = T, row.names = F )

## mu = 0.2
filenames <- list.files("out/files/mu_0.2", # FOLDER NAME
                        pattern="*.csv", full.names=TRUE)
# Combine into a single df
outAus <- filenames %>% 
  purrr::map_dfr(readr::read_csv)
outAus <- outAus[-(which(rowSums(outAus)==0 )),] # Remove empty rows (missing values I think, but also means lat & lon = 0)
# Rerun model to get missing values:
source("out/files/missing_values.R")
# AFTER filling in missing values:
outAus <- outAus[-(which(is.na(outAus$A_growth))),] # Remove NAs (after filling in missing values with missing_values.R)
write.csv(outAus,
          file = "out/files/mu_0.4/Aus_mu0.4.csv", col.names = T, row.names = F )

## mu = 0.4
mu_est <- 0.4
map.res <- 0.1 
filenames <- list.files("out/files/mu_0.4", # FOLDER NAME
                        pattern="*.csv", full.names=TRUE)
# Combine into a single df
outAus <- filenames %>% 
  purrr::map_dfr(readr::read_csv)
outAus <- outAus[-(which(rowSums(outAus)==0 )),] # Remove empty rows (missing values I think, but also means lat & lon = 0)
# AFTER filling in missing values:
outAus <- outAus[-(which(is.na(outAus$A_growth))),] # Remove NAs (after filling in missing values with missing_values.R)
write.csv(outAus,
          file = "out/files/mu_0.2/Aus_mu0.2.csv", col.names = T, row.names = F )

##### Map  ####
mu0 <- read.csv("out/files/mu_0/Aus_mu0.csv")
mu0.2 <- read.csv("out/files/mu_0.2/Aus_mu0.2.csv")
mu0.4 <- read.csv("out/files/mu_0.4/Aus_mu0.4.csv")
mu0$mu <- 0
mu0.2$mu <- 0.2
mu0.4$mu <- 0.4

aus <- rbind(mu0, mu0.2, mu0.4)

cities <- read_csv("src/city_coords.csv")

plot_theme <- theme(panel.background = element_blank(),
                         axis.line = element_blank(), 
                         axis.text = element_blank(), 
                         axis.ticks = element_blank(), 
                         axis.title = element_blank(),
                         plot.title = element_text(size=10))

#### Australia mu = 0 ####
Aus_mu0 <- ggplot(data = sf_oz) + 
  geom_tile(data = subset(aus, mu==0),
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\ngrowth rate (adults)\n",
                     option= "inferno",
                     limits=c(-0.026, 
                              0.076),
                     breaks=c(seq(-0.025, 0.075, by=0.025)),
                     labels=c(seq(-0.025, 0.075, by=0.025)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(mu0$lon)-0.1,
                              max(mu0$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0$lat)-0.1,
                              max(mu0$lat)+0.1))+
  geom_point(data=cities, aes(x=lon, y=lat),
             size=2, pch=21, stroke=1, fill="white")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))
Aus_mu0

#### Compare mu #####

min(aus$A_growth) # -0.03
max(aus$A_growth) # 0.075

# Positive only

ggplot(data = sf_oz)+ # subset(ozmap("states"))) + 
  
  geom_sf(fill= "grey90")+ #, col="black")+ 
  
  geom_tile(data = subset(aus, A_growth >= 0), #
            aes(x=lon, y=lat, fill=A_growth)) +
  
  geom_sf(fill= NA, col="black", lwd=0.3)+ 
  
  facet_grid(.~ glue('mu*" = {mu}"'),
             labeller = label_parsed) +
  scale_fill_viridis(name = "Mean daily population\ngrowth rate (adults)\n",
                     option= "inferno",
                     limits=c(0, #-0.03, 
                              0.08),
                     breaks=c(seq(0, 0.08, by=0.02)),
                     labels=c(seq(0, 0.08, by=0.02)))+
  
#  scale_fill_gradientn(name="Mean daily population\ngrowth rate (adults)\n", #
#                       limits=c(-0.03, 
#                                0.075),
#                       colors = c("white","#FFFF66","#CC0000"), # Set a gradient along these colours (blue to red)
#                       values =   c(scales::rescale(c(-0.03,0,0.075))),
#                       breaks=c(seq(-0.02,0.08, by=0.02)))+
  
  scale_x_continuous(limits=c(min(mu0$lon)-0.1,
                              max(mu0$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0$lat)-0.1,
                              max(mu0$lat)+0.1))+
  geom_point(data=cities, aes(x=lon, y=lat),
             size=1.2, pch=21, stroke=1, fill="white")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        strip.text = element_text(size=16),
        strip.background = element_rect(fill=NA),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))


#### mu = 0 Positive growth only ####

Aus_mu0_pos <- ggplot(data = sf_oz) + 
  geom_tile(data = mu0,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult growth rate\n",
                     limits=c(0, 
                              0.076),
                     breaks=c(seq(0, 0.075, by=0.025)),
                     labels=c(seq(0, 0.075, by=0.025)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(outAus$lon)-0.1,
                              max(outAus$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(outAus$lat)-0.1,
                              max(outAus$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())

Aus_mu0_pos

###### Zoom in sw WA ####

swwa <- read.csv('out/files/WA/WA.hi.res_0_sim_1.csv')

QZ <- read_csv("src/QZdf.csv")

#min(swwa$A_growth)
#max(swwa$A_growth)
# Fremantle Port
#-32.045226, 115.738609

# Just mu = 0
ggplot(data = sf_oz) + 
  geom_tile(data = swwa,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult\ngrowth rate\n",
                     option = "inferno",
                     limits=c(0.02,
                              0.06),
                     breaks=c(seq(0.02, 0.06, by=0.01)),
                     labels=c(seq(0.02, 0.06, by=0.01)))+
  geom_polygon(data = QZ,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
               aes(x=Longitude, y=Latitude),
               fill=NA, col="blue", 
               lwd=0.75, lty=1) +
  geom_sf(fill=NA, lwd=0.75, col="black")+ 
  coord_sf(xlim = c(115,
                    117.5),
           ylim = c(-34.5,
                    -30))+
  # Point at Port Fremantle
  #  geom_text(aes(x=115.738609, y=-32.045226), 
  #           label = "Port Fremantle",
  #          hjust=1.01, vjust = 1)+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        strip.text = element_text(size=14),
        strip.background = element_rect(fill=NA),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

# QUARANTINE ZONE 
# Quarantine Zone
# https://www.agric.wa.gov.au/borer#:~:text=in%20your%20browser.-,Quarantine%20Area%20(QA),across%2030%20local%20government%20areas.


#### WA compare mu ####
swwa <- read.csv('out/files/WA/WA.hi.res_0_sim_1.csv')
swwa$mu = 0
swwa.2 <- read.csv('out/files/WA/WA.hi.res_0.2_sim_1.csv')
swwa.2 <- swwa.2[,c(1:6)]
swwa.2$mu = 0.2
swwa.4 <- read.csv('out/files/WA/WA.hi.res_0.4_sim_1.csv')
swwa.4$mu = 0.4

wa <- rbind(swwa, swwa.2, swwa.4)

min(swwa.4$A_growth) # -.0075
max(swwa$A_growth) # .059

# POS
ggplot(data = sf_oz) + 
  geom_tile(data = wa,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult\ngrowth rate\n",
                     option = "inferno",
                     limits=c(0,
                              0.06),
                     breaks=c(seq(0, 0.06, by=0.02)),
                     labels=c(seq(0, 0.06, by=0.02)))+
  geom_sf(fill=NA, lwd=0.75, col="black")+ 
  coord_sf(xlim = c(115,
                    117.5),
           ylim = c(-34.5,
                    -30))+
  geom_polygon(data = QZ,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
               aes(x=Longitude, y=Latitude),
               fill=NA, col="red", 
               lwd=0.6, lty=1) +
  # Point at Port Fremantle
  geom_segment(aes(x = 115.2, y = -32.11, xend = 115.72, yend = -32.045226),
               arrow = arrow(length = unit(0.2, "cm")))+
  facet_grid(.~ glue('mu*" = {mu}"'),
             labeller = label_parsed)+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        strip.text = element_text(size=14),
        strip.background = element_rect(fill=NA),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

ggplot(data = sf_oz) + 
  geom_tile(data = wa,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily adult\ngrowth rate\n",
                     option = "inferno",
                     limits=c(0,
                              0.06),
                     breaks=c(seq(0, 0.06, by=0.02)),
                     labels=c(seq(0, 0.06, by=0.02)))+
  geom_sf(fill=NA, lwd=0.75, col="black")+ 
  coord_sf(xlim = c(115,
                    118.5),
           ylim = c(-34.9,
                    -30))+
  geom_polygon(data = QZ,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
               aes(x=Longitude, y=Latitude),
               fill=NA, col="red", 
               lwd=0.6, lty=1) +
  # Point at Port Fremantle
  geom_segment(aes(x = 115.2, y = -32.11, xend = 115.72, yend = -32.045226),
               arrow = arrow(length = unit(0.2, "cm")))+
  facet_grid(.~ glue('mu*" = {mu}"'),
             labeller = label_parsed)+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        strip.text = element_text(size=14),
        strip.background = element_rect(fill=NA),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))


#### WA Seasons ####
seasons <- read_csv("out/files/WA/WA_seasons_0_sim_1.csv")

seasons <- melt(seasons, # 
                  id.vars = colnames(select(seasons, -c(growth_sum, growth_aut, growth_win, growth_spr))))
colnames(seasons)[c(7,8)] <- c("season","growth")

min(seasons$growth)
max(seasons$growth)

ggplot(data = sf_oz) + 
  geom_sf(fill=NA)+ 
  geom_tile(data = seasons,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=growth)) +
  scale_fill_viridis(name = "Mean daily adult growth rate\n")+
  coord_sf(xlim = c(115,
                    117.5),
           ylim = c(-33.6,
                    -30.5))+
  facet_wrap(.~ season)+
  #  scale_x_continuous(limits=c(115,
  #                             117))+ # Fit plot to lat & lon range
  # scale_y_continuous(limits=c(-33.5,
  #                            -30.5))+
  geom_polygon(data = QZ,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
               aes(x=Longitude, y=Latitude),
               fill=NA, col="red", lwd=0.4, lty=1) +
  # Point at Port Fremantle
  geom_point(aes(x=115.738609, y=-32.045226), 
             size=2)+
  geom_text(aes(x=115.738609, y=-32.045226), 
            label = "Port Fremantle",
            hjust=1.01, vjust = 1)+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))



#### Aus diff col for neg growth ####

mu_plot <- function(df, title){
ggplot(data = sf_oz) + 
  geom_tile(data = df,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\n growth rate (adults)\n",
                     limits = c(0, maxgrow))+
                   #  option="inferno")+
  new_scale_fill() +
  geom_tile(data=subset(df, A_growth<=0),  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_gradientn(name = "Mean daily population\n growth rate (adults)\n",
                       colours = c("white", "grey30"),#"red"),
                       limits=c(mingrow, 0))+
  
  geom_sf(fill=NA)+ 
  ggtitle(title)+
  scale_x_continuous(limits=c(minlon-0.1,
                              maxlon+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(minlat-0.1,
                              maxlat+0.1))+
  plot_theme_grid
}

plot_mu0_pos <- mu_plot(mu0, "A)")
plot_mu0.2_pos <- mu_plot(mu0.2, "B)")
plot_mu0.4_pos <- mu_plot(mu0.4, "C)")

plot_leg <- ggplot(data = sf_oz) + 
  geom_tile(data=mu0,  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_viridis(name = "Mean daily population\n growth rate (adults)\n",
                     limits = c(0, maxgrow))+
  
  new_scale_fill() +
  geom_tile(data=subset(mu0, A_growth<=0),  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=A_growth)) +
  scale_fill_gradientn(name = NULL,
                       colours = c("white", "red"),
                       limits=c(mingrow, 0))+
  theme(legend.title = element_text(size=13),
        legend.text = element_text(size=10))
legend <- as_ggplot(get_plot_component(plot_leg, 
                                       'guide-box-right', return_all = TRUE))
ggarrange(plot_mu0_pos, plot_mu0.2_pos, plot_mu0.4_pos, legend, ncol=4)


######
#### Temp maps ####

Aus <- read.csv("out/files/mu_0/Aus_mu0.csv")
Cali <- read.csv("out/files/Cali_0_sim_1.csv")
SA <- read.csv("out/files/Sth_Africa_0_sim_1.csv")

cities <- read_csv("src/city_coords.csv")
Aus_temp <- ggplot(data = sf_oz) + 
  geom_tile(data = Aus,
            aes(x=lon, y=lat, fill=mean_Temp)) +
  scale_fill_viridis(name = expression(paste("Average tree temperature (",degree,"C)")),
                     option= "inferno",
                     limits=c(8, 
                              35),
                     breaks=c(seq(10, 35, by=5)),
                     labels=c(seq(10, 35, by=5)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(mu0$lon)-0.1,
                              max(mu0$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0$lat)-0.1,
                              max(mu0$lat)+0.1))+
  geom_point(data=cities, aes(x=lon, y=lat),
             size=1.8, pch=21, stroke=1, fill="white")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14),
        legend.position = "none")
Aus_temp

## US
mapdata <- map_data(map='state', region="california")
cities <- read_csv("src/known_PSHB_coords.csv")
cities <- subset(cities, country == "USA")
cities$city <- factor(cities$city, levels=c("Santa Paula", "San Marino", "Laguna Beach"))

us_temp <- ggplot(data = mapdata) + 
  coord_map(xlim = c(min(Cali$lon)-.05, 
                     max(Cali$lon)+.05), 
            ylim = c(min(Cali$lat)-.05, 
                     max(Cali$lat)+.05))+
  geom_tile(data=Cali, # Save from matrix to dataframe
            aes(x=lon, y=lat, fill=mean_Temp)) + # E.g. Adult growth rate
  geom_polygon(data = mapdata,
               aes(x = long, y = lat, group=group),
               col = "black", fill=NA) +
  scale_fill_viridis(name = expression(paste("Average tree temperature (",degree,"C)")),
                     option= "inferno",
                     limits=c(8, 
                              35),
                     breaks=c(seq(10, 35, by=5)),
                     labels=c(seq(10, 35, by=5)))+
  geom_point(data=cities, aes(x=lon, y=lat),
             size=1.8, pch=21, stroke=1, fill="white")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14),
        legend.position="none")
us_temp

# South Africa
mapdata <- map_data(map='world', region="South Africa")
cities <- read_csv("src/known_PSHB_coords.csv")
cities <- subset(cities, country == "South Africa")

sa_temp <- ggplot(data = SA) + 
  coord_map(xlim = c(min(SA$lon)-1.5, 
                     max(SA$lon)+2.2), 
            ylim = c(min(SA$lat)-1.5, 
                     max(SA$lat)+1.5))+
  geom_tile(data=SA, # Save from matrix to dataframe
            aes(x=lon, y=lat, fill=mean_Temp)) + # E.g. Adult growth rate
  geom_polygon(data = mapdata,
               aes(x = long, y = lat, group=group),
               col = "black", fill=NA) +
  scale_fill_viridis(name = expression(paste("Average tree temperature (",degree,"C)")),
                     option= "inferno",
                     limits=c(8, 
                              35),
                     breaks=c(seq(10, 35, by=5)),
                     labels=c(seq(10, 35, by=5)))+
  
  geom_point(data=cities, aes(x=lon, y=lat),
             size=1.8, pch=21, stroke=1, fill="white")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=16),
        legend.position = "none")
sa_temp

library(cowplot)
plot_leg <- ggplot(data = sf_oz) + 
  geom_tile(data = Aus,
            aes(x=lon, y=lat, fill=mean_Temp)) +
  scale_fill_viridis(name = expression(paste("Average tree temperature (",degree,"C)")),
                     option= "inferno",
                     limits=c(8, 
                              35),
                     breaks=c(seq(10, 35, by=5)),
                     labels=c(seq(10, 35, by=5)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(mu0$lon)-0.1,
                              max(mu0$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu0$lat)-0.1,
                              max(mu0$lat)+0.1))+
  geom_point(data=cities, aes(x=lon, y=lat),
             size=1.8, pch=21, stroke=1, fill="white")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))

legend <- as_ggplot(get_plot_component(plot_leg, 
                                       'guide-box-right', return_all = TRUE))

temp_plots <- ggarrange(Aus_temp, us_temp, sa_temp, legend,
             nrow=1)
temp_plots

################
#### Map total number of dispersing PA ####

plot_disp <- ggplot(data = sf_oz) + 
  geom_tile(data = mu0.2, 
            aes(x=lon, y=lat, 
                fill=tot_mu)) +
  
  scale_fill_viridis(name = "Total dispersers per year",
                     trans = "log10",
                     limits=c(1, max(mu0.2$tot_mu)))+
  geom_sf(fill=NA)+ 
  scale_x_continuous(limits=c(min(aus$lon)-0.1,
                              max(aus$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(aus$lat)-0.1,
                              max(aus$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key.size = unit(0.8, 'cm'),
        legend.title = element_text(size=14))
  
plot_disp


#### Plot final adult population (>1 only) ####
ggplot(data = sf_oz) + 
  geom_tile(data = subset(mu0.2, n_A_end > 1),  # Bit of a rough way to do it, but plot all points so colour gradient is the same...
            aes(x=lon, y=lat, fill=n_A_end)) +
  scale_fill_viridis(name = "Mean daily adult growth rate")+
  geom_sf(fill=NA)+ # WA map
  scale_x_continuous(limits=c(min(outAus$lon)-0.1,
                              max(outAus$lon)+0.1))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(outAus$lat)-0.1,
                              max(outAus$lat)+0.1))+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())

#### Distribution of growth rates: ####
ggplot(subset(aus, mu==0.2), aes(x=A_growth))+
  geom_histogram()


#### DATA BY SEASON ####

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

#### Animation: ####
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
