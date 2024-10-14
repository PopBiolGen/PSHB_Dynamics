# Need these for weatherOz to run
library(httpcode)
library(urltools)
# Plotting
library(ggplot2)
library(ozmaps)
library(sf)
library(viridis)
# From 'basic within-pop models.R'
library(dplyr)
library(lubridate)
## Load up the functions
source("src/TPCFunctions.R")
source("src/modelFunctions.R")
# Run parallel over multiple cores
library(foreach)
library(parallel)
library(doParallel)

# Load data for temperature function (from temperature-prediction-function.R -- spartan can't load Rdata file)
merge_temp <- read.csv('src/temperatures/merge_temp.csv')
mod_fit <- lm(mean_d ~ air_tmax*rh_tmax + ma30*rh_tmax, data = merge_temp)
tree_temp_model_pars <- coef(mod_fit)

# WA map
# sf_oz <- subset(ozmap("states"), NAME=="Western Australia")
sf_oz <- subset(ozmap("country")) # All Australia

map.res <- 0.1 # resolution of map (degrees) - 0.05 deg = 5 km

# Play around with different estimates of mu (probability of dispersal)
mu_est <- 0.35

# For now, manually select coordinate ranges to construct a retangular grid covering whole area
# SILO data resolution = 0.05 x 0.05 degrees
# Full Aus range:
#lat <- c(seq(-45, -10, by=map.res)) # Latitude range
#lon <- c(seq(113, 153.4, by=map.res)) # Longitude range

# Run 2 separate lat bands:
lat <- c(seq(-45, -25.1, by=map.res)) # 1
#lat <- c(seq(-25, -10, by=map.res)) # 2
# Run 5x separate long bands:
lon <- c(seq(110, 120, by=map.res)) # 1
#lon <- c(seq(120.1, 130, by=map.res)) # 2
#lon <- c(seq(130.1, 140, by=map.res)) # 3
#lon <- c(seq(140.1, 145, by=map.res)) # 4 # Lots of area 140-160
#lon <- c(seq(145.1, 160, by=map.res)) # 5

grid <- (expand.grid(lon, lat)) # Grid containing each lat & lon combination
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
             aes(x=lon, y=lat, col=land))+
  scale_x_continuous(limits=c(min(lon),max(lon)))+
  scale_y_continuous(limits=c(min(lat),max(lat)))

grid_coords <- as.matrix(grid[,-c(3,4)]) # Subset just lat & lon, convert to matrix
colnames(grid_coords)<- c("lon","lat")

### FOREACH method (run parallel over multiple cores)
n.cores <- 7 # Assign number cores 
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK",
  outfile=""
)
print(my.cluster) #check cluster definition
doParallel::registerDoParallel(cl = my.cluster) #register it to be used by %dopar%
#install.packages("doSNOW")
#install.packages("tcltk")
library(doSNOW)
library(tcltk)
registerDoSNOW(my.cluster) # Save worker outfiles (to see errors)

# Progress bar
pb <- tkProgressBar(max=nrow(grid_coords))
progress <- function(i) setTkProgressBar(pb, i)
opts <- list(progress=progress)

#### Create vector of outputs
# Similar to for loop, but runs over multiple cores then combines outputs
out_v <- foreach(i = 1:nrow(grid_coords), 
                 .combine='c', # Combine outputs into vector (can also use 'cbind' or 'rbind' to create matrix)
                 .options.snow=opts, # Progress bar
                 .packages = c("httpcode", # Need to install packages on all Worker cores
                               "urltools",
                               "ggplot2",
                               "ozmaps",
                               "sf",
                               "viridis",
                               "dplyr",
                               "lubridate",
                               "foreach",
                               "tcltk")) %dopar% {
                                 locLong <- grid_coords[i,"lon"]
                                 locLat <- grid_coords[i,"lat"]
                                 yearSim <- run_year(lat = locLat, long = locLong, make_plot = FALSE) # FROM 'basic within-pop model.R'
                                 A_growth <- yearSim$growthRate[3] # Mean adult growth rates
                                 n_A_end <- yearSim$popDat[3,366] # Number of adults at end of sim
                                 mean_Temp <- mean(yearSim$temps) # Mean temperature at location
                                 tot_mu <- sum(yearSim$P_mu)
                                 # Mean daily adult pop growth rate by season
                                 summer <- mean(diff(log(yearSim$popDat[3,c(1:60,336:366)])))
                                 autumn <- mean(diff(log(yearSim$popDat[3,61:152])))
                                 winter <- mean(diff(log(yearSim$popDat[3,153:244])))
                                 spring <- mean(diff(log(yearSim$popDat[3,245:335])))
                                 return(c(locLong, locLat, 
                                          A_growth, n_A_end, 
                                          mean_Temp, tot_mu,
                                          summer, autumn, winter, spring)) #
                               }
##########################################################################
# V2 (same as spartan)
sim_fun <- function(locLat, locLong){
  yearSim <- run_year(lat = locLat, long = locLong, make_plot = FALSE) # Run same model as 'basic within-pop model.R'
  A_growth <- yearSim$growthRate[3] # Mean adult growth rates
  n_A_end <- yearSim$popDat[3,366] # Number of adults at end of sim
  mean_Temp <- mean(yearSim$temps) # Mean temperature at location
  tot_mu <- sum(yearSim$P_mu) # Total out-dispersing Pre-adults
  # Mean daily adult pop growth rate by season
  summer <- mean(diff(log(yearSim$popDat[3,c(1:60,336:366)])))
  autumn <- mean(diff(log(yearSim$popDat[3,61:152])))
  winter <- mean(diff(log(yearSim$popDat[3,153:244])))
  spring <- mean(diff(log(yearSim$popDat[3,245:335])))
  return(c(locLong, locLat, 
           A_growth, n_A_end, 
           mean_Temp, tot_mu,
           summer, autumn, winter, spring)) # Save as vector
}

out_v <- foreach(i = 1:nrow(grid_coords), 
                 .combine='c', # Combine outputs into vector (can also use 'cbind' or 'rbind' to create matrix)
               #  .options.snow=opts, # Progress bar
                 .packages = c("httpcode", # Need to install packages on all Worker cores
                               "urltools",
                               "ggplot2",
                               "ozmaps",
                               "sf",
                               "viridis",
                               "dplyr",
                               "lubridate",
                               "foreach",
                               "tcltk")) %dopar% {
                                 locLong <- grid_coords[i,"lon"]
                                 locLat <- grid_coords[i,"lat"]
                                 
                                 tryCatch({ # Skip step if there is an error (i.e. no SILO data at location)
                                   
                                   # Check whether weather data available at location...
                                   check_SILO <- weatherOz::get_data_drill(
                                     latitude = locLat,
                                     longitude = locLong,
                                     start_date = "20130101",
                                     end_date = "20231231",
                                     values = c(
                                       "max_temp",
                                       "min_temp",
                                       "rh_tmax"
                                     ),
                                     api_key = Sys.getenv("SILO_API_KEY")
                                   )}, 
                                   error=function(e){
                                       cat("ERROR :",conditionMessage(e), "\n") # Can print error message
                                   })
                                 
                                 
                                 if(exists('check_SILO')==TRUE){ # TRUE = if data does exist in SILO database
                                   if(nrow(check_SILO) > 1){ # AND if >1 row
                                     loc_i <- sim_fun(locLat, locLong) # Run sim function for location
                                   } else{loc_i <- c(rep(NA, times=10))} # Otherwise fill with NAs
                                 } else{
                                   loc_i <- c(rep(NA, times=10)) # Otherwise fill with NAs
                                 }
                                 
                                 return(loc_i) #
                               }
#########################################################################


close(pb) # Close progress bar
outputs_grid <- matrix(out_v, 
                    nrow=nrow(grid_coords), 
                       ncol=10, # Ensure same number of cols as number of outputs
                    byrow = T)
colnames(outputs_grid)<- c("lon","lat", # Add lat & lon, leave remaining columns empty 
                           "A_growth", # Mean daily Adult growth rate
                           "n_A_end", # Adult population at end of sim
                           "mean_Temp", # Mean temperature at site
                           "tot_mu",# Total n dispersing  
                           "summer","autumn","winter","spring") # Mean daily adult pop growth per season

stopCluster(my.cluster)

write.csv(outputs_grid, # Save output
          file = "out/out_lon1_lat1_mu.35.csv", col.names = T, row.names = F )

# Plot output
map.plot <- ggplot(data = sf_oz) + 
  geom_tile(data=outputs_grid, 
            aes(x=lon, y=lat, fill=A_growth)) + # E.g. Adult growth rate
  geom_sf(fill=NA)+ # WA map
  scale_x_continuous(limits=c(min(lon)-.05,max(lon)+.05))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(lat)-.05,max(lat)+.05))+
  scale_fill_viridis(name = "Mean daily adult growth rate")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())

library(ggsave)
ggsave(map.plot,
       file = "out/map_lon1_lat1_mu.35.png", 
       #   width = 10, height = 20, dpi = 1000, units = "in", 
       device='png')


#### FOR LOOP Version ####
#outputs_grid <- matrix(0, nrow=nrow(grid), ncol=5)
#outputs_grid[c(1:nrow(outputs_grid)),c(1:2)] <- grid2 # Insert lat & lon for each coord
#colnames(outputs_grid)<- c("lon","lat","J","P","A") # Leave remaining columns to insert J, P & A growth rates

#for(i in 1:nrow(outputs_grid)){
#  locLong <- outputs_grid[i, "lon"]
#  locLat <- outputs_grid[i, "lat"]
#  yearSim <- run_year(lat = locLat, long = locLong, make_plot = FALSE) # FROM 'basic within-pop model.R'
#  rate_grid <- yearSim$growthRate # Calc mean growth rates
#  outputs_grid[i,c(3:5)] <- rate_grid # Insert growth rates into output matrix (with corresponding coords)
#}
# 
