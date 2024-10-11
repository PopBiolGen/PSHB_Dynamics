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
# Run parallel over multiple cores
library(foreach)
library(parallel)
library(doParallel)

## Load up the functions
source("src/TPCFunctions.R")
source("src/modelFunctions.R")

# Load data for temperature function (from temperature-prediction-function.R -- spartan can't load Rdata file)
merge_temp <- read.csv('src/temperatures/merge_temp.csv')
mod_fit <- lm(mean_d ~ air_tmax*rh_tmax + ma30*rh_tmax, data = merge_temp)
tree_temp_model_pars <- coef(mod_fit)

# WA map
sf_oz <- subset(ozmap("country"))

# Play around with different estimates of mu (probability of dispersal)
# mu_est <- mu_est_iter[iter]
mu_est <- 0.5 # Let's focus on just 1 mu for now...

# For now, manually select coordinate ranges to construct a retangular grid covering whole area
# SILO data resolution = 0.05 x 0.05 degrees
# Aus
#lat <- c(seq(-45, -10, by=map.res)) # Latitude range
#lon <- c(seq(110, 160, by=map.res)) # Longitude range

# Coords now listed in spartan_call

# Each spartan job (iter) corresponds to 1 lat & lon combo
lat <- unlist(lat_list[iter])
lon <- unlist(lon_list[iter])

grid <- (expand.grid(lon, lat)) # Grid containing each lat & lon combination
colnames(grid) <- c("lon", "lat")

## Need to subset only the coordinates that fall on land (ignore ocean)

grid$points <- st_as_sf(grid, coords=1:2, # Convert coords to sf object
                        crs=st_crs(sf_oz)) # Coordinate reference system

# Determine whether points are for land or ocean
grid$land <- !is.na(as.numeric(st_intersects(grid$points, sf_oz))) # Land (TRUE) when coords match with map polygon
grid <- grid[!(grid$land %in% "FALSE"),] # Remove ocean coords

# Check on map
#ggplot(data = sf_oz) +
#  geom_sf()+
#  geom_point(data=grid,
#             aes(x=lon, y=lat, col=land))+
#  scale_x_continuous(limits=c(min(lon),max(lon)))+
#  scale_y_continuous(limits=c(min(lat),max(lat)))

grid_coords <- as.matrix(grid[,-c(3,4)]) # Subset just lat & lon, convert to matrix
colnames(grid_coords)<- c("lon","lat")

outputs_grid <- matrix(0, 
                       nrow=nrow(grid_coords), 
                       ncol=6)

# Function for sim:
sim_fun <- function(locLat, locLong){
  yearSim <- run_year(lat = locLat, long = locLong, make_plot = FALSE) # Run same model as 'basic within-pop model.R'
  A_growth <- yearSim$growthRate[3] # Mean adult growth rates
  n_A_end <- yearSim$popDat[3,366] # Number of adults at end of sim
  mean_Temp <- mean(yearSim$temps) # Mean temperature at location
  tot_mu <- sum(yearSim$P_mu) # Total out-dispersing Pre-adults
  return(c(locLong, locLat, 
           A_growth, n_A_end, 
           mean_Temp, tot_mu)) # Save as vector
}

#### Create vector of outputs
# For loop (foreach not running on spartan...)
for(i in 1:nrow(grid_coords)){ 
  source("src/modelFunctions.R")
                                 locLong <- grid_coords[i,"lon"]
                                 locLat <- grid_coords[i,"lat"]
                                 
                                 tryCatch({ # Skip step if there is an error (i.e. no SILO data at location)
                                   
                                     loc_i <- sim_fun(locLat, locLong) # Run sim function for location
                                 
                                 outputs_grid[i, ] <- loc_i # Fill row with output data
                                 
                                 }, 
                                 error=function(e){
                                   #  cat("ERROR :",conditionMessage(e), "\n") # Can print error message
                                 })
                                 
                               }

colnames(outputs_grid)<- c("lon","lat", # Add lat & lon, leave remaining columns empty 
                           "A_growth", # Mean daily Adult growth rate
                           "n_A_end", # Adult population at end of sim
                           "mean_Temp", # Mean temperature at site
                           "tot_mu") # Total n dispersing P


write.csv(outputs_grid, # Save output
          file = sprintf("out/PSHB_mu_%s_sim_%s.csv", mu_est, iter), 
          col.names = T, row.names = F )

# Plot output

options(bitmapType='cairo') # To save png correctly

map.plot <- ggplot(data = sf_oz) + 
  geom_tile(data=as.data.frame(outputs_grid), # Save from matrix to dataframe
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

# Need to Save this plot
ggsave(map.plot,
       file = sprintf("out/Ausmap_mu_%s_sim_%s.png", mu_est, iter), 
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
