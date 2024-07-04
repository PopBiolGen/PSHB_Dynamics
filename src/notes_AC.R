
# Change R environment to add API key
usethis::edit_r_environ()
# Add this to R.environ:
SILO_API_KEY=andrew.coates@curtin.edu.au

# Need these for weatherOz
library(httpcode)
library(urltools)
#install.packages("weatherOz")
# Development version:
# install.packages("weatherOz", repos = "https://ropensci.r-universe.dev",
  #               dependencies=TRUE)
library(weatherOz)


############################################

# I want to pull out all the lat/lon coords in weatherOz (within certain range), save as a list/vector
# Can then run a function (for loop?) that runs model for each coord, then saves mean growith into combined file

# Option 1:
# Patched point dataset
# All stations within 500km radius from Perth Gardens station
https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?start=20130101&finish=20231231&station=9097&format=near&radius=500&comment=XNH&username=andrew.coates@curtin.edu.au  

# Option 2:
# Use gridded dataset
# Interpolated data from gridded datasets (not sure if different from Patched Point dataset, above?)
# Resolution = 0.05deg x 0.05deg (~ 5km x 5km)
https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?start=20130101&finish=20231231&lat=-32.005892&lon=115.896019&format=alldata&username=andrew.coates@curtin.edu.au&password=apirequest  

# Create list of coords fitting grid covering study area. 
# Offshore coords return NA 

# Need to either add function that skips NAs, OR
# Input coords only including (from an existing map raster?)


library(ggplot2)

lat <- c(seq(-32.3, -31.7, by=0.05))
lon <- c(seq(115.8, 116.05, by=0.05))
grid <- as.matrix(expand.grid(lon, lat))
outputs_grid <- matrix(0, nrow=nrow(grid), ncol=5)
outputs_grid[c(1:nrow(outputs_grid)),c(1:2)] <- grid
colnames(outputs_grid)<- c("lon","lat","J","P","A")

for(i in 1:nrow(outputs_grid)){
  locLong <- outputs_grid[i, "lon"]
  locLat <- outputs_grid[i, "lat"]
  yearSim <- run_year(lat = locLat, long = locLong, make_plot = FALSE)
  rate_grid <- yearSim$growthRate
  outputs_grid[i,c(3:5)] <- rate_grid
}

ggplot(outputs_grid, aes(x=lon, y=lat, col=A))+
  geom_point(size=12, pch=15)+
  scale_x_continuous(breaks=c(seq(115.8, 116.05, by=0.05)))+
  scale_y_continuous(breaks=c(seq(-32.3, -31.7, by=0.05)))


library(ozmaps)
sf_oz <- subset(ozmap("states"), NAME=="Western Australia")

ggplot(data = sf_oz) + geom_sf()+
  geom_point(data=outputs_grid,
             aes(x=lon, y=lat, col=A))+
  scale_x_continuous(limits= c(115.6, 116.1),
                     breaks=lon)+
  scale_y_continuous(limits= c(-32.4, -31.5),
                     breaks=lat)

  


#### V2
# For now, manually select range of grid:
lat <- c(seq(-32.9, -31.2, by=0.05))
lon <- c(seq(115.35, 116.4, by=0.05))
#grid <- as.matrix(expand.grid(lon, lat))
grid <- (expand.grid(lon, lat))
colnames(grid) <- c("lon", "lat")
# Check on map
ggplot(data = sf_oz) +
  geom_sf()+
  geom_point(data=grid,
             aes(x=lon, y=lat))+
scale_x_continuous(limits=c(min(lon),max(lon)))+
  scale_y_continuous(limits=c(min(lat),max(lat)))+
  theme(panel.background = element_blank())

######
# COMPARE w map raster instead of dataset...
library(sf)
library(spData) ## For `world`, an sf MULTIPOLYGON object

## Convert coords to sf object
grid$points <- st_as_sf(grid, coords=1:2, 
                crs=st_crs(sf_oz)) # Coordinate reference system...

## Find which points fall over land
grid$land <- !is.na(as.numeric(st_intersects(grid$points, sf_oz)))

grid <- grid[!(grid$land %in% "FALSE"),]

grid2 <- as.matrix(grid[,-c(3,4)])

# Check on map
ggplot(data = sf_oz) +
  geom_sf()+
  geom_point(data=grid,
             aes(x=lon, y=lat, col=land))+
  scale_x_continuous(limits=c(min(lon),max(lon)))+
  scale_y_continuous(limits=c(min(lat),max(lat)))

# Make output matrix
outputs_grid <- matrix(0, nrow=nrow(grid), ncol=5)
outputs_grid[c(1:nrow(outputs_grid)),c(1:2)] <- grid2
colnames(outputs_grid)<- c("lon","lat","J","P","A")

for(i in 1:nrow(outputs_grid)){
  locLong <- outputs_grid[i, "lon"]
  locLat <- outputs_grid[i, "lat"]
  yearSim <- run_year(lat = locLat, long = locLong, make_plot = FALSE)
  rate_grid <- yearSim$growthRate
  outputs_grid[i,c(3:5)] <- rate_grid
}

ggplot(data = sf_oz) + 
  #geom_point(data=outputs_grid,
    #         aes(x=lon, y=lat, col=A))+
  geom_tile(data=outputs_grid, aes(x=lon, y=lat, fill=A)) +
  geom_sf(fill=NA)+
  scale_x_continuous(limits=c(min(lon)-.05,max(lon)+.05))+
  scale_y_continuous(limits=c(min(lat)-.05,max(lat)+.05))+
  theme(panel.background = element_blank())

write.csv(outputs_grid, 
          file = "out/perth_grid.csv", col.names = T )


#####
# Other option:
# for each coord, pull from SILO
# If EMPTY table, add NA
for(i in 1:nrow(grid)){
  
  check_grid <- weatherOz::get_data_drill(
    latitude = grid[i, "lat"],
    longitude = grid[i, "lon"],
    start_date = "20130101",
    end_date = "20231231",
    api_key = Sys.getenv("SILO_API_KEY")
  )
  
  if(nrow(check_grid) == 0){
    grid[i,] <- NA
  }
}
grid <- na.omit(grid)
#####

