

usethis::edit_r_environ()
# Add this to R.environ:
SILO_API_KEY=andrew.coates@curtin.edu.au


library(httpcode)
library(urltools)
install.packages("weatherOz")
# Development version:
# install.packages("weatherOz", repos = "https://ropensci.r-universe.dev",
  #               dependencies=TRUE)
library(weatherOz)



temps_all <- weatherOz::get_data_drill(
  latitude = -32.005892,
  longitude = 115.896019,
  start_date = "20130101",
  end_date = "20231231",
  values = c(
    "max_temp",
    "min_temp",
    "rh_tmax"
  ),
  api_key = Sys.getenv("SILO_API_KEY")
)

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

## Run model ##
locLat <- -31.65
locLong <- 115.95
# Run numerical sims
yearSim <- run_year(lat = locLat, long = locLong, make_plot = TRUE)

# Need to either add function that skips NAs, OR
# Input coords only including (from an existing map raster?)


library(sf)
install.packages()
library(spData) ## For `world`, an sf MULTIPOLYGON object

## Create an sf POINTS object


lat <- c(seq(-31.65, -30.6, by=.05))
lon <- c(115.9, 115.95)
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
  geom_point()

#pts <- st_as_sf(ppointspts <- st_as_sf(points, coords=1:2, crs=4326)
## Find which points fall over land
#ii <- !is.na(as.numeric(st_intersects(pts, world)))



