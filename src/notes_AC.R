
# Change R environment to add API key
#install.packages('usethis')
library(usethis)
usethis::edit_r_environ()
# Add this to R.environ:
SILO_API_KEY=andrew.coates@curtin.edu.au

#install.packages("weatherOz")
# Development version:
install.packages("weatherOz", repos = "https://ropensci.r-universe.dev",
                 dependencies=TRUE)
library(weatherOz)
install.packages(c('data.table','terra','apsimx','zoo'))

# I want to pull out all the lat/lon coords in weatherOz (within certain range), save as a list/vector
# Patched point dataset
# All stations within 500km radius from Perth Gardens station
https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?start=20130101&finish=20231231&station=9097&format=near&radius=500&comment=XNH&username=andrew.coates@curtin.edu.au  

# Use gridded dataset
# Interpolated data from gridded datasets (not sure if different from Patched Point dataset, above?)
# Resolution = 0.05deg x 0.05deg (~ 5km x 5km)
https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?start=20130101&finish=20231231&lat=-32.005892&lon=115.896019&format=alldata&username=andrew.coates@curtin.edu.au&password=apirequest  

####################################################
