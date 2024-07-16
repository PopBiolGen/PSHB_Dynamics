
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
install.packages('readr')
library(readr)
mu_0 <- read_csv("out/mu_0.csv")
mu_0$mu <- 0
mu_0.25 <- read_csv("out/mu_0.25.csv")
mu_0.25$mu <- 0.25
mu_0.5 <- read_csv("out/mu_0.5.csv")
mu_0.5$mu <- 0.5
mu_all <- rbind(mu_0, mu_0.25, mu_0.5)

ggplot(data = sf_oz) + 
  geom_tile(data=mu_all, 
            aes(x=lon, y=lat, fill=A)) + # E.g. Adult growth rate
  geom_sf(fill=NA)+ # WA map
  scale_x_continuous(limits=c(min(mu_all$lon)-.05,
                              max(mu_all$lon)+.05))+ # Fit plot to lat & lon range
  scale_y_continuous(limits=c(min(mu_all$lat)-.05,
                              max(mu_all$lat)+.05))+
  scale_fill_viridis()+
  theme(panel.background = element_blank())+
  facet_wrap(.~ mu, ncol=3)

tapply(mu_all$A, mu_all$mu, summary)

ggplot(mu_all) +
  aes(x=mu, y=A, fill=as.factor(mu))+
         geom_boxplot()

