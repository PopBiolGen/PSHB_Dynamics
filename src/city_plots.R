
locLat <- -27.454336
locLong <- 153.018555
https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?start=20130101&finish=20231231&lat=-27.47551&lon=153.0300&format=alldata&username=andrew.coates@curtin.edu.au&password=apirequest  

wd_test <- weatherOz::get_data_drill(
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
  )

wd_test <- wd_test %>% dplyr::mutate(DOY = yday(dmy(paste(day, month, year, sep = "-")))) 

ggplot(wd_test, aes(x=DOY, y=air_tmax, col=year, group=year))+
  geom_line()+
  geom_line(data=wd_test, 
            aes(x=DOY, y=air_tmin, col=year, group=year))+
  geom_hline(yintercept = 30)+
  #facet_wrap(.~year)+
  geom_line(data=wd, aes(x=DOY, y=air_tmax), col="red")

################################


library(dplyr)
library(lubridate)
library(readr)

## Load up the functions
source("src/TPCFunctions.R")
source("src/modelFunctions.R")

# Load data for temperature function (from temperature-prediction-function.R -- spartan can't load Rdata file)
merge_temp <- read.csv('src/temperatures/merge_temp.csv')
mod_fit <- lm(mean_d ~ air_tmax*rh_tmax + ma30*rh_tmax, data = merge_temp)
tree_temp_model_pars <- coef(mod_fit)

# Assign mu parameter
mu_est <- 0

city_coords <- read_csv("src/city_coords.csv")

# Looking at plots without manual-scaled axes:
mingrow <- -0.1
maxgrow <- 0.1

# Pick a location

par(mfrow = c(3, 4))
par(mfrow = c(1, 2))
              
for(i in 7:8){

  locLat <- city_coords$lat[i]
  locLong <- city_coords$lon[i]
  city_name <- city_coords$city[i]
  
  yearSim_city <- run_year(lat = locLat, long = locLong, 
                      make_plot = FALSE)
  
  population_data <- yearSim_city$popDat
  
  # By stage
  juv_vec <- population_data[1,]
  pre_adult_vec <- population_data[2,]
  ad_vec <- population_data[3,]
  # Calculate growth rates for all life stages
  juv_growth_rate <- diff(log(juv_vec))
  pre_adult_growth_rate <- diff(log(pre_adult_vec))
  ad_growth_rate <- diff(log(ad_vec))
  # Smooth the growth rate curves using a moving average
  window_size <- 5
  juv_growth_rate_smoothed <- stats::filter(juv_growth_rate, rep(1/window_size, window_size), sides = 2)
  pre_adult_growth_rate_smoothed <- stats::filter(pre_adult_growth_rate, rep(1/window_size, window_size), sides = 2)
  ad_growth_rate_smoothed <- stats::filter(ad_growth_rate, rep(1/window_size, window_size), sides = 2)
  
  plot(juv_growth_rate_smoothed, type = "l", 
       main = city_name, 
       xlab = "Day of year", 
       ylab = "Growth rate", 
       col = "grey75", lwd = 2, 
#       ylim = c(mingrow, maxgrow),
       bty = "l")
  lines(pre_adult_growth_rate_smoothed, col = "grey50", lwd = 2)
  lines(ad_growth_rate_smoothed, col = "grey25", lwd = 2)
#  legend("bottomright", legend = c("Juveniles", "Pre-adults", "Adults"), 
 #        col = c("grey75", "grey50", "grey25"), 
  #       lty = 1, lwd = 2, cex = 0.75)
}

dev.off()


##################################

yearSim <- run_year(lat = city_coords$lat[city_coords$city=="Cairns"],
                    long = city_coords$lon[city_coords$city=="Cairns"], 
                    make_plot = TRUE)
population_data <- yearSim$popDat







View(yearSim$popDat)

population_data <- yearSim$popDat


plot_growth_rates(population_data, window_size = 5, legend_size = 0.7)

## Function to plot growth rates of all life stages over time with smoothing
plot_growth_rates <- function(population_data, window_size = 5, legend_size = 0.7) {
  # Extract population data for all life stages
  juv_vec <- population_data[1,]
  pre_adult_vec <- population_data[2,]
  ad_vec <- population_data[3,]
  
  # Calculate growth rates for all life stages
  juv_growth_rate <- diff(log(juv_vec))
  pre_adult_growth_rate <- diff(log(pre_adult_vec))
  ad_growth_rate <- diff(log(ad_vec))
  
  # Smooth the growth rate curves using a moving average
  juv_growth_rate_smoothed <- stats::filter(juv_growth_rate, rep(1/window_size, window_size), sides = 2)
  pre_adult_growth_rate_smoothed <- stats::filter(pre_adult_growth_rate, rep(1/window_size, window_size), sides = 2)
  ad_growth_rate_smoothed <- stats::filter(ad_growth_rate, rep(1/window_size, window_size), sides = 2)
  
  # Plot smoothed growth rates for all life stages
  plot(juv_growth_rate_smoothed, type = "l", main = "Growth rates over time", 
       xlab = "Day of year", ylab = "Growth rate", col = "red", lwd = 2, bty = "l")
  lines(pre_adult_growth_rate_smoothed, col = "green", lwd = 2)
  lines(ad_growth_rate_smoothed, col = "darkorange", lwd = 2)
  legend("topright", legend = c("Juveniles", "Pre-adults", "Adults"), col = c("red", "green", "darkorange"), lty = 1, lwd = 2, cex = legend_size)
}


plot(juv_growth_rate_smoothed, type = "l", 
     main = "", 
     xlab = "Day of year", 
     ylab = "Growth rate", 
     col = "grey75", lwd = 2, 
     bty = "l")
lines(pre_adult_growth_rate_smoothed, col = "grey50", lwd = 2)
lines(ad_growth_rate_smoothed, col = "grey25", lwd = 2)
legend("bottomright", legend = c("Juveniles", "Pre-adults", "Adults"), 
       col = c("grey75", "grey50", "grey25"), 
       lty = 1, lwd = 2, cex = 0.75)



# ylim = c(mingrow, maxgrow)

"red"
legend("topright", 
       legend = c("Juveniles", "Pre-adults", "Adults"), 
       col = c("red", "green", "darkorange"), 
       lty = 1, lwd = 2, cex = legend_size)


plot(juv_growth_rate_smoothed, type = "l", main = "Growth rates over time", 
     xlab = "Day of year", ylab = "Growth rate", col = "red", lwd = 2, bty = "l")

jgr <- as.data.frame(juv_growth_rate_smoothed)
colnames(jgr) <- 'gr'
View(jgr)

ggplot(data=jgr,
       aes(y=x, x=row_number(x)))+
  geom_point()
       
       
