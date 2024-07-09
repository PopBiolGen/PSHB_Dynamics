library(parallel)
library(foreach)
library(doParallel)

n.cores <- 5
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
#check cluster definition
print(my.cluster)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()
#how many workers are available?
foreach::getDoParWorkers()


#### Try vector of just ADULT growth rates
out_v <- foreach(i = 1:nrow(outputs_grid), .combine='c',
                 .packages = c("httpcode", # Need to install packages in Workers
                   "urltools",
                   "ggplot2",
                   "ozmaps",
                   "sf",
                   "viridis",
                   "dplyr",
                   "lubridate",
                   "foreach")) %dopar% {
  locLong <- lon[i]
  locLat <- lat[i]
  yearSim <- run_year(lat = locLat, long = locLong, make_plot = FALSE) # FROM 'basic within-pop model.R'
  rate_grid <- yearSim$growthRate # Calc mean growth rates
  rate_grid[3] # Insert growth rates into output matrix (with corresponding coords)
}

