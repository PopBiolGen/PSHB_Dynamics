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


#### Create vector of just ADULT growth rates
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
  locLong <- outputs_grid[i,"lon"]
  locLat <- outputs_grid[i,"lat"]
  yearSim <- run_year(lat = locLat, long = locLong, make_plot = FALSE) # FROM 'basic within-pop model.R'
  rate_grid <- yearSim$growthRate # Calc mean growth rates
  return(rate_grid[3]) # For now, just use ADULT GROWTH RATE values (similar rates across all stages)
}
out_v

outputs_grid[,4]<-out_v
