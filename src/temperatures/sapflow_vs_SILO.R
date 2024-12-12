library(dplyr)
library(lubridate)
library(weatherOz)

## ---------------------------

# response variable -- temperature from sapflow probes
# Load some sapflow data
fList <- list.files("dat/sapflow")
sflow <- read.csv(paste0("dat/sapflow/", fList[1]), skip = 18, header = TRUE) %>%
  select(Date, Time, Max.Td.In...C., Max.Tu.In...C., Max.Td.Out...C., Max.Tu.Out...C.) %>%
  mutate(Date = dmy(Date), year = year(Date), DOY =yday(Date))

sflow$datetime <- with(sflow, ymd(sflow$Date) + hms(sflow$Time))

ggplot(data= subset(sflow, DOY >= 350 & DOY <= 360))+
  geom_line(aes(x=datetime, y=Max.Td.In...C.),
            lwd=0.75, col="red")+
  geom_line(aes(x=datetime, y=Max.Td.Out...C.),
            lwd=0.75, col="blue")+
  geom_line(aes(x=datetime, y=Max.Tu.In...C.),
            lwd=0.75, col="orange")+
  geom_line(aes(x=datetime, y=Max.Tu.Out...C.),
            lwd=0.75, col="lightblue")+
  theme(panel.background = element_blank())+
  geom_point(data= subset(wd, DOY >= 350 & DOY <= 360),
             aes(x=datetime, y=meanDaily), col="green", size=3)+
  geom_point(data= subset(wd, DOY >= 350 & DOY <= 360),
             aes(x=datetime, y=air_tmax), col="darkgreen", size=3)

sflow_day <- aggregate(Max.Td.In...C. ~ Date, FUN=mean, data=sflow)
sflow_day$datetime <- with(sflow_day, ymd(sflow_day$Date) + hms("12:00:00"))


min(sflow$datetime)
max(sflow$datetime)

### SILO data ###
# King's Park
locLat <- -31.96165
locLong <- 115.8317

wd <- weatherOz::get_data_drill(
  latitude = locLat,
  longitude = locLong,
  start_date = "20220809",
  end_date = "20230628",
  values = c(
    "max_temp",
    "min_temp",
    "rh_tmax"
  ),
  api_key = Sys.getenv("SILO_API_KEY")
)
#calculate 1 month moving average temp in lieu of soil temp at 100cm
wd <- wd %>% dplyr::mutate(DOY = yday(dmy(paste(day, month, year, sep = "-")))) %>%
  mutate(meanDaily = (air_tmax+air_tmin)/2, soil = zoo::rollmean(meanDaily, k = 30, fill = NA, align = "right"))
  
wd$datetime <- with(wd, ymd(wd$date) + hms("12:00:00"))

ggplot(data= sflow)+
  geom_line(aes(x=datetime, y=Max.Td.In...C.),
            lwd=0.1, col="red")+
  geom_line(aes(x=datetime, y=Max.Tu.In...C.),
            lwd=0.1, col="orange")+
  geom_line(data=wd, 
             aes(x=datetime, y=meanDaily),
            col="green")+
  geom_line(data=wd, 
            aes(x=datetime, y=air_tmin),
            col="lightblue")+
  geom_line(data=wd, 
            aes(x=datetime, y=air_tmax),
            col="blue")+
  geom_line(data=sflow_day, 
            aes(x=datetime, y=Max.Td.In...C.),
            col="red")+
  theme(panel.background = element_blank())




# Zoom in
sflow.1 <- subset(sflow, DOY < 15)
wd.1 <- subset(wd, DOY < 15)

times <- subset(sflow.1, Time=="00:00:00" | Time=="00:00:01")

ggplot(data= sflow.1)+
  geom_line(aes(x=datetime, y=Max.Td.In...C.),
            lwd=0.1, col="red")+
  geom_line(aes(x=datetime, y=Max.Tu.In...C.),
            lwd=0.1, col="orange")+
  geom_line(data=wd.1, 
            aes(x=datetime, y=meanDaily),
            col="green")+
  geom_line(data=wd.1, 
            aes(x=datetime, y=air_tmin),
            col="lightblue")+
  geom_line(data=wd.1, 
            aes(x=datetime, y=air_tmax),
            col="blue")+
#  geom_line(data=sflow_day, 
 #           aes(x=datetime, y=Max.Td.In...C.),
  #          col="red")+
  geom_vline(xintercept = c(times$datetime))+
  theme(panel.background = element_blank())

  
sflow_day <- aggregate(Max.Td.In...C. ~ Date, FUN=mean, data=sflow)
sflow_day$datetime <- with(sflow_day, ymd(sflow_day$Date) + hms("12:00:00"))

###### 2nd sapflow dataset ####

sflow2 <- read.csv(paste0("dat/sapflow/", fList[2]), skip = 40, header = TRUE) %>%
  select(Date, Time, Max.Td.In...C., Max.Tu.In...C., Max.Td.Out...C., Max.Tu.Out...C.) %>%
  mutate(Date = dmy(Date), year = year(Date), DOY =yday(Date))

sflow2$datetime <- with(sflow2, ymd(sflow2$Date) + hms(sflow2$Time))

ggplot(data= subset(sflow2, DOY >= 350 & DOY <= 360))+
  geom_line(aes(x=datetime, y=Max.Td.In...C.),
            lwd=0.75, col="red")+
  geom_line(aes(x=datetime, y=Max.Td.Out...C.),
            lwd=0.75, col="blue")+
  geom_line(aes(x=datetime, y=Max.Tu.In...C.),
            lwd=0.75, col="orange")+
  geom_line(aes(x=datetime, y=Max.Tu.Out...C.),
            lwd=0.75, col="lightblue")+
  theme(panel.background = element_blank())+
  geom_point(data= subset(wd2, DOY >= 350 & DOY <= 360),
             aes(x=datetime, y=meanDaily), col="green", size=3)+
  geom_point(data= subset(wd2, DOY >= 350 & DOY <= 360),
             aes(x=datetime, y=air_tmax), col="darkgreen", size=3)

sflow2_day <- aggregate(Max.Td.In...C. ~ Date, FUN=mean, data=sflow2)
sflow2_day$datetime <- with(sflow2_day, ymd(sflow2_day$Date) + hms("12:00:00"))


min(sflow2$datetime)
max(sflow2$datetime)

wd2 <- weatherOz::get_data_drill(
  latitude = locLat,
  longitude = locLong,
  start_date = "20230926",
  end_date = "20240322",
  values = c(
    "max_temp",
    "min_temp",
    "rh_tmax"
  ),
  api_key = Sys.getenv("SILO_API_KEY")
)
#calculate 1 month moving average temp in lieu of soil temp at 100cm
wd2 <- wd2 %>% dplyr::mutate(DOY = yday(dmy(paste(day, month, year, sep = "-")))) %>%
  mutate(meanDaily = (air_tmax+air_tmin)/2, soil = zoo::rollmean(meanDaily, k = 30, fill = NA, align = "right"))

wd2$datetime <- with(wd2, ymd(wd2$date) + hms("12:00:00"))

ggplot(data= sflow2)+
  geom_line(aes(x=datetime, y=Max.Td.In...C.),
            lwd=0.1, col="red")+
  geom_line(aes(x=datetime, y=Max.Tu.In...C.),
            lwd=0.1, col="orange")+
  geom_line(data=wd2, 
            aes(x=datetime, y=meanDaily),
            col="green")+
  geom_line(data=wd2, 
            aes(x=datetime, y=air_tmin),
            col="lightblue")+
  geom_line(data=wd2, 
            aes(x=datetime, y=air_tmax),
            col="blue")+
  geom_line(data=sflow2_day, 
            aes(x=datetime, y=Max.Td.In...C.),
            col="red")+
  theme(panel.background = element_blank())


#### sapflow vs Predictions ####

# From modelFunction:
tree_temp <- function(air_tmax, rh_tmax, soil, int = -0.4884, beta = 0.0349){
  logit.p <- int + beta*rh_tmax # rh predicts p
  p <- plogis(logit.p)
  mean_temp <- p*air_tmax + (1-p)*soil
}

wd$model_pred <- tree_temp(wd$air_tmax, wd$rh_tmax, 
                                   wd$soil)

wd2$model_pred <- tree_temp(wd2$air_tmax, wd2$rh_tmax, 
                           wd2$soil)


ggplot()+
  geom_line(data = sflow,
            aes(x=datetime, y=Max.Td.In...C.),
            lwd=0.1, col="red")+
  geom_line(data=wd, 
            aes(x=datetime, y=model_pred),
            col="blue")+
  theme(panel.background = element_blank())

ggplot()+
  geom_line(data = sflow2,
            aes(x=datetime, y=Max.Td.In...C.),
            lwd=0.1, col="red")+
  geom_line(data=wd2, 
            aes(x=datetime, y=model_pred),
            col="blue")+
  theme(panel.background = element_blank())


### Change temperature in day ####

sflow$time <- as.POSIXct(sflow$Time, format = "%H:%M:%S")
ggplot()+
  geom_line(data = sflow,
            aes(x=time, y=Max.Td.In...C.,
                group=Date))

sflow2$time <- as.POSIXct(sflow2$Time, format = "%H:%M:%S")
ggplot()+
  geom_line(data = sflow2,
            aes(x=time, y=Max.Td.In...C.,
                group=Date))

