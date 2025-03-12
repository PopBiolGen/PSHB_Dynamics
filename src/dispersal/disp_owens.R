library(readr)
library(ggplot2)
library(dplyr)

recap <- read_csv("src/owens_recap.csv")
#recap[is.na(recap)] <- 0
#recap$prop.fly <- recap$nfly / recap$nmark 
#recap$prop.recap <- recap$ntotalrecap / recap$nfly

# Adjust count of non-fliers by recap proportion (i.e. assume that prob of detecting PSHB at dist=0 is same as detecting at dist>0)
#recap$ntrap_ad <- recap$ntrap
#recap$ntrap_ad <- ifelse(recap$trapID=="RP",
#                         recap$ntrap * recap$prop.recap,
 #                        recap$ntrap)
#write.csv(recap, file="src/owens_recap.csv")

# Total n at each distance
recap_sum <- aggregate(ntrap ~ dist, data = recap, FUN = sum)

ggplot(recap_sum,
       aes(x=dist, y=ntrap))+
         geom_col()

ggplot(subset(recap, trapID != "RP"),
       aes(x=dist, y=ntrap))+
  geom_col(data=subset(recap_sum, dist != 0), 
           aes(x=dist, y=ntrap))+
  geom_point()+
  scale_x_continuous(breaks=seq(15,125,5))

#ggplot(recap,
#       aes(x=dist, y=ntrap))+
#  geom_point()+
#  scale_x_continuous(breaks=seq(0,125,5))


# Remove non-dispersers
disp_only <- subset(recap_sum, dist != 0)
ggplot(subset(disp_only),
       aes(x=dist, y=ntrap))+
  geom_col()



# Proportion recaptured at each dist
recap_recap <- subset(recap, trapID != "RP")
recap_recap$prop.trap <- recap_recap$ntrap / recap_recap$ntotalrecap       

ggplot(recap_recap,
       aes(x=dist, y=prop.trap))+
  geom_point()

sum_disp <- subset(recap_sum, dist != 0)


# Data point for every individual beetle recaptured
ind_dist <- c(rep(sum_disp$dist, times=sum_disp$ntrap))
#View(as.matrix(ind_dist))
summary(ind_dist)

# Histogram
n_dist <- data.frame(dist = ind_dist)
ggplot(n_dist,
       aes(x=dist))+
  geom_histogram(binwidth=5)+
  scale_x_continuous(breaks=seq(15,125,5))



# Prop disperse // prop recapture
no_disp <- subset(recap, trapID == "RP")
summary(no_disp$prop.fly)
summary(no_disp$prop.recap)


recap_ad_sum <- aggregate(ntrap_ad ~ dist, data = recap, FUN = sum)

ggplot(recap_ad_sum,
       aes(x=dist, y=ntrap_ad))+
  geom_col()

recap_ad_sum$ntrap_ad <- round(recap_ad_sum$ntrap_ad)
ind_dist_ad <- c(rep(recap_ad_sum$dist, times=recap_ad_sum$ntrap_ad))
#View(as.matrix(ind_dist))
summary(ind_dist_ad)

# Histogram
n_dist_ad <- data.frame(dist = ind_dist_ad)
ggplot(n_dist_ad,
       aes(x=dist))+
  geom_histogram(binwidth=5)+
  scale_x_continuous(breaks=seq(0,125,5))

ggplot(n_dist_ad,
       aes(x=dist))+
  geom_histogram(binwidth=5)+
  scale_x_continuous(breaks=seq(0,125,5))


#### OTHER DATA SET ####

recap <- read_csv("src/owens_recap.2.csv")

ggplot(recap,
       aes(x=dist, y=ntrap))+
  geom_point()+
  scale_x_continuous(breaks=seq(15,125,5))

recap_sum <- aggregate(ntrap ~ dist, data = recap, FUN = sum)

ggplot(recap_sum,
       aes(x=dist, y=ntrap))+
  geom_col()

ind_dist <- c(rep(recap_sum$dist, times=recap_sum$ntrap))
#View(as.matrix(ind_dist))
summary(ind_dist)

# Histogram
n_dist <- data.frame(dist = ind_dist)
ggplot(n_dist,
       aes(x=dist))+
  geom_histogram(binwidth=5)+
  scale_x_continuous(breaks=seq(15,125,5))
