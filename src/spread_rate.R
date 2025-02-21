
#####
swwa <- read.csv('out/files/WA/WA.hi.res_0_sim_1.csv')

r_max <- max(swwa$A_growth)
r_perth <- 0.05307253 # from basic model script
r_mu.4 <- 0.006868913

r <- r_mu.4

t <- 1

md_field <- 32 # mean dist from field trial
md_mill <- 106

# From Owens data (looking at recaptures only)
dat_mean <- 28
dat_ad_mean <- 22
dat_median <- 17

md <- dat_mean

D <- md^2/(2*t)

# OR using d.sigma (*see inference model)
sd <- 35.6
D <- sd^2/(2*t)
#See: https://www2.gwu.edu/~phy21bio/Reading/randomwalkBerg.pdf

#See Chat GPT:
#https://chatgpt.com/share/67b55dae-65f0-8008-abd4-9b379b866d90



vF <- 2*sqrt(r*D)
vF
vF*365/1000

# Mean from Owens data (disperers only):
# 9.45 = 3.45 km/yr
# Median:
# 5.7 = 2 km/year
# Mean from Owens data (including non-dispersers (adjusted by detection prob))
# 7.4 = 2.7 km/yr

# r = 0.059 (max west coast WA)
# field data: vF = 11 (m/day? = 4 km/y)

# r = 0.053 (Kings Park)
# field data: vF = 10.4 (m/day? = 3.7 km/y)

# r = 0.006868913 (Perth, mu=0.4)
# vF = 3.75 (= 1.4 km/y)

# r_max
# mill data: vF = 36 (m/day? = 13.2 km/y)

######## notes ########
library(ggplot2)
library(readr)
disp <- read_csv("disp_owens.csv")

ggplot(subset(disp, cat!= "pc_release"),
       aes(x=max_dist, y=pc, fill=lure))+
  geom_col(position=position_dodge())+
  facet_grid(.~cat, 
             scales = 'free')




field <- data.frame(subset(disp, cat=="pc_recap" & lure=="total")[,c("max_dist","pc")])

field$min_dist <- c(0, 20, 40, 60, 80, 100)

mean_max <- mean(rep(c(20,40,60,80,100,140), times=c(field$pc*1000)))
sd_max <- sd(rep(c(20,40,60,80,100,140), times=c(field$pc*1000)))

mean_min <- mean(rep(c(field$min_dist), times=c(field$pc*1000)))
sd_min <- sd(rep(c(field$min_dist), times=c(field$pc*1000)))

ndist <- data.frame(rep(c(field$max_dist), times=c(ceiling(field$pc))))
colnames(ndist) <- 'dist'
mean(ndist$dist)
sd(ndist$dist)


ndist2 <- data.frame(c(rep(c(field$max_dist), times=c(field$pc)),
                     rep(c(field$max_dist*-1), times=c(field$pc))))
colnames(ndist2) <- 'dist'

mean2 <- mean(ndist2$dist)
sd2 <- sd(ndist2$dist)


ggplot(ndist2) + 
#  geom_point(data=field,
 #          aes(x=min_dist, y=pc/100))+
  geom_histogram(aes(x = dist, y=..density..)) +
  stat_function(fun = dtruncnorm, args = list(mean = mean2, sd = sd2))


norm_min <- rnorm(mean = mean_min, sd = sd_min, n = 100)
hist(norm_min)

install.packages('truncnorm')
library(truncnorm)
r <- rtruncnorm(100, a=0, b=140, mean=mean_min, sd=sd_min)
r <- data.frame(r)
colnames(r)<-'rdist'
r

# For each distance category, get runif distribution of values...
rndist <- c(runif(ceiling(field$pc[1]), min=0, max=19),
runif(ceiling(field$pc[2]), min=20, max=39),
runif(ceiling(field$pc[3]), min=40, max=59),
runif(ceiling(field$pc[4]), min=60, max=79),
runif(ceiling(field$pc[5]), min=80, max=99),
runif(ceiling(field$pc[6]), min=100, max=140))

rndist <- data.frame(rndist)
colnames(rndist) <- 'rndist'

ggplot(rndist)+
  geom_histogram(aes(x=rndist), binwidth=10)

meanrn <- mean(rndist$rndist)
sdrn <- sd(rndist$rndist)

r <- rtruncnorm(100, a=0, b=140, mean=meanrn*0, sd=sdrn)
r <- data.frame(r)
colnames(r)<-'rdist'


ggplot() + 
  geom_histogram(data = r,
               aes(x = rdist, y=..density..), fill="red", alpha=0.5)+
  geom_histogram(data = rndist,
               aes(x = rndist, y=..density..), fill="blue", alpha=0.5)+
  geom_density(data = r,
                 aes(x = rdist, y=..density..), fill="red", alpha=0.5)+
  geom_density(data = rndist,
                 aes(x = rndist, y=..density..), fill="blue", alpha=0.5)
#  geom_histogram(data = ndist,
 #                aes(x = dist, y=..density..), fill="grey", alpha=0.5)


swwa <- read.csv('out/files/WA/WA.hi.res_0_sim_1.csv')

r_max <- max(swwa$A_growth)
r_perth <- 0.05307253 # from basic model script

r <- r_max

D <- meanrn^2/(2*1)
vF <- 2*sqrt(r*D)
vF # 8 m/day
