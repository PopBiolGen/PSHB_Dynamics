# A script to test a model for making inference form the dispersal data

## Simulate some data

# a grid of sampling points
ps <- expand.grid(list(x = seq(-130, 130, 30), y = seq(-130, 130, 30)))
# distances from the release point, at (0,0)
ps$d <- sqrt(ps$x^2 + ps$y^2)

# describes expected count at distance d according to 2D Gaussian function with some detection radius r around traps
dfun <- function(d, dens0, sigma, radius){
  # density at distance d according to 2D Gaussian function
  d.at.d <- dens0 / sqrt(2*pi*sigma^2) * exp(-(d^2/(2*sigma^2))) / (2*pi*d)
  # expected count
  d.at.d * pi * radius^2
}

# expected counts assuming nr released and sigma = s
nr <- 1000
s <- 47
r <- 10
ps$lambda <- dfun(ps$d, nr, s, r)
# observed counts
ps$obs <- rpois(nrow(ps), ps$lambda)

#### Repeat above using Owens dispersal data ####
library(readr)
library(dplyr)
library(ggplot2)

recap <- read_csv("src/owens_recap.csv")

ps <- aggregate(ntrap ~ trapID + dist, data = recap, FUN = sum)
ps <- ps[-(ps$trapID=="RP"),]
colnames(ps)[2:3] <- c('d', 'obs')

nr <- sum(subset(recap, trapID=='RP')$nfly) # total n PSHB that dispersed

# After running greta (below)...
# Check observed against predicted
ps$lambda <- dfun(ps$d, nr, # Run dfun
                  35.6, 8.2) # with values from summary(draws)
ps$pred <- rpois(nrow(ps), ps$lambda)
# Do predictions fit with observed data?
ggplot()+
  geom_point(data=ps,
             aes(x=d, y=obs), col="blue")+
  geom_point(data=ps,
             aes(x=d, y=pred), col="red")

# Predicted counts at smaller dist increments:
ps <- data.frame(d = seq(5, 130, by=1))
ps$lambda <- dfun(ps$d, nr, # Run dfun
                  35.6, 8.2) # with values from summary(draws)
ps$pred <- rpois(nrow(ps), ps$lambda)
ggplot()+
  geom_line(data=ps,
             aes(x=d, y=pred), col="red")

#### Make inference ####

library(greta)

# name data variables
d.obs <- as_data(ps$obs)
d.d <- as_data(ps$d)


# define priors
d.sigma <- uniform(0, 300) # possible dispersal sigmas
t.rad <- uniform(0, 100) # possible radii

# define model
expected.count <- dfun(d.d, nr, sigma = d.sigma, radius = t.rad)
distribution(d.obs) <- poisson(expected.count)

g.mod <- model(d.sigma, t.rad)
plot(g.mod)

# sample the posterior
draws <- mcmc(g.mod, n_samples = 1000, chains = 4)

# did it converge?
coda::gelman.diag(draws)
bayesplot::mcmc_trace(draws)

# How close did we come to input parameter values?
summary(draws)




#####

ps2 <- data.frame(d = seq(5, 130, by=1))
ps2$lambda <- dfun(ps2$d, 1000000, # Run dfun
                  35.6, 8.2) # with values from summary(draws)
ps2$pred <- rpois(nrow(ps2), ps2$lambda)


ggplot()+
  geom_line(data=ps2,
            aes(x=d, y=pred/1000000), col="red", lwd=1)+
  geom_col(data=ps,
           aes(x=d, y=obs/nr), fill="grey40", width=1)+
  scale_x_continuous(breaks=seq(0,120, by=20),
                     limits=c(10,130))+
  theme(axis.text = element_text(size=15),
        axis.title = element_blank())

