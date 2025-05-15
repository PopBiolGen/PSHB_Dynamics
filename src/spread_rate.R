## ---------------------------
##
## Script name: spread_rate.R
##
## Purpose of script: Estimate spread rate of PSHB, according to diffusion model (Fisher's velocity)
## 
## ---------------------------

# r (pop growth rate)
r_perth <- 0.05307253 # King's Park, Perth, at mu = 0
r_mu.4 <- 0.006868913 # mu = 0.4
r <- r_perth
t <- 1 # time-step
sd <- 25.7 # SD = sigma in 2D normal curve (see dispersal-inference.R)

D <- sd^2/(2*t) # Calc diffusion coeff
vF <- 2*sqrt(r*D) # Calc fisher velocity
vF*365/1000 # km/yr
# 3.06 km/yr
