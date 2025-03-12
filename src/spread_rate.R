
#### Fisher velocity ####
# r (pop growth rate)
r_perth <- 0.05307253 # King's Park
r_mu.4 <- 0.006868913

r <- r_mu.4

t <- 1 # time-step

# Using d.sigma (*see dispersal-inference)
sd <- 35.6
D <- sd^2/(2*t)
#See: https://www2.gwu.edu/~phy21bio/Reading/randomwalkBerg.pdf
#See Chat GPT:
#https://chatgpt.com/share/67b55dae-65f0-8008-abd4-9b379b866d90

vF <- 2*sqrt(r*D)
vF # m/day
vF*365/1000 # km/yr
