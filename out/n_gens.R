# Run 'basic within-pop model', then:
temps <- yearSim$temp
# Use thermal values for PSHB from Umeda & Paine (summarised in Dodge)
# (but can play with other values too)
# Tmin & Tmax = 13.3 & 33.1
c(min(temps), max(temps)) # King's Park av tree temps within range over whole year

# Estimated K of TSHB (from Walgama) = 373 DD
# Including pre-oviposition period = 136 DD
walgamaP <- 373
# Estimated K (Umeda & Paine) = 398 cumulative DD (not including pre-oviposition period)
# Using Walgama preoviposition period, = 534 DD
umedaJ <- 398
umedaP <- 534

K <- walgamaP

gen_time <- matrix(0, nrow=2, ncol=length(temps))
gen_time[1,1] <- temps[1]

for(i in 2:366){
  
  if (gen_time[1, i-1] < K) { # If the cumulated DD is less than K
    
    gen_time[1,i] <- gen_time[1, i-1] + temps[i] # Cumulative DD
    
    gen_time[2,i] <- 0 # Second row keeps track of number of generations
    
  } else {
    gen_time[1,i] <- temps[i]   # If K has been reached, start new generation
    
    gen_time[2,i] <- 1 # Mark 1 for new generation
  }
  
}

plot(t(gen_time)[,1], type="l")

n_gens <- rowSums(gen_time)[2] + # Count number of generations
  gen_time[1,366]/K # Plus fraction of K at end of year
n_gens

sum(temps) # Total accumulated DD
