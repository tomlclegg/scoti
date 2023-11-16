p_monitor_boat <- 2:31
nsample <- 1000
pmonitor <- 0.5 # Observers work same as crew shifts (on/off over 24 hours)
p_monitor_metier<- 1
bymetier <- FALSE
boat_samp <- TRUE
refusal_rate <- seq(0.1, 0.9, length.out = 4) # Impossible to define as sensitive to too many factors
p_haul_obs <- 0.95 # dedicated programme has lower chance of missing observations, set to small, insignificant chance
detect_prob <- 1
misclassification <- 0
vessel.effect <- 0.7
