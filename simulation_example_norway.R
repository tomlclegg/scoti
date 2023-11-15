#####
##### this code gives an example of how the functions can be used to simulate fishing and monitoring (here 100 independent years simulated each monitored 1000 times)
################################################################################################################################################

# source the sim functions
source("make_fishing_year_metier.r")
source("monitor_BPUE_metier.R")

# Simulate the true state of the fishery
p.metier <- 1
p.bycatch <- 0.12 # range is from 0.02 to 0.12
p.large.event <- 0.007
mean.bycatch.event <- 2 # (rounded down from 2.4)
mean.bycatch.large.event <- 17.6 # max in dataset is 36. This is the upper 25%ile
nboat <- 28
mean.fishing.event.boat.day <- 1
stochastic <- FALSE

# Simulate monitoring
pmonitor <- 1
nsample <- 100 # How many times to run - this is independent of the fishery, it's just how many draws you want to do.
p_monitor_boat <- 3/31 #  n boats that were willing to report seabirds
boat_samp <- TRUE
p_haul_obs <- 1 # should be fixed at 1 per David
detect_prob <- 0.7
refusal_rate <- 0 # ignoring refusal for now
misclassification <- 0
bymetier <- FALSE
p_monitor_metier <- 1


# Generate 'true' values for fishery (1 yr)  -----------------------------------
fishing <- make_fishing_year_metier(mean.bycatch.event = mean.bycatch.event,
                                    mean.bycatch.large.event = mean.bycatch.large.event,
                                    p.large.event = p.large.event,nboat = nboat,
                                    mean.fishing.event.boat.day = mean.fishing.event.boat.day,
                                    p.bycatch = p.bycatch, p.metier = p.metier,
                                    stochastic = stochastic
                                    )
head(fishing) # columns of output: bycatch is whether there was an event that day, nbycatch = number of individuals

BPUE_real <- sum(fishing$nbycatch) / dim(fishing)[1] # Avg bycatch per day, in this situation
cat("BPUE_real = ", BPUE_real, "\n")



# Generate observations from the 'true' state -----------------------------

obs_fishing <- monitor_BPUE_metier(pmonitor = pmonitor,
                                   nsample = nsample,
                                   BPUE_real = BPUE_real,
                                   fishing = fishing,
                                   p_monitor_boat = p_monitor_boat,
                                   boat_samp = boat_samp,
                                   p_haul_obs = p_haul_obs,
                                   detect_prob = detect_prob,
                                   refusal_rate = refusal_rate,
                                   misclassification = misclassification,
                                   bymetier = bymetier,
                                   p_monitor_metier = p_monitor_metier
                                   )
print(obs_fishing)

BPUE_bias <- (obs_fishing$BPUE_est - BPUE_real) / BPUE_real
cat("BPUE_bias = ", BPUE_bias, "\n")
print(obs_fishing)
