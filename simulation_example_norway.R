#####
##### this code gives an example of how the functions can be used to simulate fishing and monitoring (here 100 independent years simulated each monitored 1000 times)
################################################################################################################################################

# source the sim functions
source("make_fishing_year_metier.r")
source("monitor_BPUE_metier.R")
source("make_fishing_year_metier_space.r")

# Libraries
library(dplyr)
library(ggplot2)
library(wesanderson)

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
p_monitor_boat <- 3 / 31 #  n boats that were willing to report seabirds
boat_samp <- TRUE
p_haul_obs <- 1 # should be fixed at 1 per David
detect_prob <- 0.7
refusal_rate <- 0 # ignoring refusal for now
misclassification <- 0
bymetier <- FALSE
p_monitor_metier <- 1


# Generate 'true' values for fishery (1 yr)  -----------------------------------
fishing <- make_fishing_year_metier(
  mean.bycatch.event = mean.bycatch.event,
  mean.bycatch.large.event = mean.bycatch.large.event,
  p.large.event = p.large.event, nboat = nboat,
  mean.fishing.event.boat.day = mean.fishing.event.boat.day,
  p.bycatch = p.bycatch, p.metier = p.metier,
  stochastic = stochastic, vessel.effect = 0.7
)
head(fishing) # columns of output: bycatch is whether there was an event that day, nbycatch = number of individuals

BPUE_real <- sum(fishing$nbycatch) / dim(fishing)[1] # Avg bycatch per day, in this situation
cat("BPUE_real = ", BPUE_real, "\n")



# Generate observations from the 'true' state -----------------------------

obs_fishing <- monitor_BPUE_metier(
  pmonitor = pmonitor,
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


# Test 'spatial' function -------------------------------------------------
source("make_fishing_year_metier_space.R")
# Added params for when you're using the spatial version of the fn
narea <- 10
spatio.temporal.fishery.trend <- TRUE # this turns on or off the other spatial/temporal
spatio.temporal.bycatch.trend <- TRUE
spatial.effort.skewness.general <- c(1, 1)
spatial.effort.skewness.special <- c(1.7, 0.3)
time.periods.fishery <- 32:60
time.periods.bycatch <- 32:60

fishing_spatial <- make_fishing_year_metier_space(
  mean.bycatch.event = mean.bycatch.event,
  mean.bycatch.large.event = mean.bycatch.large.event,
  p.large.event = p.large.event,
  nboat = nboat,
  mean.fishing.event.boat.day = mean.fishing.event.boat.day,
  p.bycatch = p.bycatch,
  p.metier = p.metier,
  narea = narea,
  stochastic = stochastic,
  spatio.temporal.fishery.trend = spatio.temporal.fishery.trend,
  spatio.temporal.bycatch.trend = spatio.temporal.bycatch.trend,
  spatial.effort.skewness.general = spatial.effort.skewness.general,
  spatial.effort.skewness.special = spatial.effort.skewness.special,
  time.periods.fishery = time.periods.fishery,
  time.periods.bycatch = time.periods.bycatch,
  hotspot.area = 10
)
fishing_spatial |>
  dplyr::mutate(day_type = case_when(fishing.day >= 32 & fishing.day <= 60 ~ "special", TRUE ~ "regular")) |>
  dplyr::group_by(area, day_type) |>
  dplyr::summarize(mean(bycatch))


# Figures for case study example ------------------------------------------
# Scenario 1: Vessel effects ----------------------------------------------
nsample <- 5000
vessel.effect.vec <- c(0, 0.3, 0.7, 0.9)
p_monitor_boat.vec <- (2:31) / 31
bigdf <- vector()

for (p in 1:length(p_monitor_boat.vec)) {
  BPUE_real_vec <- BPUE_CV_vec <- BPUE_est_vec <- BPUE_bias_vec <- vector()
  for (i in 1:length(vessel.effect.vec)) {
    set.seed(123)
    fishing <- make_fishing_year_metier(
      mean.bycatch.event = mean.bycatch.event,
      mean.bycatch.large.event = mean.bycatch.large.event,
      p.large.event = p.large.event, nboat = nboat,
      mean.fishing.event.boat.day = mean.fishing.event.boat.day,
      p.bycatch = p.bycatch, p.metier = p.metier,
      stochastic = stochastic,
      vessel.effect = vessel.effect.vec[i]
    )
    BPUE_real_vec[i] <- sum(fishing$nbycatch) / dim(fishing)[1]

    obs_fishing <- monitor_BPUE_metier(
      pmonitor = pmonitor,
      nsample = nsample,
      BPUE_real = BPUE_real,
      fishing = fishing,
      p_monitor_boat = p_monitor_boat.vec[p],
      boat_samp = boat_samp,
      p_haul_obs = p_haul_obs,
      detect_prob = detect_prob,
      refusal_rate = refusal_rate,
      misclassification = misclassification,
      bymetier = bymetier,
      p_monitor_metier = p_monitor_metier
    )

    BPUE_bias_vec[i] <- (obs_fishing$BPUE_est - BPUE_real_vec[i]) / BPUE_real_vec[i]
    BPUE_est_vec[i] <- obs_fishing$BPUE_est
    BPUE_CV_vec[i] <- obs_fishing$CV
  } # /end vessel effect loop
  df <- data.frame(
    p_monitor_boat.vec[p],
    vessel.effect.vec,
    BPUE_real_vec,
    BPUE_est_vec,
    BPUE_CV_vec,
    BPUE_bias_vec
  )
  bigdf <- rbind(bigdf, df)
  cat(round(p / length(p_monitor_boat.vec) * 100), "% done \n")
}

save(bigdf, file = paste0("output/vessel_effect_cv_vs_BPUE_", nsample, ".rds"))
load("output/vessel_effect_cv_vs_BPUE.rds")

p1 <- bigdf %>%
  mutate(vessel.effect.vec = as.factor(vessel.effect.vec)) %>%
  ggplot(aes(
    x = p_monitor_boat.vec.p., y = BPUE_CV_vec,
    color = vessel.effect.vec, group = vessel.effect.vec
  )) +
  geom_line(lwd = 1.2) +
  xlab("Proportion of vessels monitored") +
  ylab("CV of BPUE estimate") +
  scale_color_manual("Vessel effect", values = wes_palette("Rushmore1", n = 5, type = "discrete")[c(1, 3, 4, 5)]) +
  theme_classic(base_size = 16)


png("output/vessel_effect_cv_vs_BPUE.png", width = 8, height = 6, units = "in", res = 200)
p1
dev.off()

# Scenario 2: expanding beyond ref fleet  ---------------------------------
# Parameterize simulations to represent an observer program, that you could compare to a reference fleet.

p_monitor_boat.vec <- 2:31 / 31
nsample <- 2000
pmonitor <- 0.5 # Observers work same as crew shifts (on/off over 24 hours)
p_monitor_metier <- 1
bymetier <- FALSE
boat_samp <- TRUE
refusal_rate.vec <- round(seq(0.1, 0.9, length.out = 4), digits = 2) # Impossible to define as sensitive to too many factors
p_haul_obs <- 0.95 # dedicated programme has lower chance of missing observations, set to small, insignificant chance
detect_prob <- 1
misclassification <- 0
vessel.effect <- 0.7

bigdf <- vector()

for (p in 1:length(p_monitor_boat.vec)) {
  BPUE_real_vec <- BPUE_CV_vec <- BPUE_est_vec <- BPUE_bias_vec <- vector()
  for (i in 1:length(refusal_rate.vec)) {
    set.seed(123)
    fishing <- make_fishing_year_metier(
      mean.bycatch.event = mean.bycatch.event,
      mean.bycatch.large.event = mean.bycatch.large.event,
      p.large.event = p.large.event, nboat = nboat,
      mean.fishing.event.boat.day = mean.fishing.event.boat.day,
      p.bycatch = p.bycatch, p.metier = p.metier,
      stochastic = stochastic,
      vessel.effect = vessel.effect
    )
    BPUE_real_vec[i] <- sum(fishing$nbycatch) / dim(fishing)[1]

    obs_fishing <- monitor_BPUE_metier(
      pmonitor = pmonitor,
      nsample = nsample,
      BPUE_real = BPUE_real,
      fishing = fishing,
      p_monitor_boat = p_monitor_boat.vec[p],
      boat_samp = boat_samp,
      p_haul_obs = p_haul_obs,
      detect_prob = detect_prob,
      refusal_rate = refusal_rate.vec[i],
      misclassification = misclassification,
      bymetier = bymetier,
      p_monitor_metier = p_monitor_metier
    )

    BPUE_bias_vec[i] <- (obs_fishing$BPUE_est - BPUE_real_vec[i]) / BPUE_real_vec[i]
    BPUE_est_vec[i] <- obs_fishing$BPUE_est
    BPUE_CV_vec[i] <- obs_fishing$CV
  } # /end vessel effect loop
  df <- data.frame(
    p_monitor_boat.vec[p],
    refusal_rate.vec,
    BPUE_real_vec,
    BPUE_est_vec,
    BPUE_CV_vec,
    BPUE_bias_vec
  )
  bigdf <- rbind(bigdf, df)
  cat(round(p / length(p_monitor_boat.vec) * 100), "% done \n")
}

# save(bigdf, file = paste0("output/observer_prog_cv_vs_BPUE_", nsample, ".rds"))

load("output/observer_prog_cv_vs_BPUE_10000.rds")
obs_program <- bigdf
nrow(obs_program) # this one is larger because it has different refusal rates

p2 <- obs_program |>
  mutate(refusal_rate.vec = as.factor(refusal_rate.vec)) |>
  ggplot(aes(
    x = p_monitor_boat.vec.p., y = BPUE_CV_vec,
    color = refusal_rate.vec,
    group = refusal_rate.vec
  )) +
  geom_line(lwd = 1.2) +
  theme_classic(base_size = 16) +
  scale_color_manual("Refusal rate", values = wes_palette("FantasticFox1", n = 4, type = "discrete")) +
  geom_hline(
    yintercept = 0.5367243,
    lty = 2, lwd = 1,
    color = "grey"
  ) + # This is the CV from the reference fleet we estimated above w vessel effect of 0.7
  xlab("Target proportion of vessels monitored") +
  ylab(expression(CV[BPUE]))

png("output/refusal_obsprogramme_cv_vs_BPUE.png",
  width = 8, height = 6, units = "in", res = 200
)
p2
dev.off()

# Scenario 3: Spatio-temporal changes  ---------------------------------
# Parameterize simulations to represent variations in seabird density and fishery effort across time and space.
# Test 'spatial' function -------------------------------------------------
source("make_fishing_year_metier_space.R")
# Added params for when you're using the spatial version of the fn
p.metier <- 1
p.bycatch <- 0.12 # range is from 0.02 to 0.12
p.large.event <- 0.12 #12% chance of big event if in hotspot
mean.bycatch.event <- 2 # (rounded down from 2.4)
mean.bycatch.large.event <- 17 # max in dataset is 36. This is the upper 25%ile
nboat <- 28
mean.fishing.event.boat.day <- 1
stochastic <- FALSE
narea <- 10
hotspot.area=10
spatio.temporal.fishery.trend <- TRUE # this turns on or off the other spatial/temporal
spatio.temporal.bycatch.trend <- TRUE
spatial.effort.skewness.general <- c(1.6,0.4)#Slight difference in allocation of vessels
spatial.effort.skewness.special <- c(1.7,0.3)#Slight difference in allocation of vessels
time.periods.fishery <- c(1:30, 270:365) # Two peaks in fishery intensity
time.periods.bycatch <- 150:240 # Hotspot in summer


# Figures for case study example ------------------------------------------
# Scenario 3: With spatiotemp ----------------------------------------------
nsample <- 2000
vessel.effect.vec <- c(0.7)
p_monitor_boat.vec <- (2:31) / 31
pmonitor <- 1
boat_samp <- TRUE
p_haul_obs <- 1 # should be fixed at 1 per David
detect_prob <- 0.7
refusal_rate <- 0 # ignoring refusal for now
misclassification <- 0
bymetier <- FALSE
p_monitor_metier <- 1
bigdf_spatio <- vector()

for (p in 1:length(p_monitor_boat.vec)) {
  BPUE_real_vec <-BPUE_CV_vec <- BPUE_est_vec <- BPUE_bias_vec <- vector()
  for (j in 1:length(vessel.effect.vec)) {
    set.seed(123)
    fishing<- make_fishing_year_metier_space(mean.bycatch.event = mean.bycatch.event,
                                             mean.bycatch.large.event = mean.bycatch.large.event,
                                             p.large.event = p.large.event,
                                             nboat = nboat,
                                             mean.fishing.event.boat.day = mean.fishing.event.boat.day,
                                             p.bycatch = p.bycatch,
                                             p.metier = p.metier,
                                             narea = narea,
                                             stochastic = stochastic,
                                             spatio.temporal.fishery.trend = spatio.temporal.fishery.trend,
                                             spatio.temporal.bycatch.trend = spatio.temporal.bycatch.trend,
                                             spatial.effort.skewness.general =spatial.effort.skewness.general,
                                             spatial.effort.skewness.special = spatial.effort.skewness.special,
                                             time.periods.fishery = time.periods.fishery,
                                             time.periods.bycatch = time.periods.bycatch,
                                             hotspot.area = hotspot.area,
                                             vessel.effect = vessel.effect.vec[j]
    )
    BPUE_real_vec[j] <- sum(fishing$nbycatch) / dim(fishing)[1]

    obs_fishing <- monitor_BPUE_metier(
      pmonitor = pmonitor,
      nsample = nsample,
      BPUE_real = BPUE_real,
      fishing = fishing,
      p_monitor_boat = p_monitor_boat.vec[p],
      boat_samp = boat_samp,
      p_haul_obs = p_haul_obs,
      detect_prob = detect_prob,
      refusal_rate = refusal_rate,
      misclassification = misclassification,
      bymetier = bymetier,
      p_monitor_metier = p_monitor_metier
    )

    BPUE_bias_vec[j] <- (obs_fishing$BPUE_est - BPUE_real_vec[j]) / BPUE_real_vec[j]
    BPUE_est_vec[j] <- obs_fishing$BPUE_est
    BPUE_CV_vec[j] <- obs_fishing$CV

  } #/end vessel effect loop
  df <- data.frame(p_monitor_boat.vec[p],
                   vessel.effect.vec,
                   BPUE_real_vec,
                   BPUE_est_vec,
                   BPUE_CV_vec,
                   BPUE_bias_vec)
  bigdf_spatio <- rbind(bigdf_spatio,df)
  cat(round(p/length(p_monitor_boat.vec) * 100), "% done \n")
}

save(bigdf_spatio, file = paste0('output/vessel_effect_cv_vs_BPUE_07_spatio_',nsample,'.rds'))
#load('vessel_effect_cv_vs_BPUE_5000.rds')

# Figures for case study example ------------------------------------------
# Scenario 3: Without spatiotemp ----------------------------------------------
spatio.temporal.fishery.trend <- FALSE # this turns on or off the other spatial/temporal
spatio.temporal.bycatch.trend <- FALSE

#nsample <- 200
#vessel.effect.vec <- 0.7
#p_monitor_boat.vec <- (2:31)/31
bigdf_nospatio <- vector()

for (p in 1:length(p_monitor_boat.vec)) {
  BPUE_real_vec <-BPUE_CV_vec <- BPUE_est_vec <- BPUE_bias_vec <- vector()
  for (i in 1:length(vessel.effect.vec)) {
    set.seed(123)
    fishing<- make_fishing_year_metier_space(mean.bycatch.event = mean.bycatch.event,
                                             mean.bycatch.large.event = mean.bycatch.large.event,
                                             p.large.event = p.large.event,
                                             nboat = nboat,
                                             mean.fishing.event.boat.day = mean.fishing.event.boat.day,
                                             p.bycatch = p.bycatch,
                                             p.metier = p.metier,
                                             narea = narea,
                                             stochastic = stochastic,
                                             spatio.temporal.fishery.trend = spatio.temporal.fishery.trend,
                                             spatio.temporal.bycatch.trend = spatio.temporal.bycatch.trend,
                                             spatial.effort.skewness.general =spatial.effort.skewness.general,
                                             spatial.effort.skewness.special = spatial.effort.skewness.special,
                                             time.periods.fishery = time.periods.fishery,
                                             time.periods.bycatch = time.periods.bycatch,
                                             hotspot.area = hotspot.area,
                                             vessel.effect = vessel.effect.vec[i]
    )
    BPUE_real_vec[i] <- sum(fishing$nbycatch) / dim(fishing)[1]

    obs_fishing <- monitor_BPUE_metier(
      pmonitor = pmonitor,
      nsample = nsample,
      BPUE_real = BPUE_real,
      fishing = fishing,
      p_monitor_boat = p_monitor_boat.vec[p],
      boat_samp = boat_samp,
      p_haul_obs = p_haul_obs,
      detect_prob = detect_prob,
      refusal_rate = refusal_rate,
      misclassification = misclassification,
      bymetier = bymetier,
      p_monitor_metier = p_monitor_metier
    )

    BPUE_bias_vec[i] <- (obs_fishing$BPUE_est - BPUE_real_vec[i]) / BPUE_real_vec[i]
    BPUE_est_vec[i] <- obs_fishing$BPUE_est
    BPUE_CV_vec[i] <- obs_fishing$CV

  } #/end vessel effect loop
  df <- data.frame(p_monitor_boat.vec[p],
                   vessel.effect.vec,
                   BPUE_real_vec,
                   BPUE_est_vec,
                   BPUE_CV_vec,
                   BPUE_bias_vec)
  bigdf_nospatio <- rbind(bigdf_nospatio,df)
  cat(round(p/length(p_monitor_boat.vec) * 100), "% done \n")
}

save(bigdf_nospatio, file = paste0('output/vessel_effect_cv_vs_BPUE_07_nospatio_',nsample,'.rds'))
#load('vessel_effect_cv_vs_BPUE_5000.rds')


#####
#Combining data for viz

bigdf_spatio$spatial<-"True"
bigdf_nospatio$spatial<-"False"
#bigdf_simspatio$spatial<-"Sim"
bigdf3<-rbind(bigdf_spatio,bigdf_nospatio)


library(ggplot2)
library(wesanderson)

p1 <- bigdf3 %>%
  mutate(vessel.effect.vec = as.factor(vessel.effect.vec)) %>%
  ggplot(aes(x=p_monitor_boat.vec.p.,y=BPUE_CV_vec  ,
             color = spatial, group = spatial)) +
  geom_line(lwd=1.2) +
  xlab("Proportion of vessels monitored") +
  ylab("CV of BPUE estimate") +
  scale_color_manual("Spatio-temporal effect", values = wes_palette("Rushmore1",n = 5, type = 'discrete')[c(1,3,4,5)] ) +
  theme_classic(base_size = 16)


png("output/cv_vs_BPUE_2000_spatio_no_spatio.png", width = 8, height = 6, units = 'in', res = 200)

p1
dev.off()
