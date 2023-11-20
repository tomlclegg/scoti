# Introducing area and time variability

## treating at first as two 'high res' metier categories, so say metier level 6 for a fishing year inspected at level 4
# parameters controlling fishery effort allocation and bycatch hot spot areas, relative to time
# spatial.effort.skewness.special<-c(1.7,0.3)# for time periods with different distribution

#' Making fishing year with spatial and spatiotemporal effects
#'
#' @param mean.bycatch.event
#' @param mean.bycatch.large.event
#' @param p.large.event
#' @param nboat
#' @param mean.fishing.event.boat.day
#' @param p.bycatch
#' @param p.metier
#' @param vessel.effect
#' @param narea - integer. defines how many areas you want.
#' @param stochastic
#' @param spatio.temporal.fishery.trend logical (TRUE/FALSE).
#' @param spatio.temporal.bycatch.trend logical (TRUE/FALSE).
#' @param spatial.effort.skewness.general vector. alpha and beta parameters from the in beta-binomial distribution, X∼BB(n,α,β). If α=β=1, then it is a discrete uniform distribution, if α≥1 and β<1 then it is a discrete left-skewed distribution.
#' @param spatial.effort.skewness.special vector. for time periods with different distribution.
#' @param time.periods.fishery #time of year you want the within area effort to shift, could be several periods (uses spatial.effort.skewness.special )
#' @param time.periods.bycatch
#' @param hotspot.area define area(s) that you would like to be hotspots for bycatch
#'
#' @return
#' @export
#'
#' @examples
make_fishing_year_metier_space <- function(mean.bycatch.event = 1, mean.bycatch.large.event = 20, p.large.event = 0.01,
                                           nboat = 100, mean.fishing.event.boat.day = 2, p.bycatch = c(0.1, .01), p.metier = c(.2, .8),
                                           narea = 10, stochastic = TRUE,vessel.effect=0 ,spatio.temporal.fishery.trend = TRUE, spatio.temporal.bycatch.trend = TRUE,
                                           spatial.effort.skewness.general = c(1, 1), spatial.effort.skewness.special = c(1.7, 0.3),
                                           time.periods.fishery = 32:60, time.periods.bycatch = 32:60, hotspot.area = 10) {
  # p.metier is the proportion of vessel in the, here, length of p.bycatch metiers
  # p bycatch event alternative distribution particularly for low density species
  require(extraDistr)
  nmetier <- length(p.bycatch)

  fishing.day <- 1:365
  fleet <- 1:nboat
  metier <- sample(1:nmetier, nboat, replace = TRUE, prob = p.metier) # here we deal with probability of metier, not proportion of metier

  if (stochastic == TRUE) {
    # here number of hauls is still not associated to "high res" metier
    mean.fishing.event.boat.day <- rtpois(nboat, mean.fishing.event.boat.day, a = 0) # introduce stochasticity so that the mean number of events per boats vary
    fishing.event.per.boat <- rpois(nboat, mean.fishing.event.boat.day)
  } else {
    fishing.event.per.boat <- rpois(nboat, mean.fishing.event.boat.day) # uniform fishing behaviour
  }


  fishing.area.dist <- sample(1:narea, nboat, replace = TRUE, prob = dbbinom(1:narea - 1, narea - 1, spatial.effort.skewness.general[1], spatial.effort.skewness.general[2])) # (runif(nboat,1,narea)) #




  i <- 1
  fishing <- data.frame(fishing.day = fishing.day[i], boat = rep(fleet, fishing.event.per.boat), area = rep(fishing.area.dist, fishing.event.per.boat), metiers = rep(metier, fishing.event.per.boat), bycatch = rbinom(sum(fishing.event.per.boat), 1, p.bycatch[rep(metier, fishing.event.per.boat)]), nbycatch = 0)

  event.type <- rbinom(sum(fishing$bycatch), 1, p.large.event)
  fishing$nbycatch[fishing$bycatch == 1] <- apply(cbind((1 - event.type) * rtpois(sum(fishing$bycatch), mean.bycatch.event, a = 0), event.type * rtpois(sum(fishing$bycatch), mean.bycatch.large.event, a = 0)), 1, max)



  for (i in 2:365) {
    if (stochastic == TRUE) {
      mean.fishing.event.boat.day<-rtpois(nboat,mean.fishing.event.boat.day,a=0) #that's stays the same for the whole year #introduce stochasticity so that the mean number of events per boats vary
      fishing.event.per.boat <- rpois(nboat, mean.fishing.event.boat.day)
    } else {
      fishing.event.per.boat <- rpois(nboat, mean.fishing.event.boat.day) # uniform fishing behaviour
    }

    if (spatio.temporal.fishery.trend == TRUE) {
      # enable change in fishery density in specific areas and time periods
      fishing.area.dist <- if (i %in% time.periods.fishery) {
        sample(1:narea, nboat, replace = TRUE, prob = dbbinom(1:narea - 1, narea - 1, spatial.effort.skewness.special[1], spatial.effort.skewness.special[2]))
      } else {
        sample(1:narea, nboat, replace = TRUE, prob = dbbinom(1:narea - 1, narea - 1, spatial.effort.skewness.general[1], spatial.effort.skewness.general[2]))
      }
      area <- rep(fishing.area.dist, fishing.event.per.boat)
    } else {
      fishing.area.dist <- sample(1:narea, nboat, replace = TRUE, prob = dbbinom(1:narea - 1, narea - 1, spatial.effort.skewness.general[1], spatial.effort.skewness.general[2]))
      area <- rep(fishing.area.dist, fishing.event.per.boat)
    }


    temp <- data.frame(fishing.day = fishing.day[i], boat = rep(fleet, fishing.event.per.boat), area = area, metiers = rep(metier, fishing.event.per.boat), bycatch = rbinom(sum(fishing.event.per.boat), 1, p.bycatch[rep(metier, fishing.event.per.boat)]), nbycatch = 0)
    # enable change in fishery density in specific areas and time periods
    if (spatio.temporal.bycatch.trend == TRUE) {
        bycatch_high <- temp[temp$fishing.day %in% time.periods.bycatch & temp$area %in% hotspot.area, ]
        bycatch_high$bycatch <- rbinom(length(bycatch_high[,1]), 1, exp(log(p.bycatch)*log(2)))
        event.type <- rbinom(sum(bycatch_high$bycatch), 1, p.large.event)
        bycatch_high$nbycatch[bycatch_high$bycatch == 1] <- apply(cbind((1 - event.type) * rtpois(sum(bycatch_high$bycatch), mean.bycatch.event, a = 0), event.type * rtpois(sum(bycatch_high$bycatch), mean.bycatch.large.event, a = 0)), 1, max)
        bycatch_low <- temp[!temp$fishing.day %in% time.periods.bycatch | !temp$area %in% hotspot.area, ]
        bycatch_low$bycatch <- rbinom(length(bycatch_low[,1]), 1, p.bycatch)
        bycatch_low$nbycatch[bycatch_low$bycatch == 1] <- rtpois(sum(bycatch_low$bycatch), mean.bycatch.event, a = 0)

        temp <- rbind(bycatch_high, bycatch_low)
    } else {
      temp <- temp
      event.type <- rbinom(sum(temp$bycatch), 1, p.large.event)
      temp$nbycatch[temp$bycatch == 1] <- apply(cbind((1 - event.type) * rtpois(sum(temp$bycatch), mean.bycatch.event, a = 0), event.type * rtpois(sum(temp$bycatch), mean.bycatch.large.event, a = 0)), 1, max)

}


    #event.type <- rbinom(sum(temp$bycatch), 1, p.large.event)
    #temp$nbycatch[temp$bycatch == 1] <- apply(cbind((1 - event.type) * rtpois(sum(temp$bycatch), mean.bycatch.event, a = 0), event.type * rtpois(sum(temp$bycatch), mean.bycatch.large.event, a = 0)), 1, max)

    fishing <- rbind(fishing, temp)
    # Add vessel effects
    tbl <- table(fishing$boat)
    nbycatch.vessel.adj <- rnorm(n = length(tbl), mean = 0, sd = vessel.effect)
    nbycatch.vessel.adj <- nbycatch.vessel.adj[match(fishing$boat, names(tbl))]
    fishing$nbycatch <- round(exp(log(fishing$nbycatch) + nbycatch.vessel.adj))
  }
  #########
  ## so for this challenge we need to change the computation of the estimated total bycatch it becomes the estimated BPUE x estimated effort
  return(fishing)
}
