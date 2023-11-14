#####
##### this function monitors the simulated fishing year
####################################################################################################

monitor_BPUE_metier<-function(pmonitor=0.5,nsample=1000,BPUE_real=0,fishing=NA, p_monitor_boat=.1,boat_samp=TRUE,
p_haul_obs=1,detect_prob=1,refusal_rate=0, misclassification=0, bymetier=FALSE, p_monitor_metier=.5) {

#p_haul_obs the probability that an entire fishing events (hauls) was observed by the observer
#detect_prob is the detection probability of each individual in the bycatch event
#while at first, detect_prob is an independent draw on each individual in the haul, we can implement a decrease in detect_prob
#with nbycatch increasing (on a log scale or similar)

#misclassification is the mis-identification probability for the bycaught species
#misclassification might come in later when we have multiple species which can be confused, and therefore individuals from nbycatch_sp_i can
#be taken to add to nbycatch_sp_j

#refusal_rate: the probability that a vessel selected for monitoring refuses to engage
#refusal is replaced to introduce selective pressures
#option 1 random option 2 associated with nbycatch

#integrate observer procedure effect
#this is protocol influence process
# two potential effects : haul level effect: unobserved bycatch when monitored (lack of observation)
#as well as decrease in the number of animal reported on a bycatch event (incomplete observation, eg drop out, multiple location needed)
#observation probability function post sampling

#second step: variance associated with observed ID

#p_monitor_metier: what is the proportion of the monitoring effort which is attributed to each "metier" simulated in fishing
nmetier=length(unique(fishing$metiers))

if (bymetier==FALSE) {
BPUE_est<-array(NA,dim=nsample)
} else {
BPUE_est<-array(NA,dim=c(nsample,nmetier))
}

for (j in 1:nsample) { 

if (bymetier==FALSE) {

if (boat_samp==FALSE) {
monitored<-sample(c(1:dim(fishing)[1]),floor(pmonitor*dim(fishing)[1]),replace=FALSE) # sample without replacement
fishing_monitored<-fishing[monitored,]
  if (p_haul_obs<1) {
not_observed<-sample(c(1:dim(fishing_monitored)[1]),floor((1-p_haul_obs)*dim(fishing_monitored)[1]),replace=FALSE)
fishing_monitored$bycatch[not_observed]<-0
fishing_monitored$nbycatch[not_observed]<-0
    }
  if (detect_prob<1) {
fishing_monitored$nbycatch<-sapply(fishing_monitored$nbycatch,function(x) rbinom(1,x,detect_prob))
}
                                   
BPUE_est[j]<-(sum(fishing_monitored$nbycatch)/length(monitored))

} else {
boat_sampled<-sample(unique(fishing$boat),size=floor(length(unique(fishing$boat))*p_monitor_boat),replace=FALSE)
## think about situations when boats are never going out in a year. at the moment, the observer programme allows to react and sample
## only those vessels that have been fishing at least once per year.

#refusal
boat_sampled<-sample(boat_sampled,size=floor(length(boat_sampled)*(1-refusal_rate)),replace=FALSE)

fleet_sampled<-fishing[fishing$boat%in%boat_sampled,]
monitored<-sample(c(1:dim(fleet_sampled)[1]),floor(pmonitor*dim(fleet_sampled)[1]),replace=FALSE) # sample without replacement
fishing_monitored<-fleet_sampled[monitored,]

if (p_haul_obs<1) {
not_observed<-sample(c(1:dim(fishing_monitored)[1]),floor((1-p_haul_obs)*dim(fishing_monitored)[1]),replace=FALSE)
fishing_monitored$bycatch[not_observed]<-0
fishing_monitored$nbycatch[not_observed]<-0
  }
if (detect_prob<1) {
fishing_monitored$nbycatch<-sapply(fishing_monitored$nbycatch,function(x) rbinom(1,x,detect_prob))
}
                                   
BPUE_est[j]<-(sum(fishing_monitored$nbycatch)/length(monitored))
}

} else {
nmetier<-length(unique(fishing$metiers)) # for error catching later on

if (boat_samp==FALSE) {
nmetier<-length(unique(fishing$metiers)) # for error catching later on
monitored_by_metier<-p_monitor_metier*pmonitor

monitored<-sample(as.numeric(row.names(fishing[fishing$metiers==1,])),floor(monitored_by_metier[1]*length(row.names(fishing[fishing$metiers==1,]))),replace=FALSE) # sample without replacement
for (i in 2:nmetier) {
monitored<-c(monitored,sample(as.numeric(row.names(fishing[fishing$metiers==i,])),floor(monitored_by_metier[i]*length(row.names(fishing[fishing$metiers==i,]))),replace=FALSE)) # sample without replacement

}

fishing_monitored<-fishing[monitored,]
not_observed<-sample(c(1:dim(fishing_monitored)[1]),floor((1-p_haul_obs)*dim(fishing_monitored)[1]),replace=FALSE)
fishing_monitored$bycatch[not_observed]<-0
fishing_monitored$nbycatch[not_observed]<-0
fishing_monitored$nbycatch<-sapply(fishing_monitored$nbycatch,function(x) rbinom(1,x,detect_prob))

BPUE_est[j,]<-(tapply(fishing_monitored$nbycatch,fishing_monitored$metiers,sum)/tapply(fishing_monitored$nbycatch,fishing_monitored$metiers,length))



} else {
boat_monitored_by_metier<-p_monitor_metier*p_monitor_boat


boat_sampled<-sample(unique(fishing$boat[fishing$metiers==1]),ceiling(boat_monitored_by_metier[1]*length(unique(fishing$boat[fishing$metiers==1]))),replace=FALSE) # sample without replacement
for (i in 2:nmetier) {
boat_sampled<-c(boat_sampled,sample(unique(fishing$boat[fishing$metiers==i]),ceiling(boat_monitored_by_metier[i]*length(unique(fishing$boat[fishing$metiers==i]))),replace=FALSE)) # sample without replacement

}

#refusal
boat_sampled<-sample(boat_sampled,size=ceiling(length(boat_sampled)*(1-refusal_rate)),replace=FALSE)

fleet_sampled<-fishing[fishing$boat%in%boat_sampled,]
monitored<-sample(c(1:dim(fleet_sampled)[1]),floor(pmonitor*dim(fleet_sampled)[1]),replace=FALSE) # sample without replacement
fishing_monitored<-fleet_sampled[monitored,]
not_observed<-sample(c(1:dim(fishing_monitored)[1]),floor((1-p_haul_obs)*dim(fishing_monitored)[1]),replace=FALSE)
fishing_monitored$bycatch[not_observed]<-0
fishing_monitored$nbycatch[not_observed]<-0
fishing_monitored$nbycatch<-sapply(fishing_monitored$nbycatch,function(x) rbinom(1,x,detect_prob))


BPUE_est[j,]<-(tapply(fishing_monitored$nbycatch,fishing_monitored$metiers,sum)/tapply(fishing_monitored$nbycatch,fishing_monitored$metiers,length))
}

} #metier else bracket

} # sample iteration loop
if (bymetier==FALSE) {
BPUE_est_mean<-mean(BPUE_est,na.rm=TRUE) #if bymetier is TRUE then 2 element vector
BPUE_est_CV<-sd(BPUE_est,na.rm=TRUE)/mean(BPUE_est,na.rm=TRUE) 
} else {
BPUE_est_mean<-colMeans(BPUE_est,na.rm=TRUE) #if bymetier is TRUE then 2 element vector
BPUE_est_CV<-apply(BPUE_est,2,function(x) sd(x,na.rm=TRUE))/colMeans(BPUE_est,na.rm=TRUE) 
}

return(list(BPUE_est=BPUE_est_mean,CV=BPUE_est_CV))
}


