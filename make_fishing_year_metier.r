
make_fishing_year_metier<-function(mean.bycatch.event=1,mean.bycatch.large.event=20,p.large.event=0.01,
							nboat=100,mean.fishing.event.boat.day=2,p.bycatch=c(0.1,.01),p.metier=c(.2,.8),stochastic=TRUE) {

#p.metier is the proportion of vessel in the, here, length of p.bycatch metiers 
# p bycatch event alternative distribution particularly for low density species
require(extraDistr)

nmetier<-length(p.bycatch)

fishing.day<-1:365
fleet<-1:nboat							
metier<-sample(1:nmetier,nboat,replace=TRUE,prob=p.metier) #here we deal with probability of metier, not proportion of metier hence sampling with replacement

if (stochastic==TRUE) {
# here number of hauls is still not associated to "high res" metier
mean.fishing.event.boat.day<-rtpois(nboat,mean.fishing.event.boat.day,a=0)  #introduce stochasticity so that the mean number of events per boats vary
fishing.event.per.boat<-rpois(nboat,mean.fishing.event.boat.day)

} else {
fishing.event.per.boat<-rpois(nboat,mean.fishing.event.boat.day) #uniform fishing behaviour

}

i=1
fishing<-data.frame(fishing.day=fishing.day[i],boat=rep(fleet,fishing.event.per.boat),metiers=rep(metier,fishing.event.per.boat),bycatch=rbinom(sum(fishing.event.per.boat),1,p.bycatch[rep(metier,fishing.event.per.boat)]),nbycatch=0)

event.type<-rbinom(sum(fishing$bycatch),1,p.large.event)
fishing$nbycatch[fishing$bycatch==1]<-apply(cbind((1-event.type)*rtpois(sum(fishing$bycatch),mean.bycatch.event,a=0),event.type*rtpois(sum(fishing$bycatch),mean.bycatch.large.event,a=0)),1,max)


for (i in 2:365) {

if (stochastic==TRUE) {
#mean.fishing.event.boat.day<-rtpois(nboat,mean.fishing.event.boat.day,a=0) #that's stays the same for the whole year #introduce stochasticity so that the mean number of events per boats vary
fishing.event.per.boat<-rpois(nboat,mean.fishing.event.boat.day)
} else {
fishing.event.per.boat<-rpois(nboat,mean.fishing.event.boat.day) #uniform fishing behaviour
}

temp<-data.frame(fishing.day=fishing.day[i],boat=rep(fleet,fishing.event.per.boat),metiers=rep(metier,fishing.event.per.boat),bycatch=rbinom(sum(fishing.event.per.boat),1,p.bycatch[rep(metier,fishing.event.per.boat)]),nbycatch=0)
event.type<-rbinom(sum(temp$bycatch),1,p.large.event)
temp$nbycatch[temp$bycatch==1]<-apply(cbind((1-event.type)*rtpois(sum(temp$bycatch),mean.bycatch.event,a=0),event.type*rtpois(sum(temp$bycatch),mean.bycatch.large.event,a=0)),1,max)

fishing<-rbind(fishing,temp)

}
#########
## so for this challenge we need to change the computation of the estimated total bycatch it becomes the estimated BPUE x estimated effort
return(fishing)
}

