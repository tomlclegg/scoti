heterogeneity_stats_distributions<-function(pd=.5,pv=.5,iter=1000,n.estimates=30,
			mean.bycatch.event=1,mean.bycatch.large.event=20,p.large.event=0.01,
			nboat=100,mean.fishing.event.boat.day=2,p.bycatch=c(0.1,.01),p.metier=c(.2,.8),stochastic=TRUE) { 

#iter is the number of iteration for which tau and I2 are estimated, it dictates the number of values in the distribution against 
# which the observed tau and I2 will be compared
#n.estimates is the number of BPUE estimates we are using to model BPUE in the real data
# pd is the proportion of days monitored per vessel
# pv is the proportion of vessels monitored

dist.df.all<-data.frame(pd=NA,pv=NA,BPUE.real=NA,BPUE.meta=NA,DaS.monitored=NA,tau=NA,I2=NA)
# lazy declaration of the df

v<-floor(nboat*pv)

dist.df<-data.frame(pd=pd,pv=pv,BPUE.real=rep(NA,iter),BPUE.meta=rep(NA,iter),DaS.monitored=rep(NA,iter),tau=rep(NA,iter),I2=rep(NA,iter))

for (f in 1:iter) {
fishing<-make_fishing_year_metier(mean.bycatch.event=mean.bycatch.event,mean.bycatch.large.event=mean.bycatch.large.event,p.large.event=p.large.event,nboat=nboat,mean.fishing.event.boat.day=mean.fishing.event.boat.day,p.bycatch=p.bycatch,p.metier=p.metier,stochastic=stochastic)

DaS.fleet<- (365*nboat)-(sum(table(fishing$boat,fishing$fishing.day)==0))
haul.fleet<-dim(fishing)[1]
total.bycatch.event<-sum(fishing$bycatch)
total.bycatch<-sum(fishing$nbycatch)
# the "real" bycatch information

meta.df<-data.frame(n_ind=rep(NA,n.estimates),DaysAtSea=rep(NA,n.estimates))
#now we carry out the n.estimates 'studies' or monitoring observations as obtained in the reported data

for (j in 1:n.estimates) {
monitored.total.bycatch<-0
monitored.DaS<-0
boat.sampled<-sample(1:nboat,v,replace=F)

for (i in 1:length(boat.sampled)) {
temp<-subset(fishing,boat==boat.sampled[i])
d<-floor(length(unique(temp$fishing.day))*pd)

monitored.DaS<-monitored.DaS+d #let's update how many days were monitored
days.sampled<-sample(unique(temp$fishing.day),d,replace=F)
monitored.total.bycatch<-monitored.total.bycatch+sum(temp[temp$fishing.day%in%days.sampled,]$nbycatch)

}
meta.df$n_ind[j]<-monitored.total.bycatch
meta.df$DaysAtSea[j]<-monitored.DaS
}

mod<-rma.glmm(xi=n_ind,ti=(DaysAtSea),measure="IRLN",data=meta.df)

dist.df$BPUE.meta[f]<-as.numeric(exp(mod$beta))
dist.df$tau[f]<-mod$tau
dist.df$I2[f]<-mod$I2
dist.df$BPUE.real[f]<-total.bycatch/DaS.fleet
dist.df$DaS.monitored[f]<-mean(meta.df$DaysAtSea)/DaS.fleet
}
dist.df.all<-rbind(dist.df.all,dist.df)

dist.df.all<-dist.df.all[-1,]

return(dist.df.all)
}
