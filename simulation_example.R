#####
##### this code gives an example of how the functions can be used to simulate fishing and monitoring (here 100 independent years simulated each monitored 1000 times)
################################################################################################################################################


# bymetier<-c("TRUE","FALSE")
# boat_samp<-c("TRUE","FALSE")

# ###
# pmonitor_boatFALSE<-c(.9,.5,.1,.05,.01,.001)
# ###

###
p_monitor_metier<-cbind(c(1,.8,.2,.2),c(1,.2,.8,.2))

#estimate_fishing_effort_metier<-function(fishing=NA,p_report=.9,bymetier=FALSE,metierlevel=TRUE) {

#monitor_BPUE_metier<-function(pmonitor=0.5,nsample=1000,BPUE_real=0,fishing=NA, p_monitor_boat=.1,boat_samp=TRUE,
#p_haul_obs=1,detect_prob=1,refusal_rate=0, misclassification=0, bymetier=FALSE, p_monitor_metier=.5)

#make_fishing_year_metier<-function(mean.bycatch.event=1,mean.bycatch.large.event=20,p.large.event=0.01,
#							nboat=100,mean.fishing.event.boat.day=2,p.bycatch=c(0.1,.01),p.metier=c(.2,.8),stochastic=TRUE)


#fishing1<-make_fishing_year_metier(p.bycatch=p.bycatch[1,],p.metier=p.metier[1,])

#BPUE_real<-sum(fishing1$nbycatch)/dim(fishing1)[1]

#effort<-estimate_fishing_effort_metier(fishing=fishing1,p_report=p_report[1,],bymetier=TRUE,metierlevel=TRUE)


#monitoring<-monitor_BPUE_metier(pmonitor=pmonitor_boatTRUE[3],nsample=1000,BPUE_real=BPUE_real,fishing=fishing1, 
#p_monitor_boat=p_monitor_boat_boatTRUE[2],boat_samp=TRUE,p_haul_obs=1,detect_prob=1,refusal_rate=0, misclassification=0, bymetier=TRUE, p_monitor_metier=p_monitor_metier[1,])

bymetier<-"FALSE"
boat_samp<-"TRUE"

p.bycatch<-cbind(c(.1,.01,.001,.001,.001),c(.2,.02,.002,.01,.1))
p.metier<-cbind(c(.5,.2,.8),c(.5,.8,.2))
p_report<-cbind(c(1,.8,.2,.2),c(1,.2,.8,.2))

pmonitor_boatTRUE<-c(.01,0.05,.1,.2)
p_monitor_boat_boatTRUE<-c(.01,.05,.1,.2)
# pmonitor_boatFALSE<-c(.001,.01,.05,.1,.5,.9)
p_monitor_metier<-cbind(c(1,.8,.2,.2),c(1,.2,.8,.2))

monitor_estimate<-data.frame(year=NA,p_bycatch_1=NA,p_bycatch_2=NA,p_metier_1=NA,p_metier_2=NA,pmonitor=NA,p_monitor_boat=NA,boat_samp=NA,bymetier=NA,p_monitor_metier=NA,BPUE_real=NA,BPUE_est=NA,BPUE_est_CV=NA)

b=1 #1 to 5
m=1 #1 to 3

###

 for (y in 1:100) {
#iter<-1
#for (m.s in 1:2) {

#for (b.s in 1:2) {


#for (b in 1:dim(p.bycatch)[1]) {

#for (m in 1:dim(p.metier)[1]) {
fishing<-make_fishing_year_metier(p.bycatch=p.bycatch[b,],p.metier=p.metier[m,])
BPUE_real<-sum(fishing$nbycatch)/dim(fishing)[1]
print("fishing done")
flush.console()

for (pm.b in 1:length(pmonitor_boatTRUE)) {

for (p_m.b in 1:length(p_monitor_boat_boatTRUE)) {

for (p_m_m in 1:dim(p_monitor_metier)[1]) {
if (bymetier==FALSE) {
temp<-data.frame(year=y,p_bycatch_1=NA,p_bycatch_2=NA,p_metier_1=NA,p_metier_2=NA,pmonitor=NA,p_monitor_boat=NA,boat_samp=NA,bymetier=NA,p_monitor_metier=NA,BPUE_real=NA,BPUE_est=NA,BPUE_est_CV=NA)

temp$p_bycatch_1<-p.bycatch[b,1]
temp$p_bycatch_2<-p.bycatch[b,2]
temp$p_metier_1<-p.metier[m,1]
temp$p_metier_2<-p.metier[m,2]
temp$pmonitor<-pmonitor_boatTRUE[pm.b]
temp$p_monitor_boat<-p_monitor_boat_boatTRUE[p_m.b]
temp$boat_samp<-boat_samp
temp$bymetier<-bymetier
temp$p_monitor_metier<-p_monitor_metier[p_m_m,1]
temp$BPUE_real<-BPUE_real

} else {
temp<-data.frame(year=rep(y,2),p_bycatch_1=NA,p_bycatch_2=NA,p_metier_1=NA,p_metier_2=NA,pmonitor=NA,p_monitor_boat=NA,boat_samp=NA,bymetier=NA,p_monitor_metier=NA,BPUE_real=NA,BPUE_est=NA,BPUE_est_CV=NA)
temp$p_bycatch_1<-p.bycatch[b,1]
temp$p_bycatch_2<-p.bycatch[b,2]
temp$p_metier_1<-p.metier[m,1]
temp$p_metier_2<-p.metier[m,2]
temp$pmonitor<-pmonitor_boatTRUE[pm.b]
temp$p_monitor_boat<-p_monitor_boat_boatTRUE[p_m.b]
temp$boat_samp<-boat_samp
temp$bymetier<-bymetier
temp$p_monitor_metier<-p_monitor_metier[p_m_m,]
temp$BPUE_real<-BPUE_real

}



monitoring<-monitor_BPUE_metier(pmonitor=pmonitor_boatTRUE[pm.b],nsample=1000,BPUE_real=BPUE_real,fishing=fishing, 
p_monitor_boat=p_monitor_boat_boatTRUE[p_m.b],boat_samp=boat_samp,p_haul_obs=1,detect_prob=1,refusal_rate=0, misclassification=0, bymetier=bymetier, p_monitor_metier=p_monitor_metier[p_m_m,])

temp$BPUE_est<-monitoring$BPUE_est
temp$BPUE_est_CV<-monitoring$CV

monitor_estimate<-rbind(monitor_estimate,temp)

# print(b)
# print(m)
# flush.console()
} #p_m_m
} # p_m.b
} # pm.b
#} #m
#} #b
#} #b.s
#} #m.s 
 
 print(y)
 flush.console()
 } #y
 
 
