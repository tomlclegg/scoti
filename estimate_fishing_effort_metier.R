#####
##### this function estimates the fishing effort of the simulated fishing year
#####################################################################################

estimate_fishing_effort_metier<-function(fishing=NA,p_report=.9,bymetier=FALSE,metierlevel=TRUE) {
if (metierlevel==FALSE) {
effort<-data.frame(DaS_report=NA,hauls_report=NA,DaS_estimated=NA,hauls_estimated=NA,DaS_real=NA,hauls_real=NA)
#we calculate real effort, reported effort, and estimated effort where we know the fleet size and assume average effort for those unreported
# here reporting bias is at the vessel/boat level at first random then biased by high res metier (eg small vessels not reporting)
if (bymetier==FALSE) {
reporting<-sample(1:nboat,floor(p_report*nboat),replace=FALSE)
fishing.report<-fishing[fishing$boat%in%reporting,]
effort$hauls_real<-dim(fishing)[1]
effort$hauls_report<-dim(fishing.report)[1]
effort$hauls_estimated<-dim(fishing.report)[1]+(nboat-floor(p_report*nboat))*mean(table(fishing.report$boat)) #add 1-p_report nboat fishing mean number of hauls of those reporting
Das<-rowSums(table(fishing$boat,fishing$fishing.day)!=0)
Das_report<-rowSums(table(fishing.report$boat,fishing.report$fishing.day)!=0)

effort$DaS_real<-sum(Das)
effort$DaS_report<-sum(Das_report)
effort$DaS_estimated=sum(Das_report)+(nboat-floor(p_report*nboat))*mean(Das_report)

} else {
#p_report is a vector
nmetier<-length(unique(fishing$metiers))

reporting<-sample(unique(fishing$boat[fishing$metiers==1]),floor(p_report[1]*length(unique(fishing$boat[fishing$metiers==1]))),replace=FALSE) # sample without replacement
for (i in 2:nmetier) {
reporting<-c(reporting,sample(unique(fishing$boat[fishing$metiers==i]),floor(p_report[i]*length(unique(fishing$boat[fishing$metiers==i]))),replace=FALSE)) # sample without replacement

}

#reporting<-sample(1:nboat,floor(p_report*nboat),replace=FALSE)

fishing.report<-fishing[fishing$boat%in%reporting,]
effort$hauls_real<-dim(fishing)[1]
effort$hauls_report<-dim(fishing.report)[1]
effort$hauls_estimated<-dim(fishing.report)[1]+(nboat-floor(p_report*nboat))*mean(table(fishing.report$boat)) #add 1-p_report nboat fishing mean number of hauls of those reporting
Das<-rowSums(table(fishing$boat,fishing$fishing.day)!=0)
Das_report<-rowSums(table(fishing.report$boat,fishing.report$fishing.day)!=0)

effort$DaS_real<-sum(Das)
effort$DaS_report<-sum(Das_report)
effort$DaS_estimated<-sum(Das_report)+(nboat-floor(p_report*nboat))*mean(Das_report)

}
} else { # if metierlevel bracket
effort<-data.frame(metier=unique(fishing$metiers),DaS_report=NA,hauls_report=NA,DaS_estimated=NA,hauls_estimated=NA,DaS_real=NA,hauls_real=NA)
###
if (bymetier==FALSE) {
reporting<-sample(1:nboat,floor(p_report*nboat),replace=FALSE)
fishing.report<-fishing[fishing$boat%in%reporting,]
effort$hauls_real<-as.numeric(table(fishing$metier))
effort$hauls_report<-as.numeric(table(fishing.report$metier))
nvessels_metier<-colSums(table(fishing$boat,fishing$metiers)!=0)-colSums(table(fishing.report$boat,fishing.report$metiers)!=0)
effort$hauls_estimated<-as.numeric(table(fishing.report$metier))+nvessels_metier*colMeans(table(fishing.report$boat,fishing.report$metiers)) #add 1-p_report nboat fishing mean number of hauls of those reporting

#####################!!!
Das<-apply(table(fishing$boat,fishing$fishing.day,fishing$metiers)!=0,c(3,1),sum)
Das_report<-apply(table(fishing.report$boat,fishing.report$fishing.day,fishing.report$metiers)!=0,c(3,1),sum)

effort$DaS_real<-rowSums(Das)
effort$DaS_report<-rowSums(Das_report)
effort$DaS_estimated=rowSums(Das_report)+nvessels_metier*rowMeans(Das_report)

} else {
#p_report is a vector
nmetier<-length(unique(fishing$metiers))

reporting<-sample(unique(fishing$boat[fishing$metiers==1]),floor(p_report[1]*length(unique(fishing$boat[fishing$metiers==1]))),replace=FALSE) # sample without replacement
for (i in 2:nmetier) {
reporting<-c(reporting,sample(unique(fishing$boat[fishing$metiers==i]),floor(p_report[i]*length(unique(fishing$boat[fishing$metiers==i]))),replace=FALSE)) # sample without replacement

}

#reporting<-sample(1:nboat,floor(p_report*nboat),replace=FALSE)

fishing.report<-fishing[fishing$boat%in%reporting,]
effort$hauls_real<-as.numeric(table(fishing$metier))
effort$hauls_report<-as.numeric(table(fishing.report$metier))
nvessels_metier<-colSums(table(fishing$boat,fishing$metiers)!=0)-colSums(table(fishing.report$boat,fishing.report$metiers)!=0)
effort$hauls_estimated<-as.numeric(table(fishing.report$metier))+nvessels_metier*colMeans(table(fishing.report$boat,fishing.report$metiers)) #add 1-p_report nboat fishing mean number of hauls of those reporting

Das<-apply(table(fishing$boat,fishing$fishing.day,fishing$metiers)!=0,c(3,1),sum)
Das_report<-apply(table(fishing.report$boat,fishing.report$fishing.day,fishing.report$metiers)!=0,c(3,1),sum)

effort$DaS_real<-rowSums(Das)
effort$DaS_report<-rowSums(Das_report)
effort$DaS_estimated=rowSums(Das_report)+nvessels_metier*rowMeans(Das_report)

}
###
}

return(effort) #one row if no metier n rows if n metiers
}

