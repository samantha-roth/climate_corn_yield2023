#reorder pwl_norm by year instead of location
rm(list = ls())

nYrs<- 36

#library(tidyverse)

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_df")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/n.temp.scaled2.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/prpr2.scaled2.RData")

timeloc_df<- timeloc_df[which(timeloc_df$year%in%seq(1981,2016,by=1)),]

length(which(is.na(n.temp.scaled)))
length(which(is.na(prpr2.scaled)))

#recInds<- which(timeloc_df$year>2016)
#nrow(timeloc_df[-recInds,])


#format the data so it's in order of year instead of location

year<- as.character(timeloc_df$year)
year<- as.numeric(year)

year<- year[order(year)]

pwln.ord<- matrix(NA, ncol= ncol(n.temp.scaled), nrow= nrow(n.temp.scaled))
#pwln.ord<- as.data.frame(n.temp.scaled)
colnames(pwln.ord)<- colnames(n.temp.scaled)

n.temp.scaled<- as.matrix(n.temp.scaled)

for(i in 1:nYrs){
  inds<- which(timeloc_df$year==1980+i)
  pwln.ord[which(year==1980+i),]<- n.temp.scaled[inds,]
}

save(pwln.ord, file="/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/pwl_norm_yrorder2.RData")

prpr2.ord<- matrix(NA, ncol= ncol(prpr2.scaled), nrow= nrow(prpr2.scaled))
#pwln.ord<- as.data.frame(n.temp.scaled)
colnames(prpr2.ord)<- colnames(prpr2.scaled)

prpr2.scaled<- as.matrix(prpr2.scaled)

for(i in 1:nYrs){
  inds<- which(timeloc_df$year==1980+i)
  prpr2.ord[which(year==1980+i),]<- prpr2.scaled[inds,]
}

save(prpr2.ord, file="/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/prpr2_norm_yrorder2.RData")


length(which(is.na(prpr2.ord)))
