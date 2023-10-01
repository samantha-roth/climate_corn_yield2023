#created: 03/31/2021, updated 09/07/2022
#combine data sources into one data frame for the years 1981-2016
rm(list = ls())
graphics.off()

#setwd("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me")

countynum=1878

tmax_all<-matrix(NA,nrow=countynum,ncol=27*365+9*366)
tmin_all<-matrix(NA,nrow=countynum,ncol=27*365+9*366)

m1<-1 #used to record which columns to write in each loop
m2<-1


for(i in 1:36){
  
  k=365
  if (i%%4==0){ #when exist Feb29
    k=366
  }
  
  m2<-m1+k-1
  
  load(paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmax",1980+i,".RData"))
  load(paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmin",1980+i,".RData"))
  
  
  tmax_all[,c(m1:m2)]<-tmax
  tmin_all[,c(m1:m2)]<-tmin
  
  m1<-m2+1
  print(i)
}

tmax<- tmax_all
tmin<- tmin_all

save(tmax,file=paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmax.RData"))
save(tmin,file=paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmin.RData"))


naTmax<- which(is.na(tmax_all),arr.ind=TRUE)
naTmin<- which(is.na(tmin_all),arr.ind=TRUE)
