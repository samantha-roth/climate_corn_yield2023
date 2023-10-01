
#get the growing season for each year using TopoWX data

rm(list=ls())

load(paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmax.RData"))
load(paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmin.RData"))

nyrs=36
gs_length=185
countynum=1878

#third find growing season and three growing phases
ma <- function(arr, n){  #moving avg function
  res = rep(NA,length(arr)-n+1)
  for(i in n:length(arr)){
    res[i] = mean(arr[(i-n+1):i])
  }
  res
}

Tthres<-10
GS_start<-matrix(NA,nrow=countynum,ncol=nyrs)
GS_end<-matrix(NA,nrow=countynum,ncol=nyrs)
m1=1
m2=1
for (i in 1:countynum){
  m1=1
  m2=1
  for (j in 1:nyrs){
    k=365
    if (j%%4==2){ #when exist Feb29
      k=366
    }
    m2<-m1+k-1
    Tmaxtoget<-tmax[i,c(m1:m2)]
    Tmintoget<-tmin[i,c(m1:m2)]
    Tmeantoget<-0.5*(Tmaxtoget+Tmintoget)
    GDDMA<-ma(Tmeantoget,21)
    GS_start[i,j]<-which(GDDMA >= Tthres)[1]
    GS_end[i,j]<-GS_start[i,j]+(gs_length-1)
    m1=m2+1
  }
}
save(GS_start,file="/storage/work/svr5482/Countywise/TopoWX/GS_start")
save(GS_end,file="/storage/work/svr5482/Countywise/TopoWX/GS_end")


#load("/storage/work/svr5482/Countywise/TopoWX/GS_start")
#load("/storage/work/svr5482/Countywise/TopoWX/GS_end")
