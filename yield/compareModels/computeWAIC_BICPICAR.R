#Compute WAIC for the BIC PICAR model

rm(list=ls())

#library(fields); library(mvtnorm); library(invgamma); library(spam)

library(foreach)
library(doParallel)

#load data
load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

#get rid of county fixed effects
remove<-which(colnames(XMat)=="fips01003"):ncol(XMat)
XMat<- XMat[,-remove]

#cutoff selected by BIC
cutoff=0.2
#rank selected by BIC
rankM=20

load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AMat_allyrs_cutoff",cutoff,".RData",sep=""))
load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/MOE_cutoff",cutoff,".RData",sep=""))

M1<- as.matrix(MoransOperatorEig$vectors[,1:rankM])
AMat<- as.matrix(AMat)

mBase<- AMat%*%M1

ufips<- unique(timeloc.ord$fips)

M<- matrix(NA, nrow=nrow(XMat), ncol= rankM)

for(i in 1:ntrainyrs){
  timelocyr<- timeloc.ord[(cs.obs.train[i]+1):cs.obs.train[i+1],]
  fipsinds<- which(ufips%in%(timelocyr$fips))
  M[(cs.obs.train[i]+1):cs.obs.train[i+1],]<- mBase[fipsinds,]
}


rm(MoransOperatorEig)
# Remove unecessary objects
rm(distMatCVList) ; rm(distMatModList)
#rm(distMatHaversin) ; rm(distMatHaversin2)
rm(test_data); rm(train_data) ;
rm(ifyouaintfirst) ; rm(yourelast)
rm(comboLocation) ; rm(dat)
rm(timeloc.ord); rm(timelocyr)
rm(gridLocation,CVgridLocation)


################################################################################

#set constant
p<-ncol(M)
n<-nrow(XMat) #number of observations
k<-ncol(XMat) #number of coefficients to estimate
################################################################################
################################################################################
setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20",sep=""))
load("samples_cfmpw_allyrs.RData")

N= nrow(XMat)
N2= 4.9e4

all_b= res[2:nrow(res),1:k]
all_d= res[2:nrow(res),(k+1):(k+p)]
all_s2= res[2:nrow(res),ncol(res)-1]
all_t2= res[2:nrow(res),ncol(res)]

rm(res)

sum_llik<- 0
sum_lik<-0

i= nrow(XMat)
x= XMat[i,]

for(j in 1:N2){
  b= all_b[j,]
  d= all_d[j,]
  s2= all_s2[j]
  t2= all_t2[j]
  
  mu= XMat[i,]%*%b+ M[i,]%*%d
  
  llik<- dnorm(obsModLinear[i], mean= as.numeric(mu), sd= sqrt(s2), log=TRUE)
  lik<- dnorm(obsModLinear[i], mean= as.numeric(mu), sd= sqrt(s2), log=FALSE)
  
  sum_llik<- sum_llik + llik
  sum_lik<- sum_lik + lik
}

save(sum_lik,sum_llik,
     file=paste0("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/sum_likAndsum_llik",25,".RData"))

#setup parallel backend to use many processors
cores=detectCores()
cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
registerDoParallel(cl)

div= (N-1)/1996
remainder<- 0:(div-1)

almostall<- 1:(N-1)

foreach(h=remainder)%dopar%{
  
  inds<- which(almostall%%div==h)
  sum_lik<-0
  sum_llik<-0
  
  for(i in inds){
    
    x= XMat[i,]
    
    for(j in 1:N2){
      
      b= all_b[j,]
      d= all_d[j,]
      s2= all_s2[j]
      t2= all_t2[j]
      
      mu= XMat[i,]%*%b+ M[i,]%*%d
      
      llik<- dnorm(obsModLinear[i], mean= as.numeric(mu), sd= sqrt(s2), log=TRUE)
      lik<- dnorm(obsModLinear[i], mean= as.numeric(mu), sd= sqrt(s2), log=FALSE)
      
      sum_llik<- sum_llik + llik
      sum_lik<- sum_lik + lik
    }
  }
  save(sum_lik,sum_llik,
       file=paste0("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/sum_likAndsum_llik",h,".RData"))
}

#WAIC1<- 2*log(sum_lik/M) - (4/M)*sum_llik

stopCluster(cl)


#finish computing WAIC

rm(list=ls())

load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

setwd("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20")
load("samples_cfmpw_allyrs.RData")

N= nrow(XMat)
N2= 4.9e4

sum_llik0<- 0
sum_lik0<- 0

for(h in 0:25){
  load(paste0("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/sum_likAndsum_llik",h,".RData"))
  
  sum_llik0<- sum_llik0 + sum_llik
  sum_lik0<- sum_lik0 + sum_lik
  
}

WAIC1<- 2*log(sum_lik0/N2) - (4/N2)*sum_llik0

save(WAIC1,file="/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/WAIC1.RData")
