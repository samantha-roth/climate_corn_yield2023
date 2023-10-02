
#WAIC code

rm(list=ls())

library(foreach)
library(doParallel)

load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

setwd("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output")
load("samples_cfmpw_fast.RData")
load("time_cfmpw_fast.RData")

N= nrow(XMat)
M= 2e4

sum_llik<- 0
sum_lik<-0

i= nrow(XMat)
x= XMat[i,]
for(j in 1:M){
  b= res[j+1,1:(ncol(res)-1)]
  s2= res[j+1,ncol(res)]
  llik<- dnorm(obsModLinear[i], mean= as.numeric(x%*%b), sd= sqrt(s2), log=TRUE)
  lik<- dnorm(obsModLinear[i], mean= as.numeric(x%*%b), sd= sqrt(s2), log=FALSE)
  
  sum_llik<- sum_llik + llik
  sum_lik<- sum_lik + lik
}

save(sum_lik,sum_llik,file=paste0("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/sum_likAndsum_llik",25,".RData"))

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
    for(j in 1:M){
      b= res[j+1,1:(ncol(res)-1)]
      s2= res[j+1,ncol(res)]
      llik<- dnorm(obsModLinear[i], mean= as.numeric(x%*%b), sd= sqrt(s2), log=TRUE)
      lik<- dnorm(obsModLinear[i], mean= as.numeric(x%*%b), sd= sqrt(s2), log=FALSE)
      
      sum_llik<- sum_llik + llik
      sum_lik<- sum_lik + lik
    }
  }
  save(sum_lik,sum_llik,file=paste0("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/sum_likAndsum_llik",h,".RData"))
}

#WAIC1<- 2*log(sum_lik/M) - (4/M)*sum_llik

stopCluster(cl)

#finish computing WAIC

rm(list=ls())

load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

setwd("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output")
load("samples_cfmpw_fast.RData")
load("time_cfmpw_fast.RData")

N= nrow(XMat)
M= 2e4

sum_llik0<- 0
sum_lik0<- 0

for(h in 0:25){
  load(paste0("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/sum_likAndsum_llik",h,".RData"))
  
  sum_llik0<- sum_llik0 + sum_llik
  sum_lik0<- sum_lik0 + sum_lik
  
}

WAIC1<- 2*log(sum_lik0/M) - (4/M)*sum_llik0

save(WAIC1,file="/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/WAIC1_CIModel.RData")
