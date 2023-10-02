#WAIC code

rm(list=ls())

library(foreach)
library(doParallel)

load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwTWX.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

setwd("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output")
load("samples_cfmpw_TWX.RData")

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

save(sum_lik,sum_llik,file=paste0("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/sum_likAndsum_llik",25,"_TWX.RData"))

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
  save(sum_lik,sum_llik,file=paste0("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/sum_likAndsum_llik",h,"_TWX.RData"))
}

#WAIC1<- 2*log(sum_lik/M) - (4/M)*sum_llik

stopCluster(cl)
