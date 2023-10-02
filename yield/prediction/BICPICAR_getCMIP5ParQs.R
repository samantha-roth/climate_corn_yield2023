#find the 5th and 95th quantiles of the thinned predictions from the BICPICAR model

rm(list=ls())
setwd("/storage/work/svr5482")

library(dplyr)
#library(foreach)
#library(doParallel)

nfutureyrs<- 30
nCounties<- 1821
nThin<- 1000

modelnames<-c("MIROC5","MRI-CGCM3","IPSL-CM5B-LR","IPSL-CM5A-LR", 
              "HadGEM2-ES365","GFDL-ESM2M","GFDL-ESM2G","CSIRO-Mk3-6-0","bcc-csm1-1",
              "MIROC-ESM", "IPSL-CM5A-MR", "CNRM-CM5","BNU-ESM",
              "MIROC-ESM-CHEM", "inmcm4", "HadGEM2-CC365", "CanESM2", "bcc-csm1-1-m")

load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/detrend/samples_cfmpw_allyrs.RData")
res<- res[1:21001,]

#first, thin mcmc samples down to like 1000
thinInds<- seq(from=22,to=nrow(res),by=21)

resThin<- res[thinInds,]
resThin_s2<- resThin[,ncol(resThin)-1]

#this way I can take the 2.5th percentile and 97.5th percentile of each row
#to get a quantile for each county-year observation

qs.025 <- function(x) { as.numeric(quantile(x, probs = c(.025))) }
qs.05 <- function(x) { as.numeric(quantile(x, probs = c(.05))) }
qs.025 <- function(x) { as.numeric(quantile(x, probs = c(.25))) }
qs.5 <- function(x) { as.numeric(quantile(x, probs = c(.5))) }
qs.75 <- function(x) { as.numeric(quantile(x, probs = c(.75))) }
qs.95 <- function(x) { as.numeric(quantile(x, probs = c(.95))) }
qs.975 <- function(x) { as.numeric(quantile(x, probs = c(.975))) }

#cores=detectCores()
#cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
#registerDoParallel(cl)

#foreach (l = 1:2)%dopar%{
for(l in 1:2){  
  load("/storage/work/svr5482/Climate_CornYield-me/SourceData/lastAreaData.RData")

  library(dplyr)
  qs.05 <- function(x) { as.numeric(quantile(x, probs = c(.05))) }
  qs.95 <- function(x) { as.numeric(quantile(x, probs = c(.95))) }
  
  if (l==1){
    yr<-"2020_2049"
    yr1<- 2020
  }
  if (l==2){
    yr<-"2070_2099"
    yr1<-2070
  }
  
  allThinPreds<- matrix(NA,nrow=nfutureyrs*nCounties,ncol=length(modelnames)*nThin)
  
  #time1<- Sys.time()
  #foreach(k = 1:length(modelnames))%dopar%{
  for(k in 1:length(modelnames)){
    print(modelnames[k])
    
    load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/",modelnames[k],"_",yr,"_thinPredMat.RData",sep=""))
    
    allThinPreds[,(nThin*(k-1)+1):(nThin*k)]<- thinPreds
  }
  #time<- Sys.time()-time1
  
  #allyearmeans<- matrix(NA, nrow= nfutureyrs, ncol= nThin*length(modelnames))
  allyearmeanQs<- matrix(NA, nrow= nfutureyrs, ncol= 4)
  allyearmean.975bd<- matrix(NA, nrow= nfutureyrs, ncol= 3)
  allyearmean.025bd<- matrix(NA, nrow= nfutureyrs, ncol= 3)
  
  for(t in 1:nfutureyrs){
    yearpreds<- allThinPreds[(nCounties*(t-1)+1):(nCounties*t),]
    #allyearmeans<- colMeans(yearpreds)
    
    allyearmeans<- rep(NA,ncol(yearpreds))
    #for(col in 1:ncol(allThinPreds)) allyearmeans[col]<- weighted.mean(yearpreds[,col],lastarea)
    for(col in 1:ncol(allThinPreds)) allyearmeans[col]<- sum(yearpreds[,col]*lastarea)/sum(lastarea)
    
    allyearmeanQs[t,]<- c(qs.025(allyearmeans),qs.5(allyearmeans),
                          mean(allyearmeans),qs.975(allyearmeans))
    
    allyearmean.975bd[t,]<- c(qs.5(allyearmeans+1.96*rep(sqrt(resThin_s2),length(modelnames))/nCounties),
                              mean(allyearmeans+1.96*rep(sqrt(resThin_s2),length(modelnames))/nCounties),
                              qs.975(allyearmeans+1.96*rep(sqrt(resThin_s2),length(modelnames))/nCounties))
    
    allyearmean.025bd[t,]<- c(qs.5(allyearmeans-1.96*rep(sqrt(resThin_s2),length(modelnames))/nCounties),
                              mean(allyearmeans-1.96*rep(sqrt(resThin_s2),length(modelnames))/nCounties),
                              qs.025(allyearmeans-1.96*rep(sqrt(resThin_s2),length(modelnames))/nCounties))
  }
  
  save(allyearmeanQs,allyearmean.025bd,allyearmean.975bd,
       file=paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/allyearmeanQs",yr,".RData",sep=""))
  
  
  #ThinPredsQs<- cbind(apply(allThinPreds,1,qs.05),apply(allThinPreds,1,qs.95))
  
  #save(ThinPredsQs,
  #     file=paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/thinPredQs",yr,".RData",sep=""))
  
}
#stopCluster(cl)

#rm(list=ls())
#setwd("/storage/work/svr5482")

yr<-"2020_2049"
#load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/thinPredQs",yr,".RData",sep=""))
load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/allyearmeanQs",yr,".RData",sep=""))
#apply(ThinPredsQs,2,summary)

yr<-"2070_2099"
#load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/thinPredQs",yr,".RData",sep=""))
load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/allyearmeanQs",yr,".RData",sep=""))
#apply(ThinPredsQs,2,summary)
