#thin MCMC samples so I can consider both parametric and climate model uncertainty

#CI model

rm(list=ls())

library(dplyr)
library(foreach)
library(doParallel)


if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output")}

setwd("/storage/work/svr5482")
load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
remove<-which(colnames(XMat)=="state1yr"):which(colnames(XMat)=="state55yr_sq")
XMat<- XMat[,-remove]

load("Climate_CornYield-me/yield/Bayesian/CI/output/detrend/samples_cfmpw_fast.RData")
res<- res[1:12001,]

#first, thin mcmc samples down to like 1000
thinInds<- seq(from=13,to=nrow(res),by=12)

resThin<- res[thinInds,]

beta_cols<- 1:ncol(XMat)
#betaNT_cols<- beta_cols[-c(which(colnames(XMat)=="state1yr"):which(colnames(XMat)=="state55yr_sq"))]

#res_beta<- res[,1:ncol(XMat)]
res_s2<- resThin[,ncol(resThin)]
res_beta<- resThin[,beta_cols]
rm(XMat,XMatCV,res)

modelnames<-c("MIROC5","MRI-CGCM3","IPSL-CM5B-LR","IPSL-CM5A-LR", 
              "HadGEM2-ES365","GFDL-ESM2M","GFDL-ESM2G","CSIRO-Mk3-6-0","bcc-csm1-1",
              "MIROC-ESM", "IPSL-CM5A-MR", "CNRM-CM5","BNU-ESM",
              "MIROC-ESM-CHEM", "inmcm4", "HadGEM2-CC365", "CanESM2", "bcc-csm1-1-m")

nfutureyrs<- 30
nCounties<- 1821

msqrt_s2<- mean(sqrt(res_s2))

q.05 <- function(x) { as.numeric(quantile(x, probs = c(.05))) }
q.95 <- function(x) { as.numeric(quantile(x, probs = c(.95))) }

time1<- Sys.time()

cores=detectCores()
cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
registerDoParallel(cl)

foreach(k = 1:length(modelnames))%dopar%{
#for(k in 1:length(modelnames)){
  print(modelnames[k])
  for (l in 1:2){
    #time1<- Sys.time()
    if (l==1){
      yr<-"2020_2049"
      yr1<- 2020
    }
    if (l==2){
      yr<-"2070_2099"
      yr1<-2069
    }
    
    load(paste("Climate_CornYield-me/PICAR/holdLastYear/future/Crop_",yr,"_cfmpwSpatialData.RData",sep=""))
    
    load(paste("Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/",modelnames[k],"/",yr,"/tpr.RData",sep=""))
    
    #print(length(which(is.na(dat1))))
    
    XMatBase<- XMatCV; rm(XMatCV)
    restInds<- which(colnames(XMatBase)=="fips01003"):ncol(XMatBase)
    
    XMatCV<- as.matrix(cbind(1,dat1,XMatBase[,restInds])); rm(dat1, XMatBase)
    
    thinPreds<- matrix(NA,nrow=nrow(XMatCV),ncol=length(thinInds))
    
    for(t in 1:nfutureyrs){
      predmat_test<- XMatCV[(nCounties*(t-1)+1):(nCounties*t),]%*%t(res_beta)
      thinPreds[(nCounties*(t-1)+1):(nCounties*t),]<- as.numeric(predmat_test)
    }
    
    save(thinPreds,file=paste("Climate_CornYield-me/yield/Bayesian/CI/output/",modelnames[k],"_",yr,"_thinPredMat.RData",sep=""))
    
  }
}

stopCluster(cl)

time<- Sys.time()- time1
save(time,file=paste("Climate_CornYield-me/yield/Bayesian/CI/output/futurethinpredtime.RData",sep=""))


for(k in 1:18){
  print(k)
  for(l in 1:2){
    print(l)
    if (l==1){
      yr<-"2020_2049"
      yr1<- 2020
    }
    if (l==2){
      yr<-"2070_2099"
      yr1<-2069
    }
    
    load(paste("Climate_CornYield-me/yield/Bayesian/CI/output/",modelnames[k],"_",yr,"_thinPredMat.RData",sep=""))
  }
}
