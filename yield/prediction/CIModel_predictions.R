#get future predictions

rm(list=ls())

library(dplyr)
library(foreach)
library(doParallel)

setwd("/storage/work/svr5482")

load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
remove<-which(colnames(XMat)=="state1yr"):which(colnames(XMat)=="state55yr_sq")
XMat<- XMat[,-remove]

load("Climate_CornYield-me/yield/Bayesian/CI/output/detrend/samples_cfmpw_fast.RData")
#load("Climate_CornYield-me/Bayesian/CI/output/time_cfmpw_fast.RData")
res<- res[1:12001,]

beta_cols<- 1:ncol(XMat)
#betaNT_cols<- beta_cols[-c(which(colnames(XMat)=="state1yr"):which(colnames(XMat)=="state55yr_sq"))]

#res_beta<- res[,1:ncol(XMat)]
res_s2<- res[,ncol(res)]
res_beta<- res[,beta_cols]

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
    restInd<- which(colnames(XMatBase)=="fips01003") #used to be "state1yr"
    
    XMatCV<- as.matrix(cbind(1,dat1,XMatBase[,restInd:ncol(XMatBase)])); rm(dat1, XMatBase)
    
    allPreds<- matrix(NA,nrow=nrow(XMatCV),ncol=5)
    
    for(t in 1:nfutureyrs){
      time1<- Sys.time()
      predmat_test<- XMatCV[(nCounties*(t-1)+1):(nCounties*t),]%*%t(res_beta)
      meanpred_test<- rowMeans(predmat_test)
      
      meanlowbound<- rowMeans(predmat_test)- 1.96*msqrt_s2
      meanhighbound<- rowMeans(predmat_test)+ 1.96*msqrt_s2
      
      allPreds[(nCounties*(t-1)+1):(nCounties*t),1]<- meanpred_test
      allPreds[(nCounties*(t-1)+1):(nCounties*t),2:3]<- cbind(apply(predmat_test,1, q.05),apply(predmat_test,1, q.95))
      allPreds[(nCounties*(t-1)+1):(nCounties*t),4:5]<- cbind(meanlowbound,meanhighbound)
      time<- Sys.time()-time1
    }
    
    save(allPreds,file=paste("Climate_CornYield-me/yield/Bayesian/CI/output/",modelnames[k],"_",yr,"_predMat.RData",sep=""))
    #time<- Sys.time()- time1
  }
}

time<- Sys.time()- time1
save(time,file=paste("Climate_CornYield-me/yield/Bayesian/CI/output/futurepredtime.RData",sep=""))

stopCluster(cl)

for(k in 1:18){
  print(k)
  for(l in 1:2){
    if (l==1){
      yr<-"2020_2049"
      yr1<- 2020
    }
    if (l==2){
      yr<-"2070_2099"
      yr1<-2069
    }
    print(l)
    load(paste("Climate_CornYield-me/yield/Bayesian/CI/output/",modelnames[k],"_",yr,"_predMat.RData",sep=""))
    
  }
}
