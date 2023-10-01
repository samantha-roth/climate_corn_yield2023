#get future predictions

rm(list=ls())

library(dplyr)
library(foreach)
library(doParallel)


if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output")}

cutoff= .2
rankM= 20

setwd("/storage/work/svr5482")
load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
#get rid of county fixed effects
remove<-which(colnames(XMat)=="state1yr"):ncol(XMat)
#remove<-which(colnames(XMat)=="state1yr"):ncol(XMat)
XMat<- XMat[,-remove]

#get rid of county fixed effects
remove<-which(colnames(XMatCV)=="state1yr"):ncol(XMatCV)
#remove<-which(colnames(XMatCV)=="state1yr"):ncol(XMatCV)
XMatCV<- XMatCV[,-remove]

load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/detrend/samples_cfmpw_allyrs.RData")
#load("Climate_CornYield-me/PICAR/timeConstant/noCIs/cutoff.2/rank20/time_cfmpw_allyrs.RData")
res<- res[1:21001,]

beta_cols<- 1:ncol(XMat)
#betaNT_cols<- beta_cols[-c(which(colnames(XMat)=="state1yr"):which(colnames(XMat)=="state55yr_sq"))]

res_beta<- res[,beta_cols] 
res_delta<- res[,ncol(XMat)+1:rankM] 
res_s2<- res[,ncol(res)-1]
res_tau2<- res[,ncol(res)]
rm(XMat,XMatCV,res)

load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AMat_allyrs_cutoff",cutoff,".RData",sep=""))
load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/MOE_cutoff",cutoff,".RData",sep=""))

mBase<- as.matrix(AMat)%*%as.matrix(MoransOperatorEig$vectors[,1:rankM])

AMdelta<- mBase%*%t(res_delta)

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
    
    #load(paste("Climate_CornYield-me/PICAR/holdLastYear/future/Crop_",yr,"_cfmpwSpatialData.RData",sep=""))
    
    load(paste("Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/",modelnames[k],"/",yr,"/tpr.RData",sep=""))
    
    #print(length(which(is.na(dat1))))
    
    #XMatBase<- XMatCV; rm(XMatCV)
    #restInds<- which(colnames(XMatBase)=="state1yr"):which(colnames(XMatBase)=="state55yr_sq")
    
    #XMatCV<- as.matrix(cbind(1,dat1,XMatBase[,restInds])); rm(dat1, XMatBase)
    XMatCV<- as.matrix(cbind(1,dat1)); rm(dat1)
    
    allPreds<- matrix(NA,nrow=nrow(XMatCV),ncol=5)
    
    for(t in 1:nfutureyrs){
      #time1<- Sys.time()
      predmat_test<- XMatCV[(nCounties*(t-1)+1):(nCounties*t),]%*%t(res_beta) + AMdelta
      meanpred_test<- rowMeans(predmat_test)
      
      meanlowbound<- rowMeans(predmat_test)- 1.96*msqrt_s2
      meanhighbound<- rowMeans(predmat_test)+ 1.96*msqrt_s2
      
      allPreds[(nCounties*(t-1)+1):(nCounties*t),1]<- meanpred_test
      allPreds[(nCounties*(t-1)+1):(nCounties*t),2:3]<- cbind(apply(predmat_test,1, q.05),apply(predmat_test,1, q.95))
      allPreds[(nCounties*(t-1)+1):(nCounties*t),4:5]<- cbind(meanlowbound,meanhighbound)
      #time<- Sys.time()-time1
    }
    
    save(allPreds,file=paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/",modelnames[k],"_",yr,"_predMat.RData",sep=""))
    #time<- Sys.time()- time1
  }
}

time<- Sys.time()- time1
save(time,file=paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/futurepredtime.RData",sep=""))

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
    load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/",modelnames[k],"_",yr,"_predMat.RData",sep=""))
    
  }
}
