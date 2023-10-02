#look at predictions from AIC_PICAR_model.R

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/plots")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/plots")}

rm(list=ls())
setwd("/storage/work/svr5482")
load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/samples_cfmpw_allyrs.RData")
load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/time_cfmpw_allyrs.RData")


library(foreach)
library(doParallel)

#residuals when subtracting average prediction from yield


load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

#get rid of county fixed effects
remove<-which(colnames(XMat)=="fips01003"):ncol(XMat)
XMat<- XMat[,-remove]

#get rid of county fixed effects
remove<-which(colnames(XMatCV)=="fips01003"):ncol(XMatCV)
XMatCV<- XMatCV[,-remove]

# Remove unecessary objects
rm(distMatCVList, distMatModList)
#rm(distMatHaversin) ; rm(distMatHaversin2)
rm(test_data,train_data)
rm(ifyouaintfirst) ; rm(yourelast)
rm(comboLocation) ; rm(dat)

cutoff<-.2

load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AMat_allyrs_cutoff",cutoff,".RData",sep=""))
load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/MOE_cutoff",cutoff,".RData",sep=""))

rankM<- 20

mBase<- as.matrix(AMat)%*%as.matrix(MoransOperatorEig$vectors[,1:rankM])

ufips<- unique(timeloc.ord$fips)

M<- matrix(NA, nrow=nrow(XMat), ncol= rankM)

for(i in 1:ntrainyrs){
  timelocyr<- timeloc.ord[(cs.obs.train[i]+1):cs.obs.train[i+1],]
  fipsinds<- which(ufips%in%(timelocyr$fips))
  M[(cs.obs.train[i]+1):cs.obs.train[i+1],]<- mBase[fipsinds,]
}

MCV<- matrix(NA, nrow=nrow(XMatCV), ncol= rankM)

for(i in 1:ntestyrs){
  timelocyr<- timeloc.ord[which(timeloc.ord$year==1980+ntrainyrs+i),]
  fipsinds<- which(ufips%in%(timelocyr$fips))
  MCV[(cs.obs.test[i]+1):cs.obs.test[i+1],]<- mBase[fipsinds,]
}


res_beta<- res[,1:ncol(XMat)]
res_delta<- res[,ncol(XMat)+(1:rankM)]

#res_s2<- res[,ncol(res)]

#cores=detectCores()
#cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
#registerDoParallel(cl)

allPreds<- matrix(NA,nrow=nrow(XMat)+nrow(XMatCV),ncol=3)

q.05 <- function(x) { as.numeric(quantile(x, probs = c(.05))) }
q.95 <- function(x) { as.numeric(quantile(x, probs = c(.95))) }

for(i in 1:nyrs){
  #foreach(i = 1:nyrs)%dopar%{
  if(i<=ntrainyrs){
    predmat_train<- XMat[(cs.obs.train[i]+1):cs.obs.train[i+1],]%*%t(res_beta)+
      M[(cs.obs.train[i]+1):cs.obs.train[i+1],]%*%t(res_delta)
    meanpred_train<- rowMeans(predmat_train)
    
    
    allPreds[which(timeloc.ord$year==(1980+i)),1]<- meanpred_train
    allPreds[which(timeloc.ord$year==(1980+i)),2:3]<- cbind(apply(predmat_train,1, q.05),apply(predmat_train,1, q.95))
    
  }
  if(i>ntrainyrs){
    predmat_test<- XMatCV[(cs.obs.test[i-ntrainyrs]+1):cs.obs.test[i-ntrainyrs+1],]%*%t(res_beta)+
      MCV[(cs.obs.test[i-ntrainyrs]+1):cs.obs.test[i-ntrainyrs+1],]%*%t(res_delta)
    meanpred_test<- rowMeans(predmat_test)
    
    allPreds[which(timeloc.ord$year==(1980+i)),1]<- meanpred_test
    allPreds[which(timeloc.ord$year==(1980+i)),2:3]<- cbind(apply(predmat_test,1, q.05),apply(predmat_test,1, q.95))
    
  }
  
}

save(allPreds,file="Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/allPreds.RData")


#stopCluster(cl)
