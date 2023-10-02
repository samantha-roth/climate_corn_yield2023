#look at results from SRMeanModel.R

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/plots")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/plots")}

rm(list=ls())
setwd("/storage/work/svr5482")
load("Climate_CornYield-me/yield/Bayesian/CI/output/samples_cfmpw_TWX.RData")

#residuals when subtracting average prediction from yield

load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwTWX.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

nyrs= 36
ntrainyrs= 34
ntestyrs=2

res_beta<- res[,1:ncol(XMat)]
res_s2<- res[,ncol(res)]

#cores=detectCores()
#cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
#registerDoParallel(cl)


allPreds<- matrix(NA,nrow=nrow(XMat)+nrow(XMatCV),ncol=3)

q.05 <- function(x) { as.numeric(quantile(x, probs = c(.05))) }
q.95 <- function(x) { as.numeric(quantile(x, probs = c(.95))) }

#foreach(i = 1:nyrs)%dopar%{
for(i in 1:nyrs){
  time1<- Sys.time()
  if(i<=ntrainyrs){
    time1<- Sys.time()
    predmat_train<- XMat[(cs.obs.train[i]+1):cs.obs.train[i+1],]%*%t(res_beta)
    meanpred_train<- rowMeans(predmat_train)
    
    allPreds[which(timeloc.ord$year==(1980+i)),1]<- meanpred_train
    allPreds[which(timeloc.ord$year==(1980+i)),2:3]<- cbind(apply(predmat_train,1, q.05),apply(predmat_train,1, q.95))
    time<- Sys.time()- time1
  }
  if(i>ntrainyrs){
    predmat_test<- XMatCV[(cs.obs.test[i-ntrainyrs]+1):cs.obs.test[i-ntrainyrs+1],]%*%t(res_beta)
    meanpred_test<- rowMeans(predmat_test)
    
    allPreds[which(timeloc.ord$year==(1980+i)),1]<- meanpred_test
    allPreds[which(timeloc.ord$year==(1980+i)),2:3]<- cbind(apply(predmat_test,1, q.05),apply(predmat_test,1, q.95))
    
  }
  time<- Sys.time()- time1
}

save(allPreds,file="Climate_CornYield-me/yield/Bayesian/CI/output/allPreds_TWX.RData")

#load("Climate_CornYield-me/yield/Bayesian/CI/output/allPreds_TWX.RData")

#stopCluster(cl)
