#look at results from SRMeanModel.R

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/plots")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/plots")}

rm(list=ls())
setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output",sep=""))
load("samples_cfmpw_TWX.RData")
load("time_cfmpw_TWX.RData")

library(batchmeans)

for(l in seq(10000,15000,by=1000)){
  print(l)
  ess_vals<- rep(NA,ncol(res))
  for(i in 1:ncol(res)) ess_vals[i]<- ess(res[500:l,i],imse=FALSE)
  print(min(ess_vals))
}
#need at least 15000 based on ess

ess_vals<- rep(NA,ncol(res))
for(i in 1:ncol(res)) ess_vals[i]<- ess(res[,i],imse=FALSE)
summary(ess_vals)


ess_vals_imse<- rep(NA,ncol(res))
for(i in 1:ncol(res)) ess_vals_imse[i]<- ess(res[,i],imse=TRUE)
summary(ess_vals_imse)

mean(ess_vals_imse)/ptFinal; min(ess_vals_imse)/ptFinal

#check for chain convergence

for(i in 7:20){
  minlength<- i*1000
  meanfrac<- function(x) mean(x[500:(minlength/2)])/mean(x[500:minlength])
  meanfracs<- apply(res,2,meanfrac)
  print(minlength)
  print(quantile(meanfracs,probs=c(0,.005,.025,.5,.975,.995,1)))
}


badinds<- c(which(meanfracs<.95),which(meanfracs>1.05))
#for(i in badinds){
#  plot(1:nrow(res),res[,i],type="l",main=paste("variable ",i,sep=""))
#  lines(1:minlength,res[1:minlength,i],type="l",col="red")
#}

for(i in badinds){
  plot(1:500,res[1:500,i],type="l",main=paste("variable ",i,sep=""))
  abline(h=mean(res[51:500,i]),col="red")
  #lines(1:minlength,res[1:minlength,i],type="l",col="red")
}#looks like everything has converged by 50-100 steps

for(i in badinds){
  print(paste("Variable ",i," all steps: ",mean(res[50:nrow(res),i]),sep=""))
  print(paste("Variable ",i," first ",minlength, " steps: ",mean(res[50:minlength,i]),sep=""))
}

#what's the ESS if we only consider steps 101 to 20000?
#library(batchmeans)
ess_vals_minlength<- rep(NA,ncol(res))
for(i in 1:ncol(res)) ess_vals_minlength[i]<- ess(res[101:minlength,i],imse=FALSE)
summary(ess_vals_minlength) #min= 14937

ess_vals_minlength<- rep(NA,ncol(res))
for(i in 1:ncol(res)) ess_vals_minlength[i]<- ess(res[101:minlength,i],imse=TRUE)
summary(ess_vals_minlength) #min=17456 


#what about 10,000 steps?
ess_vals_minlength<- rep(NA,ncol(res))
for(i in 1:ncol(res)) ess_vals_minlength[i]<- ess(res[101:(minlength/2),i],imse=TRUE)
summary(ess_vals_minlength) #min=17456 

#first 5 variables: intercept, Pr_GS, Pr_GS2, wsum1, and wsum2
for(i in 1:5){
  hist(res[,i],main=paste("variable ",i,sep=""))
}

for(i in 1:5){
  plot(1:nrow(res),res[,i],main=paste("variable ",i,sep=""),type="l")
}


allPreds<- matrix(NA,nrow=nrow(XMat)+nrow(XMatCV),ncol=3)

q.05 <- function(x) { as.numeric(quantile(x, probs = c(.05))) }
q.95 <- function(x) { as.numeric(quantile(x, probs = c(.95))) }

for(i in 1:nyrs){
  if(i<=ntrainyrs){
    predmat_train<- XMat[(cs.obs.train[i]+1):cs.obs.train[i+1],]%*%t(res_beta)
    meanpred_train<- rowMeans(predmat_train)
    
    allPreds[which(timeloc.ord$year==(1980+i)),1]<- meanpred_train
    allPreds[which(timeloc.ord$year==(1980+i)),2:3]<- cbind(apply(predmat_train,1, q.05),apply(predmat_train,1, q.95))
    
  }
  if(i>ntrainyrs){
    predmat_test<- XMatCV[(cs.obs.test[i-ntrainyrs]+1):cs.obs.test[i-ntrainyrs+1],]%*%t(res_beta)
    meanpred_test<- rowMeans(predmat_test)
    
    allPreds[which(timeloc.ord$year==(1980+i)),1]<- meanpred_test
    allPreds[which(timeloc.ord$year==(1980+i)),2:3]<- cbind(apply(predmat_test,1, q.05),apply(predmat_test,1, q.95))
    
  }
  
}



save(allPreds,file="Climate_CornYield-me/Bayesian/CI/output/allPreds.RData")

stopCluster(cl)
