#simple Bayesian model with Schlenker and Roberts' mean function

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian")}

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI")}

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/detrend")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/detrend")}

rm(list=ls())

library(fields); library(mvtnorm); library(invgamma); library(spam); library(emulator)

library(foreach)
library(doParallel)

setwd("/storage/work/svr5482")

load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
load(paste("Climate_CornYield-me/yield/Bayesian/CI/output/obsNoTimeCI.RData",sep=""))

#get rid of county fixed effects
remove<-which(colnames(XMat)=="state1yr"):which(colnames(XMat)=="state55yr_sq")
XMat<- XMat[,-remove]

obsModLinear<- obsNoTimeCI[1:length(obsModLinear)]

# Remove unecessary objects
#rm(distMatCVList, distMatModList)
##rm(distMatHaversin) ; rm(distMatHaversin2)
#rm(test_data,train_data)
#rm(ifyouaintfirst) ; rm(yourelast)
#rm(comboLocation) ; rm(dat)
##################################################################################################
##################################################################################################
# Turn Design matrices into spam objects
format(object.size(XMat), units="KB") #663.2 Mb
XMat<-as.spam(XMat)
format(object.size(XMat), units="KB") #4.4 Mb
format(object.size(XMatCV), units="KB")
XMatCV<-as.spam(XMatCV)
format(object.size(XMatCV), units="KB")

tXX<- crossprod(XMat)
format(object.size(tXX), units="KB")
tyX<- crossprod(obsModLinear,XMat)
format(object.size(tyX), units="KB")
tXy<- t(tyX)
format(object.size(tXy), units="KB")
tyy<- crossprod(obsModLinear)

#set constant
n<-nrow(XMat) #number of observations
k<-ncol(XMat) #number of coefficients to estimate

nrowX<-nrow(XMat)
ncolX<- ncol(XMat)

#set up posteriors to sample from

#Code to generate a random sample from the posterior distribution of beta

rpostBeta<-function(sigma2){
  priorBeta<-c(0,100) # mean and variance of priors
  #foo<-(1/sigma2)*tXX+(1/priorBeta[2])*diag(ncolX) # Precision Matrix
  spamfoo<-as.spam((1/sigma2)*tXX+(1/priorBeta[2])*diag.spam(k)) # Precision Matrix
  #covMat<-solve(foo) # Faster way to compute the inverse of a matrix
  covMat<-chol2inv(chol(spamfoo))
  mu<-(1/sigma2)*tcrossprod(covMat,tyX)
  #rmvnorm.prec(n=1, mu=mu, Q=foo)
  rmvnorm.prec(n=1, mu=mu, Q=spamfoo)
}

#Code to generate a random sample from the posterior distribution of sigma2

rpost_sigma2<- function(beta){
  prior<-c(0.2,0.2)
  sh= nrowX/2 + prior[1]
  #ra= .5*(tyy+quad.form(tXX,beta)-2*crossprod(tXy,beta)) + prior[2]
  ra= .5*(tyy+crossprod(beta,crossprod(tXX,beta))-2*crossprod(tXy,beta)) + prior[2]
  #crossprod(beta,crossprod(tXX,beta))
  invgamma::rinvgamma(1,shape=sh,rate=ra) 
}

#run the code for the Gibbs sampler#Code the Gibbs sampler.
gibbs <- function(init, n.sample) {
  x1.t <- init[1:ncolX] #beta
  x2.t <- init[ncolX+1] #sigma2
  
  x1.out <- matrix(NA,nrow=n.sample+1,ncol=ncolX)
  x2.out <- rep(NA,n.sample+1)
  
  x1.out[1,]= x1.t
  x2.out[1]= x2.t
  
  for (i in 1 : n.sample) {
    #print(i)
    if(i%%1000==0){print(paste("Iteration",i))}
    #sample from the posterior distributions
    # system.time({
    x1.out[i+1,] <-rpostBeta(sigma2 = x2.out[i])@entries
    # })
    
    ############################################################################
    # system.time({
    x2.out[i+1] <-rpost_sigma2(beta=as.vector(x1.out[i+1,]))
    # })
    ############################################################################
    
    
  }
  out <- cbind(x1.out, x2.out)
  out
}


prior_s2<-c(0.2,0.2)

init_vals<- c(rep(0,ncolX),1) 

set.seed(625)
pt<-proc.time() # Start Time
res <- gibbs(init = init_vals, n.sample = 2e4)
#resfast <- gibbs(init = init_vals, n.sample = 100)
ptFinal<-proc.time()-pt ; ptFinal<-ptFinal[3] # End Time to be used in Effective Samples per Second Calculation

setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/detrend",sep=""))
save(res,file="samples_cfmpw_fast.RData")
save(ptFinal,file="time_cfmpw_fast.RData")

#cores=detectCores()
#cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
#registerDoParallel(cl)

#foreach(start = 1:5)%dopar%{

#  if(start<5){
#    library(fields); library(mvtnorm); library(invgamma); library(spam); library(emulator)

#    if(dir.exists(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/start",start,sep=""))==F){
#      dir.create(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/start",start,sep=""))}

#    set.seed(start)
#    init_s2<- invgamma::rinvgamma(1,shape=.2,rate=.2)
#    init_beta<- rnorm(k,mean=0,sd=10)
#    init_vals<- c(init_beta,init_s2) #when >1 phi 

#    set.seed(625+start)
#    pt<-proc.time() # Start Time
#    res <- gibbs(init = init_vals, n.sample = 5e4)
#    ptFinal<-proc.time()-pt ; ptFinal<-ptFinal[3] # End Time to be used in Effective Samples per Second Calculation

#    setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/start",start,sep=""))
#    save(res,file="samples_cfmpw_allyrs.RData")
#    save(ptFinal,file="time_cfmpw_allyrs.RData") 
#  }

#  if(start==5){
#    library(fields); library(mvtnorm); library(invgamma); library(spam); library(emulator)
#    init_vals<- c(rep(0,ncolX),1) 

#    set.seed(625)
#    pt<-proc.time() # Start Time
#    res <- gibbs(init = init_vals, n.sample = 7e4)
#    resfast <- gibbs(init = init_vals, n.sample = 100)
#    ptFinal<-proc.time()-pt ; ptFinal<-ptFinal[3] # End Time to be used in Effective Samples per Second Calculation

#    setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output",sep=""))
#    save(res,file="samples_cfmpw_fast.RData")
#    save(ptFinal,file="time_cfmpw_fast.RData")
#  }

#}


#stopCluster(cl)

setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/detrend",sep=""))
load("samples_cfmpw_fast.RData")
load("time_cfmpw_fast.RData")

res<- res[1:12001,]


library(batchmeans)

ESSvals<- rep(NA,ncol(res))

for(i in 1:ncol(res)){
  ESSvals[i]<- ess(res[,i])
}

summary(ESSvals)

mean(ESSvals)/ptFinal; min(ESSvals)/ptFinal

plot(1:nrow(res),res[,1],type="l")
plot(1:nrow(res),res[,2],type="l")
plot(1:nrow(res),res[,3],type="l")
plot(1:nrow(res),res[,4],type="l")
plot(1:nrow(res),res[,5],type="l")
for(i in 1:20){
  plot(1:nrow(res),res[,i],type="l",main=paste("variable ",i,sep=""))
}
