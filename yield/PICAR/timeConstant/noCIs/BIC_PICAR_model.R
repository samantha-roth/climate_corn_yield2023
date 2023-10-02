#fit the model with the delta corresponding to each column of Phi following an AR(1) process
#independently of the other deltas

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs")}

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2")}

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20")}


rm(list=ls())

library(fields); library(mvtnorm); library(invgamma); library(spam)

library(foreach)
library(doParallel)

#load data
load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

#get rid of county fixed effects
remove<-which(colnames(XMat)=="fips01003"):ncol(XMat)
XMat<- XMat[,-remove]

#cutoff selected by AIC
cutoff=0.2
#rank selected by AIC
rankM=20

load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/AMat_allyrs_cutoff",cutoff,".RData",sep=""))
load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/difA_sameMBase/holdLastYear/MOE_cutoff",cutoff,".RData",sep=""))

M1<- as.matrix(MoransOperatorEig$vectors[,1:rankM])
AMat<- as.matrix(AMat)

mBase<- AMat%*%M1

ufips<- unique(timeloc.ord$fips)

M<- matrix(NA, nrow=nrow(XMat), ncol= rankM)

for(i in 1:ntrainyrs){
  timelocyr<- timeloc.ord[(cs.obs.train[i]+1):cs.obs.train[i+1],]
  fipsinds<- which(ufips%in%(timelocyr$fips))
  M[(cs.obs.train[i]+1):cs.obs.train[i+1],]<- mBase[fipsinds,]
}

# Remove unecessary objects
rm(distMatCVList) ; rm(distMatModList)
#rm(distMatHaversin) ; rm(distMatHaversin2)
rm(test_data); rm(train_data) ;
rm(ifyouaintfirst) ; rm(yourelast)
rm(comboLocation) ; rm(dat)
rm(timeloc.ord); rm(timelocyr)
rm(gridLocation,CVgridLocation)
##################################################################################################
##################################################################################################
# Turn Design matrices into spam obkects
format(object.size(XMat), units="MB") #663.2 Mb
XMat<-as.spam(XMat)
format(object.size(XMat), units="MB") #4.4 Mb
format(object.size(XMatCV), units="MB")
XMatCV<-as.spam(XMatCV)
format(object.size(XMatCV), units="MB")
format(object.size(M), units="MB") #0.4 Gb
M<-as.spam(M)
format(object.size(M), units="MB") #16.6Mb
M1<-as.spam(M1)
format(object.size(M1), units="MB") #16.6Mb
################################################################################

#calculate some things ahead of time to save computation time
tX<-t(XMat)
format(object.size(tX), units="MB")

#tXX<-t(XMat)%*%XMat
tXX<- tcrossprod(tX)
format(object.size(tXX), units="MB")
tM<-t(M)
format(object.size(tM), units="MB")
tMM<- crossprod(M)
format(object.size(tMM), units="MB")
#tMM<-t(M)%*%M
ty<- t(obsModLinear)

#my added calculations
#tMy<- tM%*%obsModLinear
tMy<- tcrossprod(tM,ty)
format(object.size(tMy), units="MB")
#tMX<- tM%*%XMat
tMX<- tcrossprod(tM,tX)
format(object.size(tMX), units="MB")
#tXy<- tX%*%obsModLinear
tXy<- tcrossprod(tX,ty)
format(object.size(tXy), units="MB")
tXM<- t(tMX)
format(object.size(tXM), units="MB")
tyy<- tcrossprod(ty)
tyX<- t(tXy)
tyM<- t(tMy)
format(object.size(tMy), units="MB")

tM1M1<- crossprod(M1)

################################################################################

#set constant
p<-ncol(M)
n<-nrow(XMat) #number of observations
k<-ncol(XMat) #number of coefficients to estimate
################################################################################
################################################################################


rpostDelta<-function(tau2, sigma2, beta){
  #p<- ncol(tMM)
  
  spamfoo<-as.spam(tM1M1/tau2+tMM/sigma2)
  # spamfoo<- (foo)
  spamCovMat<- spam::solve(spamfoo)
  #covMat<-chol2inv(chol(spamfoo)) # Faster way to compute the inverse of a matrix
  
  mu<-tcrossprod(spamCovMat,t((1/sigma2)*(tMy-tcrossprod(tMX,t(beta)))))
  rmvnorm.prec(n=1, mu=mu, Q= spamfoo )
  #result<- mu+t(spam::chol(spamCovMat))%*%rnorm(p) # Samples from a multivariate normal with mean mu and covariance matrix covMat
  #ptFinalDSample<-proc.time()-pt ; ptFinalDSample<-ptFinalDSample[3]
  
}

rpostBeta<-function(sigma2, delta){
  # system.time({
  priorBeta<-c(0,100) # mean and variance of priors
  foo<-(1/sigma2)*tXX+(1/priorBeta[2])*diag.spam(k) # Precision Matrix
  # system.time({
  # cholCov<-spam::chol(foo)
  # covMat<-as.spam(chol2inv(cholCov)) # Faster way to compute the inverse of a matrix
  # })
  covMat<-solve(foo) # Faster way to compute the inverse of a matrix
  mu<-tcrossprod(covMat,t((1/sigma2)*(tXy-crossprod(tMX,delta))))
  rmvnorm.prec(n=1, mu=mu, Q=foo)
  # })
  # mu+(as.spam(spam::chol(covMat))%*%rnorm(k))
  # })
}

#Code to generate a random sample from the posterior distribution of sigma2
rpost_sigma2<- function(beta, delta){
  prior<-c(0.2,0.2)
  sh= nrow(XMat)/2 + prior[1]
  ra= .5*(tyy + crossprod(beta,crossprod(tXX,beta)) + crossprod(delta,crossprod(tMM,delta)) - 
            2*crossprod(tXy,beta)-2*crossprod(tMy,delta) + 2*crossprod(beta,crossprod(tMX,delta))) + prior[2]
  invgamma::rinvgamma(1,shape=sh,rate=ra) 
}


rpost_tau2<- function(beta, delta){ 
  prior<-c(0.2,0.2)
  sh= length(delta)/2 +prior[1]
  ra= .5*crossprod(delta,tcrossprod(tM1M1,t(delta))) + prior[2]
  invgamma::rinvgamma(1,shape=sh,rate=ra) 
}


yrvec<- 1:ntrainyrs
#distyrs<- as.matrix(dist(yrvec,method="manhattan"))

################################################################################
#run the code for the Gibbs sampler#Code the Gibbs sampler.
gibbs <- function(init, n.sample) {
  x1.t <- init[1:ncol(XMat)] #beta
  x2.t <- init[(ncol(XMat)+1):(ncol(XMat)+ncol(M))] #delta
  x3.t <- init[ncol(XMat)+ncol(M)+1] #sigma2
  x4.t <- init[ncol(XMat)+ncol(M)+2] #tau2
  
  x1.out <- matrix(NA,nrow=n.sample+1,ncol=ncol(XMat))
  x2.out <- matrix(NA,nrow=n.sample+1,ncol=ncol(M))  
  x3.out <- rep(NA,n.sample+1)
  x4.out <- rep(NA,n.sample+1)
  
  x1.out[1,]= x1.t
  x2.out[1,]= x2.t
  x3.out[1]= x3.t
  x4.out[1]= x4.t
  
  for (i in 1 : n.sample) {
    #print(i)
    if(i%%1000==0){print(paste("Iteration",i))}
    #sample from the posterior distributions
    # system.time({
    x1.out[i+1,] <-rpostBeta(sigma2 = x3.out[i], delta=as.vector(x2.out[i,]))
    # })
    
    #FOR DELTAS ~ AR(1) PROCESS
    
    ##############################################################################
    ##############################################################################
    # Cut down time here
    # system.time({
    x2.out[i+1,]<-rpostDelta(tau2=x4.out[i],
                             sigma2=x3.out[i],
                             beta=as.vector(x1.out[i+1,]))
    # })
    #############################################################################
    ##############################################################################
    # system.time({
    x3.out[i+1] <-rpost_sigma2(beta=as.vector(x1.out[i+1,]),
                               delta=as.vector(x2.out[i+1,]))
    # })
    ################################################################
    ################################################################
    # system.time({
    x4.out[i+1] <-rpost_tau2(beta=as.vector(x1.out[i+1,]),
                             delta=as.vector(x2.out[i+1,]))
    # })
    
  }
  out <- cbind(x1.out, x2.out,x3.out,x4.out)
  out
}


#init_vals<- c(rep(0,k),rep(0,ncol(M)),1,1) #when >1 phi 

#set.seed(625)
#pt<-proc.time() # Start Time
#res <- gibbs(init = init_vals, n.sample = 5e4)
#ptFinal<-proc.time()-pt ; ptFinal<-ptFinal[3] # End Time to be used in Effective Samples per Second Calculation

#setwd(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/timeConstant/noCIs/cutoff.2/rank20",sep=""))
#save(res,file="samples_cfmpw_allyrs.RData")
#save(ptFinal,file="time_cfmpw_allyrs.RData")

#essvals<- rep(NA,ncol(res))
#for(i in 1:ncol(res)) essvals[i]<- ess(res[,i])

cores=detectCores()
cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
registerDoParallel(cl)

foreach(start = 1:5)%dopar%{
  if(start < 5){
    library(fields); library(mvtnorm); library(invgamma); library(spam); library(emulator)
    
    if(dir.exists(paste("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/start",start,sep=""))==F){
      dir.create(paste("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/start",start,sep=""))}
    
    set.seed(start)
    init_s2<- invgamma::rinvgamma(1,shape=.2,rate=.2)
    init_t2<- invgamma::rinvgamma(1,shape=.2,rate=.2)
    init_beta<- rnorm(k,mean=0,sd=10)
    init_delta<- rmvnorm.prec(n=1, mu=rep(0,p), Q=(1/init_t2)*tMM )
    init_vals<- c(init_beta,as.numeric(init_delta),init_s2,init_t2) #when >1 phi 
    
    set.seed(625+start)
    pt<-proc.time() # Start Time
    res <- gibbs(init = init_vals, n.sample = 5e4)
    ptFinal<-proc.time()-pt ; ptFinal<-ptFinal[3] # End Time to be used in Effective Samples per Second Calculation
    
    setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/start",start,sep=""))
    save(res,file="samples_cfmpw_allyrs.RData")
    save(ptFinal,file="time_cfmpw_allyrs.RData")
    
  }
  if(start==5){
    library(fields); library(mvtnorm); library(invgamma); library(spam); library(emulator)
    
    init_vals<- c(rep(0,k),rep(0,ncol(M)),1,1) #when >1 phi 
    
    set.seed(625)
    pt<-proc.time() # Start Time
    res <- gibbs(init = init_vals, n.sample = 5e4)
    ptFinal<-proc.time()-pt ; ptFinal<-ptFinal[3] # End Time to be used in Effective Samples per Second Calculation
    
    setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20",sep=""))
    save(res,file="samples_cfmpw_allyrs.RData")
    save(ptFinal,file="time_cfmpw_allyrs.RData")
    
  }
  
}

stopCluster(cl)

library(batchmeans)

ESSvals<- rep(NA,ncol(res))

for(i in 1:ncol(res)){
  ESSvals[i]<- ess(res[,i])
}

mean(ESSvals)/ptFinal; min(ESSvals)/ptFinal


setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20",sep=""))
load("samples_cfmpw_allyrs.RData")
load("time_cfmpw_allyrs.RData")
