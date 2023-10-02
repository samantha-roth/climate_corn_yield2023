#simple Bayesian model with Schlenker and Roberts' mean function

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian")}

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI")}

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output")}

rm(list=ls())

library(fields); library(mvtnorm); library(invgamma); library(spam); library(emulator)

#library(foreach)
#library(doParallel)

setwd("/storage/work/svr5482")

load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwTWX.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
timeloc.ord<- timeloc.ord[which(timeloc.ord$year%in%seq(1981,2016,by=1)),]

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
res <- gibbs(init = init_vals, n.sample = 7e4)
#res <- gibbs(init = init_vals, n.sample = 100)
ptFinal<-proc.time()-pt ; ptFinal<-ptFinal[3] # End Time to be used in Effective Samples per Second Calculation

setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output",sep=""))
save(res,file="samples_cfmpw_TWX.RData")
save(ptFinal,file="time_cfmpw_TWX.RData")


#res<- res[1:30001,]
#ptFinal<- 3*ptFinal/7

#setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output",sep=""))
#save(res,file="samples_cfmpw_TWX.RData")
#save(ptFinal,file="time_cfmpw_TWX.RData")


#setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output",sep=""))
#load("samples_cfmpw_TWX.RData")
#load("time_cfmpw_TWX.RData")
