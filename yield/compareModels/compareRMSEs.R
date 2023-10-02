#compare predictive performance of the models fit on the held out and training data

rm(list=ls())

#load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.1/rank400/allPreds.RData")
#CVRMSE_PICAR_allPreds<- allPreds

load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/allPreds.RData")
BIC_PICAR_allPreds<- allPreds

load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.1/rank190/allPreds.RData")
AIC_PICAR_allPreds<- allPreds

load("Climate_CornYield-me/yield/Bayesian/CI/output/allPreds.RData")
CI_allPreds<- allPreds

rm(allPreds)

load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")

#look at RMSE performance on held out data
last4yrsinds<- (length(obsFullLinear)-length(obsCVLinear)+1):length(obsFullLinear)

#RMSE_heldout_CVRMSE_PICAR<- sqrt(mean((obsCVLinear-CVRMSE_PICAR_allPreds[last4yrsinds,1])^2))

RMSE_heldout_AIC_PICAR<- sqrt(mean((obsCVLinear-AIC_PICAR_allPreds[last4yrsinds,1])^2))

RMSE_heldout_BIC_PICAR<- sqrt(mean((obsCVLinear-BIC_PICAR_allPreds[last4yrsinds,1])^2))

RMSE_heldout_CI<- sqrt(mean((obsCVLinear-CI_allPreds[last4yrsinds,1])^2))

#RMSE_heldout_CVRMSE_PICAR
RMSE_heldout_AIC_PICAR
RMSE_heldout_BIC_PICAR
RMSE_heldout_CI

################################################################################
AIC_PICAR_allPreds_last4yrs<- AIC_PICAR_allPreds[last4yrsinds,1]
BIC_PICAR_allPreds_last4yrs<- BIC_PICAR_allPreds[last4yrsinds,1]
CI_allPreds_last4yrs<- CI_allPreds[last4yrsinds,1]

#RMSE_heldout_CVRMSE_PICAR<- sqrt(mean((obsCVLinear-CVRMSE_PICAR_allPreds[last4yrsinds,1])^2))
RMSE_heldout_AIC_PICARyrs<- rep(NA,4)
RMSE_heldout_BIC_PICARyrs<- rep(NA,4)
RMSE_heldout_CIyrs<- rep(NA,4)

for(i in 1:4){
  RMSE_heldout_AIC_PICARyrs[i]<- sqrt(mean((obsCVLinear[(cs.obs.test[i]+1):cs.obs.test[i+1]]-
                                           AIC_PICAR_allPreds_last4yrs[(cs.obs.test[i]+1):cs.obs.test[i+1]])^2))
  
  RMSE_heldout_BIC_PICARyrs[i]<- sqrt(mean((obsCVLinear[(cs.obs.test[i]+1):cs.obs.test[i+1]]-
                                           BIC_PICAR_allPreds_last4yrs[(cs.obs.test[i]+1):cs.obs.test[i+1]])^2))
  
  RMSE_heldout_CIyrs[i]<- sqrt(mean((obsCVLinear[(cs.obs.test[i]+1):cs.obs.test[i+1]]-
                                    CI_allPreds_last4yrs[(cs.obs.test[i]+1):cs.obs.test[i+1]])^2))
  
}


#RMSE_heldout_CVRMSE_PICAR
RMSE_heldout_AIC_PICARyrs
RMSE_heldout_BIC_PICARyrs
RMSE_heldout_CIyrs


#> RMSE_heldout_AIC_PICAR
#[1] 24.97568
#> RMSE_heldout_BIC_PICAR
#[1] 24.94334
#> RMSE_heldout_CI
#[1] 21.2021

#Out of all, CI model performed best on holdout data
#Out of PICAR models, BIC PICAR model performed best on holdout data in terms of CVRMSE

#look at RMSE performance on training data
first32yrsinds<- 1:nrow(XMat)

#RMSE_train_CVRMSE_PICAR<- sqrt(mean((obsModLinear-CVRMSE_PICAR_allPreds[first32yrsinds,1])^2))

RMSE_train_AIC_PICAR<- sqrt(mean((obsModLinear-AIC_PICAR_allPreds[first32yrsinds,1])^2))

RMSE_train_BIC_PICAR<- sqrt(mean((obsModLinear-BIC_PICAR_allPreds[first32yrsinds,1])^2))

RMSE_train_CI<- sqrt(mean((obsModLinear-CI_allPreds[first32yrsinds,1])^2))

#RMSE_train_CVRMSE_PICAR
RMSE_train_AIC_PICAR
RMSE_train_BIC_PICAR
RMSE_train_CI


#> RMSE_train_AIC_PICAR
#[1] 22.03737
#> RMSE_train_BIC_PICAR
#[1] 22.16331
#> RMSE_train_CI
#[1] 17.56461

#CI best, BIC best out of PICAR, then CVRMSE, then AIC


#PE_heldout_CVRMSE_PICAR<- (obsCVLinear-CVRMSE_PICAR_allPreds[last4yrsinds,1])/obsCVLinear

PE_heldout_AIC_PICAR<- (obsCVLinear-AIC_PICAR_allPreds[last4yrsinds,1])/obsCVLinear

PE_heldout_BIC_PICAR<- (obsCVLinear-BIC_PICAR_allPreds[last4yrsinds,1])/obsCVLinear

PE_heldout_CI<- (obsCVLinear-CI_allPreds[last4yrsinds,1])/obsCVLinear

#summary(PE_heldout_CVRMSE_PICAR)
summary(PE_heldout_AIC_PICAR)
summary(PE_heldout_BIC_PICAR)
summary(PE_heldout_CI)

#look at % error performance on training data
first32yrsinds<- 1:nrow(XMat)

#PE_train_CVRMSE_PICAR<- (obsModLinear-CVRMSE_PICAR_allPreds[first32yrsinds,1])/obsModLinear

PE_train_AIC_PICAR<- (obsModLinear-AIC_PICAR_allPreds[first32yrsinds,1])/obsModLinear

PE_train_BIC_PICAR<- (obsModLinear-BIC_PICAR_allPreds[first32yrsinds,1])/obsModLinear

PE_train_CI<- (obsModLinear-CI_allPreds[first32yrsinds,1])/obsModLinear

#summary(PE_train_CVRMSE_PICAR)
summary(PE_train_AIC_PICAR)
summary(PE_train_BIC_PICAR)
summary(PE_train_CI)

#> summary(PE_train_CVRMSE_PICAR)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.349822 -0.025807  0.006049 -0.003275  0.031978  0.216944 
#> summary(PE_train_AIC_PICAR)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.356972 -0.026053  0.006016 -0.003277  0.032116  0.220064 
#> summary(PE_train_BIC_PICAR)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.367989 -0.026100  0.006031 -0.003299  0.032356  0.217589 
#> summary(PE_train_CI)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.223142 -0.019746  0.004841 -0.002225  0.024802  0.241585 


#look at % error performance on held out data
last4yrsinds<- (length(obsFullLinear)-length(obsCVLinear)+1):length(obsFullLinear)

#APE_heldout_CVRMSE_PICAR<- abs(obsCVLinear-CVRMSE_PICAR_allPreds[last4yrsinds,1])/obsCVLinear

APE_heldout_AIC_PICAR<- abs(obsCVLinear-AIC_PICAR_allPreds[last4yrsinds,1])/obsCVLinear

APE_heldout_BIC_PICAR<- abs(obsCVLinear-BIC_PICAR_allPreds[last4yrsinds,1])/obsCVLinear

APE_heldout_CI<- abs(obsCVLinear-CI_allPreds[last4yrsinds,1])/obsCVLinear

#summary(APE_heldout_CVRMSE_PICAR)
summary(APE_heldout_AIC_PICAR) 
summary(APE_heldout_BIC_PICAR) 
summary(APE_heldout_CI) 

#> summary(APE_heldout_CVRMSE_PICAR)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000043 0.0092637 0.0216422 0.0333305 0.0438252 0.4355732 
#> summary(APE_heldout_AIC_PICAR) 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000052 0.0094603 0.0219729 0.0332035 0.0439010 0.4406440 
#> summary(APE_heldout_BIC_PICAR) 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000032 0.0095454 0.0218426 0.0330544 0.0435648 0.4131716 
#> summary(APE_heldout_CI) 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000041 0.0133281 0.0275761 0.0345234 0.0455484 0.3556498  

#best: BIC_PICAR

#look at % error performance on training data
first32yrsinds<- 1:nrow(XMat)

APE_train_CVRMSE_PICAR<- abs(obsModLinear-CVRMSE_PICAR_allPreds[first32yrsinds,1])/obsModLinear

APE_train_AIC_PICAR<- abs(obsModLinear-AIC_PICAR_allPreds[first32yrsinds,1])/obsModLinear

APE_train_BIC_PICAR<- abs(obsModLinear-BIC_PICAR_allPreds[first32yrsinds,1])/obsModLinear

APE_train_CI<- abs(obsModLinear-CI_allPreds[first32yrsinds,1])/obsModLinear

summary(APE_train_CVRMSE_PICAR) 
summary(APE_train_AIC_PICAR) 
summary(APE_train_BIC_PICAR) 
summary(APE_train_CI)

#> summary(APE_train_CVRMSE_PICAR) 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000001 0.0140505 0.0295880 0.0408191 0.0529683 1.3498220 
#> summary(APE_train_AIC_PICAR) 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000019 0.0140512 0.0297136 0.0409140 0.0529632 1.3569716 
#> summary(APE_train_BIC_PICAR) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.01415 0.02992 0.04113 0.05323 1.36799 
#> summary(APE_train_CI)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000009 0.0104899 0.0227788 0.0324939 0.0416431 1.2231415


#look at % error performance on held out data
last4yrsinds<- (length(obsFullLinear)-length(obsCVLinear)+1):length(obsFullLinear)

#APE_heldout_CVRMSE_PICAR<- abs(obsCVLinear-CVRMSE_PICAR_allPreds[last4yrsinds,1])/obsCVLinear

AE_heldout_AIC_PICAR<- abs(obsCVLinear-AIC_PICAR_allPreds[last4yrsinds,1])

AE_heldout_BIC_PICAR<- abs(obsCVLinear-BIC_PICAR_allPreds[last4yrsinds,1])

AE_heldout_CI<- abs(obsCVLinear-CI_allPreds[last4yrsinds,1])

#summary(APE_heldout_CVRMSE_PICAR)
summary(AE_heldout_AIC_PICAR) 
summary(AE_heldout_BIC_PICAR) 
summary(AE_heldout_CI) 

library(dplyr)
#judge performance considering uncertainty
btwnbnds_CVRMSE_PICAR<- rep(NA, length(obsFullLinear))
btwnbnds_AIC_PICAR<- rep(NA, length(obsFullLinear))
btwnbnds_BIC_PICAR<- rep(NA, length(obsFullLinear))
btwnbnds_CI<- rep(NA, length(obsFullLinear))


for(i in 1:length(obsFullLinear)){
  btwnbnds_CVRMSE_PICAR[i]<- between(obsFullLinear[i],CVRMSE_PICAR_allPreds[i,2],CVRMSE_PICAR_allPreds[i,3])
  btwnbnds_AIC_PICAR[i]<- between(obsFullLinear[i],AIC_PICAR_allPreds[i,2],AIC_PICAR_allPreds[i,3])
  btwnbnds_BIC_PICAR[i]<- between(obsFullLinear[i],BIC_PICAR_allPreds[i,2],BIC_PICAR_allPreds[i,3])
  btwnbnds_CI[i]<- between(obsFullLinear[i],CI_allPreds[i,2],CI_allPreds[i,3])
}

sum(btwnbnds_CVRMSE_PICAR)/length(btwnbnds_CVRMSE_PICAR)
sum(btwnbnds_AIC_PICAR)/length(btwnbnds_AIC_PICAR)
sum(btwnbnds_BIC_PICAR)/length(btwnbnds_BIC_PICAR)
sum(btwnbnds_CI)/length(btwnbnds_CI)


#judge performance considering uncertainty
btwnbnds_heldout_CVRMSE_PICAR<- btwnbnds_CVRMSE_PICAR[last4yrsinds]
btwnbnds_heldout_AIC_PICAR<-  btwnbnds_AIC_PICAR[last4yrsinds]
btwnbnds_heldout_BIC_PICAR<-  btwnbnds_BIC_PICAR[last4yrsinds]
btwnbnds_heldout_CI<-  btwnbnds_CI[last4yrsinds]

sum(btwnbnds_heldout_CVRMSE_PICAR)/length(btwnbnds_heldout_CVRMSE_PICAR)
sum(btwnbnds_heldout_AIC_PICAR)/length(btwnbnds_heldout_AIC_PICAR)
sum(btwnbnds_heldout_BIC_PICAR)/length(btwnbnds_heldout_BIC_PICAR)
sum(btwnbnds_heldout_CI)/length(btwnbnds_heldout_CI)

btwnbnds_train_CVRMSE_PICAR<- btwnbnds_CVRMSE_PICAR[first32yrsinds]
btwnbnds_train_AIC_PICAR<- btwnbnds_AIC_PICAR[first32yrsinds]
btwnbnds_train_BIC_PICAR<- btwnbnds_BIC_PICAR[first32yrsinds]
btwnbnds_train_CI<- btwnbnds_CI[first32yrsinds]

sum(btwnbnds_train_CVRMSE_PICAR)/length(btwnbnds_train_CVRMSE_PICAR)
sum(btwnbnds_train_AIC_PICAR)/length(btwnbnds_train_AIC_PICAR)
sum(btwnbnds_train_BIC_PICAR)/length(btwnbnds_train_BIC_PICAR)
sum(btwnbnds_train_CI)/length(btwnbnds_train_CI)

#best on training and heldout data: CI

