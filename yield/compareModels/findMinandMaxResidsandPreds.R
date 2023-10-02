#find max and min residuals across models

rm(list=ls())

setwd("/storage/work/svr5482")

#residuals when subtracting average prediction from yield

load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.1/rank190/allPreds.RData")

allPredsMod<- allPreds[1:length(obsModLinear),]
allPredsCV<- allPreds[(length(obsModLinear)+1):nrow(allPreds),]

pred_max_AIC<- max(allPreds[,1]); pred_min_AIC<- min(allPreds[,1])

ufips<- unique(timeloc.ord$fips)

#look at % error performance on held out data
last4yrsinds<- (length(obsFullLinear)-length(obsCVLinear)+1):length(obsFullLinear)

E_test<- (obsCVLinear-allPreds[last4yrsinds,1])

#look at % error performance on held out data
first34yrsinds<- 1:length(obsModLinear)

E_train<- (obsModLinear-allPreds[first34yrsinds,1])

E_max_AIC<- max(c(E_test,E_train)); E_min_AIC<- min(c(E_test,E_train))

################################################################################

#plot predictions from BIC_PICAR_model.R

#residuals when subtracting average prediction from yield

load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/allPreds.RData")

allPredsMod<- allPreds[1:length(obsModLinear),]
allPredsCV<- allPreds[(length(obsModLinear)+1):nrow(allPreds),]

pred_max_BIC<- max(allPreds[,1]); pred_min_BIC<- min(allPreds[,1])

ufips<- unique(timeloc.ord$fips)

#look at % error performance on held out data
last4yrsinds<- (length(obsFullLinear)-length(obsCVLinear)+1):length(obsFullLinear)

E_test<- (obsCVLinear-allPreds[last4yrsinds,1])

#look at % error performance on held out data
first34yrsinds<- 1:length(obsModLinear)

E_train<- (obsModLinear-allPreds[first34yrsinds,1])

E_max_BIC<- max(c(E_test,E_train)); E_min_BIC<- min(c(E_test,E_train))

#plot predictions from BIC_PICAR_model.R

################################################################################
#residuals when subtracting average prediction from yield

load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
load("Climate_CornYield-me/yield/Bayesian/CI/output/allPreds.RData")

allPredsMod<- allPreds[1:length(obsModLinear),]
allPredsCV<- allPreds[(length(obsModLinear)+1):nrow(allPreds),]

pred_max_CI<- max(allPreds[,1]); pred_min_CI<- min(allPreds[,1])

ufips<- unique(timeloc.ord$fips)

#look at % error performance on held out data
last4yrsinds<- (length(obsFullLinear)-length(obsCVLinear)+1):length(obsFullLinear)

E_test<- (obsCVLinear-allPreds[last4yrsinds,1])

#look at % error performance on held out data
first34yrsinds<- 1:length(obsModLinear)

E_train<- (obsModLinear-allPreds[first34yrsinds,1])

E_max_CI<- max(c(E_test,E_train)); E_min_CI<- min(c(E_test,E_train))


min_resid<- min(E_min_AIC,E_min_BIC,E_min_CI)
max_resid<- max(E_max_AIC,E_max_BIC,E_max_CI)

min_pred<- min(pred_min_AIC,pred_min_BIC,pred_min_CI)
max_pred<- max(pred_max_AIC,pred_max_BIC,pred_max_CI)

save(min_resid,max_resid,min_pred,max_pred,
     file="Climate_CornYield-me/yield/compareModels/minandmaxresidsandpreds.RData")
################################################################################
load("Climate_CornYield-me/yield/compareModels/minandmaxresidsandpreds.RData")
load("Climate_CornYield-me/yield/Bayesian/CI/output/allPreds_TWX.RData")

min_pred<- min(min_pred, min(allPreds[,1]))
max_pred<- max(max_pred, max(allPreds[,1]))

load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwTWX.RData")

#look at RMSE performance on held out data
last2yrsinds<- (csobs[ntrainyrs]+1):csobs[nyrs]

resids_test_TWX<- obsCVLinear-allPreds[(csobs[ntrainyrs]+1):csobs[nyrs],1]
resids_train_TWX<- obsModLinear- allPreds[1:csobs[ntrainyrs],1]

min_resid<- min(min_resid,min(c(resids_test_TWX,resids_train_TWX)))
max_resid<- max(max_resid,max(c(resids_test_TWX,resids_train_TWX)))

save(min_resid,max_resid,min_pred,max_pred,
     file="Climate_CornYield-me/yield/compareModels/minandmaxresidsandpreds.RData")
