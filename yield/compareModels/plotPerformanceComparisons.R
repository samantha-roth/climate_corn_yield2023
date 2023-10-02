#plot measures of predictive performance
rm(list=ls())

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots")==F){ 
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots")}

library(ggplot2)

#load("Climate_CornYield-me/PICAR/timeConstant/noCIs/cutoff.1/rank400/allPreds.RData")
#CVRMSE_PICAR_allPreds<- allPreds

setwd("/storage/work/svr5482")

load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/allPreds.RData")
BIC_PICAR_allPreds<- allPreds

load("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.1/rank190/allPreds.RData")
AIC_PICAR_allPreds<- allPreds

load("Climate_CornYield-me/yield/Bayesian/CI/output/allPreds.RData")
CI_allPreds<- allPreds

rm(allPreds)

load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")

################################################################################
#raw errors

#look at % error performance on held out data
last4yrsinds<- (length(obsFullLinear)-length(obsCVLinear)+1):length(obsFullLinear)

#E_heldout_CVRMSE_PICAR<- (obsCVLinear-CVRMSE_PICAR_allPreds[last4yrsinds,1])

E_heldout_AIC_PICAR<- (obsCVLinear-AIC_PICAR_allPreds[last4yrsinds,1])

E_heldout_BIC_PICAR<- (obsCVLinear-BIC_PICAR_allPreds[last4yrsinds,1])

E_heldout_CI<- (obsCVLinear-CI_allPreds[last4yrsinds,1])

#summary(E_heldout_CVRMSE_PICAR)
summary(E_heldout_AIC_PICAR)
summary(E_heldout_BIC_PICAR)
summary(E_heldout_CI)

#plot PICAR (BIC) residuals VS CI residuals

E_test_all<- as.data.frame(cbind(E_heldout_CI,E_heldout_BIC_PICAR,E_heldout_AIC_PICAR))

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/CIvsPICARBIC_Residuals.jpeg",sep="")
jpeg(file = filename,width = 800,height=800)
ggplot(E_test_all, aes(x=E_heldout_CI, y=E_heldout_BIC_PICAR)) + 
  geom_point() + 
  xlim(c(min(c(E_heldout_CI, E_heldout_BIC_PICAR)),max(c(E_heldout_CI, E_heldout_BIC_PICAR)))) +
  ylim(c(min(c(E_heldout_CI, E_heldout_BIC_PICAR)),max(c(E_heldout_CI, E_heldout_BIC_PICAR)))) +
  xlab("CI") + ylab("PICAR (BIC)") + ggtitle("Observed - predicted yield")
dev.off()

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/CIvsPICARAIC_Residuals.jpeg",sep="")
jpeg(file = filename,width = 800,height=800)
ggplot(E_test_all, aes(x=E_heldout_CI, y=E_heldout_AIC_PICAR)) + 
  geom_point() + 
  xlim(c(min(c(E_heldout_CI, E_heldout_AIC_PICAR)),max(c(E_heldout_CI, E_heldout_AIC_PICAR)))) +
  ylim(c(min(c(E_heldout_CI, E_heldout_AIC_PICAR)),max(c(E_heldout_CI, E_heldout_AIC_PICAR)))) +
  xlab("CI") + ylab("PICAR (AIC)") + ggtitle("Observed - predicted yield")
dev.off()

#E.df<- data.frame("Error"= c(E_heldout_CI,E_heldout_BIC_PICAR,E_heldout_AIC_PICAR,E_heldout_CVRMSE_PICAR),
#                  "Model"= c(rep("County Intercepts",length(E_heldout_CI)),
#                             rep("PICAR (BIC)",length(E_heldout_BIC_PICAR)),
#                             rep("PICAR (AIC)",length(E_heldout_AIC_PICAR)),
#                             rep("PICAR (CVRMSE)",length(E_heldout_CVRMSE_PICAR))))

E.df<- data.frame("Error"= c(E_heldout_CI,E_heldout_BIC_PICAR,E_heldout_AIC_PICAR),
                  "Model"= c(rep("County Intercepts",length(E_heldout_CI)),
                             rep("PICAR (BIC)",length(E_heldout_BIC_PICAR)),
                             rep("PICAR (AIC)",length(E_heldout_AIC_PICAR))))

E.df$Model <- as.factor(E.df$Model)

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/ResidualBoxplots.jpeg",sep="")
jpeg(file = filename,width = 800,height=800)
ggplot(E.df, aes(x=Model, y=Error, color=Model)) + 
  geom_boxplot() + ggtitle("Yield (bushels/acre): observed - predicted") +
  xlab("Error (bushels/acre)")
dev.off()

NOAIC_E.df<- data.frame("Error"= c(E_heldout_CI,E_heldout_BIC_PICAR),
                  "Model"= c(rep("Cty Int",length(E_heldout_CI)),
                             rep("PICAR",length(E_heldout_BIC_PICAR))))

NOAIC_E.df$Model <- as.factor(E.df$Model)

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/ResidualBoxplots.jpeg",sep="")
jpeg(file = filename,width = 800,height=800)
ggplot(NOAIC_E.df, aes(x=Model, y=Error, color=Model)) + 
  geom_boxplot() + ggtitle("Yield: observed - predicted") +
  ylab("Error (bushels/acre)") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

################################################################################
#look at RMSE performance on held out data
#last4yrsinds<- (nrow(CVRMSE_PICAR_allPreds)-length(obsCVLinear)+1):nrow(CVRMSE_PICAR_allPreds)

#RMSE_heldout_CVRMSE_PICAR<- sqrt(mean((obsCVLinear-CVRMSE_PICAR_allPreds[last4yrsinds,1])^2))

RMSE_heldout_AIC_PICAR<- sqrt(mean((obsCVLinear-AIC_PICAR_allPreds[last4yrsinds,1])^2))

RMSE_heldout_BIC_PICAR<- sqrt(mean((obsCVLinear-BIC_PICAR_allPreds[last4yrsinds,1])^2))

RMSE_heldout_CI<- sqrt(mean((obsCVLinear-CI_allPreds[last4yrsinds,1])^2))

################################################################################
#percent errors

#look at % error performance on held out data
#last4yrsinds<- (nrow(CVRMSE_PICAR_allPreds)-length(obsCVLinear)+1):nrow(CVRMSE_PICAR_allPreds)

#PE_heldout_CVRMSE_PICAR<- (obsCVLinear-CVRMSE_PICAR_allPreds[last4yrsinds,1])/obsCVLinear

PE_heldout_AIC_PICAR<- (obsCVLinear-AIC_PICAR_allPreds[last4yrsinds,1])/obsCVLinear

PE_heldout_BIC_PICAR<- (obsCVLinear-BIC_PICAR_allPreds[last4yrsinds,1])/obsCVLinear

PE_heldout_CI<- (obsCVLinear-CI_allPreds[last4yrsinds,1])/obsCVLinear

#PE.df<- data.frame("PercentError"= c(PE_heldout_CI,
#                                      PE_heldout_BIC_PICAR,
#                                      PE_heldout_AIC_PICAR,
#                                      PE_heldout_CVRMSE_PICAR),
#                  "Model"= c(rep("County Intercepts",length(PE_heldout_CI)),
#                             rep("PICAR (BIC)",length(PE_heldout_BIC_PICAR)),
#                             rep("PICAR (AIC)",length(PE_heldout_AIC_PICAR)),
#                             rep("PICAR (CVRMSE)",length(PE_heldout_CVRMSE_PICAR))))

PE.df<- data.frame("PercentError"= c(PE_heldout_CI*100,
                                     PE_heldout_BIC_PICAR*100,
                                     PE_heldout_AIC_PICAR*100),
                   "Model"= c(rep("County Intercepts",length(PE_heldout_CI)),
                              rep("PICAR (BIC)",length(PE_heldout_BIC_PICAR)),
                              rep("PICAR (AIC)",length(PE_heldout_AIC_PICAR))))

PE.df$Model <- as.factor(PE.df$Model)

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/PEBoxplots.jpeg",sep="")
jpeg(file = filename,width = 800,height=800)
ggplot(PE.df, aes(x=Model, y=PercentError, color=Model)) + 
  geom_boxplot() + 
  ggtitle("Yield: (observed - predicted)/observed") +
  ylab("Percent Error")
dev.off()

NOAIC_PE.df<- data.frame("PercentError"= c(PE_heldout_CI*100,
                                     PE_heldout_BIC_PICAR*100),
                   "Model"= c(rep("Cty Int",length(PE_heldout_CI)),
                              rep("PICAR",length(PE_heldout_BIC_PICAR))))

NOAIC_PE.df$Model <- as.factor(NOAIC_PE.df$Model)

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/PEBoxplots.jpeg",sep="")
jpeg(file = filename,width = 800,height=800)
ggplot(NOAIC_PE.df, aes(x=Model, y=PercentError, color=Model)) + 
  geom_boxplot() + 
  ggtitle("Yield: 100*(observed - predicted)/observed") +
  ylab("Percent Error") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()


summary(PE_heldout_AIC_PICAR); summary(PE_heldout_BIC_PICAR); summary(PE_heldout_CI)

################################################################################

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/ResidsVSPreds.jpeg",sep="")
jpeg(file = filename,width = 1000,height=500)
par(mfrow=c(1,3))
plot(BIC_PICAR_allPreds[last4yrsinds,1],E_heldout_BIC_PICAR,
     main="PICAR (BIC) Model",xlab="Predicted yield",ylab="Observed-predicted")
plot(AIC_PICAR_allPreds[last4yrsinds,1],E_heldout_AIC_PICAR,
     main="PICAR (AIC) Model",xlab="Predicted yield",ylab="Observed-predicted")
plot(CI_allPreds[last4yrsinds,1],E_heldout_CI,
     main="County Intercepts Model",xlab="Predicted yield",ylab="Observed-predicted")
dev.off()
