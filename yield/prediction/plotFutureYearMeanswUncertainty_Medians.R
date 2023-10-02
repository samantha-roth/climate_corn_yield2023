rm(list=ls())

setwd("/storage/work/svr5482")

nfutureyears=30

modelnames<-c("MIROC5","MRI-CGCM3","IPSL-CM5B-LR","IPSL-CM5A-LR", 
              "HadGEM2-ES365","GFDL-ESM2M","GFDL-ESM2G","CSIRO-Mk3-6-0","bcc-csm1-1",
              "MIROC-ESM", "IPSL-CM5A-MR", "CNRM-CM5","BNU-ESM",
              "MIROC-ESM-CHEM", "inmcm4", "HadGEM2-CC365", "CanESM2", "bcc-csm1-1-m")

################################################################################
#load observed data
load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/obsNoTimeBIC.RData",sep=""))
#load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.1/rank190/obsNoTimeAIC.RData",sep=""))
load(paste("Climate_CornYield-me/yield/Bayesian/CI/output/obsNoTimeCI.RData",sep=""))
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")


load("/storage/work/svr5482/Climate_CornYield-me/SourceData/cornData.RData")

#nyrs= 38
nCounties=1821
nfutureyears<- 30
zero.csobs<- c(0,csobs)

obsyearanomalyBIC<- rep(NA, nyrs)
#obsyearanomalyAIC<- rep(NA, nyrs)
obsyearanomalyCI<- rep(NA, nyrs)

#obsyearanomalyBIC_NW<- rep(NA, nyrs)
#obsyearanomalyAIC<- rep(NA, nyrs)
#obsyearanomalyCI_NW<- rep(NA, nyrs)

for(t in 1:nyrs){
  obsyearanomalyCI[t]<- weighted.mean(obsNoTimeCI[(zero.csobs[t]+1):zero.csobs[t+1]], df$area[(zero.csobs[t]+1):zero.csobs[t+1]])
  obsyearanomalyBIC[t]<- weighted.mean(obsNoTimeBIC[(zero.csobs[t]+1):zero.csobs[t+1]], df$area[(zero.csobs[t]+1):zero.csobs[t+1]])
  
  #obsyearanomalyCI_NW[t]<- mean(obsNoTimeCI[(zero.csobs[t]+1):zero.csobs[t+1]])
  #obsyearanomalyBIC_NW[t]<- mean(obsNoTimeBIC[(zero.csobs[t]+1):zero.csobs[t+1]])
}

#qs <- function(x) { as.numeric(quantile(x, probs = c(.05,.25,.5,.75,.95))) }

#obsquantiles<- matrix(NA,nrow=nyrs,ncol=5)
#for(t in 1:nyrs){
#  obsquantiles[t,]<- qs(obsFullLinear[(zero.csobs[t]+1):zero.csobs[t+1]])
#}

obsCountyMeans.df<- data.frame("year"=rep(1981:2018,2),
                               "source"=c(rep("Obs. CI",nyrs),
                                          rep("Obs. PICAR",nyrs)),
                               "anomaly"=c(obsyearanomalyCI,obsyearanomalyBIC))

obsallyrmin<- min(c(obsyearanomalyCI,obsyearanomalyBIC))
obsallyrmax<- max(c(obsyearanomalyCI,obsyearanomalyBIC))

################################################################################
#near future#
yr<-"2020_2049"

load(paste("Climate_CornYield-me/yield/Bayesian/CI/output/allyearmeanQs",yr,".RData",sep=""))

CI_allyearmeanQs<- allyearmeanQs
#CI_allyearmean.025bd<- allyearmean.025bd
#CI_allyearmean.975bd<- allyearmean.975bd

load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/allyearmeanQs",yr,".RData",sep=""))

BIC_allyearmeanQs<- allyearmeanQs
#BIC_allyearmean.025bd<- allyearmean.025bd
#BIC_allyearmean.975bd<- allyearmean.975bd

#load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.1/rank190/output/allyearmeanQs",yr,".RData",sep=""))

#AIC_allyearmeanQs<- allyearmeanQs
#AIC_allyearmean.025bd<- allyearmean.025bd
#AIC_allyearmean.975bd<- allyearmean.975bd

#column 2 is the median
f1CI_yearmeanmedian<- CI_allyearmeanQs[,2]
f1BIC_yearmeanmedian<- BIC_allyearmeanQs[,2]

#column 3 is the mean
f1CI_yearmeanmean<- CI_allyearmeanQs[,3]
f1BIC_yearmeanmean<- BIC_allyearmeanQs[,3]
#AIC_yearmeanmean<- AIC_allyearmeanQs[,3]

#column 1 is the .025 quantile of all yearly mean predictions from the thinned MCMC sample
f1CI_yearmean.025lb<- CI_allyearmeanQs[,1]
f1BIC_yearmean.025lb<- BIC_allyearmeanQs[,1]
#AIC_yearmean.025lb<- AIC_allyearmeanQs[,1]

#column 4 is the .975 quantile of all yearly mean predictions from the thinned MCMC sample
f1CI_yearmean.975lb<- CI_allyearmeanQs[,4]
f1BIC_yearmean.975lb<- BIC_allyearmeanQs[,4]
#AIC_yearmean.975lb<- AIC_allyearmeanQs[,4]

#get min and max over all
f1allyrmin<- min(c(f1CI_yearmeanmean,
                   f1BIC_yearmeanmean,
                   f1CI_yearmean.025lb,
                   f1BIC_yearmean.025lb,
                   f1CI_yearmean.975lb,
                   f1BIC_yearmean.975lb))

f1allyrmax<- max(c(f1CI_yearmeanmean,
                   f1BIC_yearmeanmean,
                   f1CI_yearmean.025lb,
                   f1BIC_yearmean.025lb,
                   f1CI_yearmean.975lb,
                   f1BIC_yearmean.975lb))


################################################################################
#far future#
yr<-"2070_2099"

load(paste("Climate_CornYield-me/yield/Bayesian/CI/output/allyearmeanQs",yr,".RData",sep=""))

CI_allyearmeanQs<- allyearmeanQs
#CI_allyearmean.025bd<- allyearmean.025bd
#CI_allyearmean.975bd<- allyearmean.975bd

load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/allyearmeanQs",yr,".RData",sep=""))

BIC_allyearmeanQs<- allyearmeanQs
#BIC_allyearmean.025bd<- allyearmean.025bd
#BIC_allyearmean.975bd<- allyearmean.975bd

#load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.1/rank190/output/allyearmeanQs",yr,".RData",sep=""))

#AIC_allyearmeanQs<- allyearmeanQs
#AIC_allyearmean.025bd<- allyearmean.025bd
#AIC_allyearmean.975bd<- allyearmean.975bd

#column 2 is the median
f2CI_yearmeanmedian<- CI_allyearmeanQs[,2]
f2BIC_yearmeanmedian<- BIC_allyearmeanQs[,2]
#AIC_yearmeanmean<- AIC_allyearmeanQs[,3]

#column 3 is the mean
f2CI_yearmeanmean<- CI_allyearmeanQs[,3]
f2BIC_yearmeanmean<- BIC_allyearmeanQs[,3]
#AIC_yearmeanmean<- AIC_allyearmeanQs[,3]

#column 1 is the .025 quantile of all yearly mean predictions from the thinned MCMC sample
f2CI_yearmean.025lb<- CI_allyearmeanQs[,1]
f2BIC_yearmean.025lb<- BIC_allyearmeanQs[,1]
#AIC_yearmean.025lb<- AIC_allyearmeanQs[,1]

#column 4 is the .975 quantile of all yearly mean predictions from the thinned MCMC sample
f2CI_yearmean.975lb<- CI_allyearmeanQs[,4]
f2BIC_yearmean.975lb<- BIC_allyearmeanQs[,4]
#AIC_yearmean.975lb<- AIC_allyearmeanQs[,4]

#get min and max over all
f2allyrmin<- min(c(f2CI_yearmeanmean,
                   f2BIC_yearmeanmean,
                   f2CI_yearmeanmedian,
                   f2BIC_yearmeanmedian,
                   f2CI_yearmean.025lb,
                   f2BIC_yearmean.025lb,
                   f2CI_yearmean.975lb,
                   f2BIC_yearmean.975lb))

f2allyrmax<- max(c(f2CI_yearmeanmean,
                   f2BIC_yearmeanmean,
                   f2CI_yearmeanmedian,
                   f2BIC_yearmeanmedian,
                   f2CI_yearmean.025lb,
                   f2BIC_yearmean.025lb,
                   f2CI_yearmean.975lb,
                   f2BIC_yearmean.975lb))


allyrmin<- min(obsallyrmin,f1allyrmin,f2allyrmin)
allyrmax<- min(obsallyrmax,f1allyrmax,f2allyrmax)

YearMean_MeanQs_StatModel.df<- data.frame("year"=c(rep(2020:2049,8),rep(2070:2099,8)),
                                          "source"=c(rep("Bds. CI",2*nfutureyears),rep("Mean CI",nfutureyears), rep("Median CI",nfutureyears),
                                                     rep("Bds. PICAR",2*nfutureyears),rep("Mean PICAR",nfutureyears), rep("Median PICAR",nfutureyears),
                                                     rep("Bds. CI",2*nfutureyears),rep("Mean CI",nfutureyears),rep("Median CI",nfutureyears),
                                                     rep("Bds. PICAR",2*nfutureyears),rep("Mean PICAR",nfutureyears), rep("Median PICAR",nfutureyears)),
                                          "anomaly"=c(f1CI_yearmean.025lb,f1CI_yearmean.975lb,f1CI_yearmeanmean,f1CI_yearmeanmedian,
                                                      f1BIC_yearmean.025lb,f1BIC_yearmean.975lb,f1BIC_yearmeanmean,f1BIC_yearmeanmedian,
                                                      f2CI_yearmean.025lb,f2CI_yearmean.975lb,f2CI_yearmeanmean,f2CI_yearmeanmedian,
                                                      f2BIC_yearmean.025lb,f2BIC_yearmean.975lb,f2BIC_yearmeanmean,f2BIC_yearmeanmedian))


YearMean_MeanQs_ObsStatModel.df<- rbind(obsCountyMeans.df,YearMean_MeanQs_StatModel.df)


filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots/YieldAnomaliesByYearByStatModel.jpeg",sep="")
jpeg(file = filename,width = 1000,height=600)
#Visualization for observations and predictions
#ggplot(YearMean_MeanQs_ObsStatModel.df, aes(x = year, y = anomaly)) + 
#geom_line(aes(color = source, linetype = source)) + 
ggplot(YearMean_MeanQs_ObsStatModel.df, aes(x = year, y = anomaly, group=source)) + 
  geom_point(aes(shape=source, color=source)) +
  scale_color_manual(values = c("red", "red",
                                "black", "black", 
                                "blue", "blue")) +
                                       #scale_linetype_manual(values=c("dotted", "dashed",
  #                               "dotted", "dashed",
  #                               "dotted", "dashed"))+
  scale_shape_manual(values=c(0, 1, 0, 1, 0, 1))+
  ggtitle("Projected mean detrended yield")+ 
  xlab("year") + ylab("mean detrended yield bushels/acre)") +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()


################################################################################

CIobsCountyMeans.df<- data.frame("year"=rep(1981:2018,2),
                                 "source"=c(rep("Observed",nyrs)),
                                 "anomaly"=c(obsyearanomalyCI))

#look at just CI model
CIYearMean_MeanQs_StatModel.df<- data.frame("year"=c(rep(2020:2049,4),rep(2070:2099,4)),
                                            "source"=c(rep("95% Cred Int",2*nfutureyears),rep("Mean",nfutureyears),rep("Median",nfutureyears),
                                                       rep("95% Cred Int",2*nfutureyears),rep("Mean",nfutureyears),rep("Median",nfutureyears)),
                                            "anomaly"=c(f1CI_yearmean.025lb,f1CI_yearmean.975lb,f1CI_yearmeanmean,f1CI_yearmeanmedian,
                                                        f2CI_yearmean.025lb,f2CI_yearmean.975lb,f2CI_yearmeanmean,f2CI_yearmeanmedian))


CIYearMean_MeanQs_ObsStatModel.df<- rbind(CIobsCountyMeans.df,CIYearMean_MeanQs_StatModel.df)

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots/CIYieldAnomaliesByYearByStatModel.jpeg",sep="")
jpeg(file = filename,width = 1000,height=500)
#Visualization for observations and predictions
#ggplot(YearMean_MeanQs_ObsStatModel.df, aes(x = year, y = anomaly)) + 
#geom_line(aes(color = source, linetype = source)) + 
ggplot(CIYearMean_MeanQs_ObsStatModel.df, aes(x = year, y = anomaly, group=source)) + 
  geom_point(aes(shape=source, color=source)) +
  scale_color_manual(values = c("blue","black","red","brown")) +
                                        #scale_linetype_manual(values=c("dotted", "dashed",
  #                               "dotted", "dashed",
  #                               "dotted", "dashed"))+
  scale_shape_manual(values=c(16,15,15,15))+
  geom_line(aes(group = year))+
  ggtitle("County intercepts: projected mean detrended yield")+ 
  xlab("year") + ylab("mean detrended yield (bushels/acre)") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

minmean<- min(CIYearMean_MeanQs_ObsStatModel.df$anomaly[which(CIYearMean_MeanQs_ObsStatModel.df$source=="Mean")])

CIYearMean_MeanQs_ObsStatModel.df$year[which(CIYearMean_MeanQs_ObsStatModel.df$anomaly==minmean)] #min year is 2097

mean(obsyearanomalyCI)
mean(f1CI_yearmean.025lb); mean(f1CI_yearmean.975lb); mean(f1CI_yearmeanmean)
mean(f2CI_yearmean.025lb); mean(f2CI_yearmean.975lb); mean(f2CI_yearmeanmean)

###############################################################################
#by what percent is detrended yield decreasing?
100*(mean(f1CI_yearmean.025lb)-mean(obsyearanomalyCI))/mean(obsyearanomalyCI)
100*(mean(f1CI_yearmean.975lb)-mean(obsyearanomalyCI))/mean(obsyearanomalyCI)
100*(mean(f1CI_yearmeanmean)-mean(obsyearanomalyCI))/mean(obsyearanomalyCI)


100*(mean(f2CI_yearmean.025lb)-mean(obsyearanomalyCI))/mean(obsyearanomalyCI)
100*(mean(f2CI_yearmean.975lb)-mean(obsyearanomalyCI))/mean(obsyearanomalyCI)
100*(mean(f2CI_yearmeanmean)-mean(obsyearanomalyCI))/mean(obsyearanomalyCI)


summary(100*(f1CI_yearmean.025lb-mean(obsyearanomalyCI))/mean(obsyearanomalyCI))
summary(100*(f1CI_yearmean.975lb-mean(obsyearanomalyCI))/mean(obsyearanomalyCI))

summary(100*(f2CI_yearmean.025lb-mean(obsyearanomalyCI))/mean(obsyearanomalyCI))
summary(100*(f2CI_yearmean.975lb-mean(obsyearanomalyCI))/mean(obsyearanomalyCI))
################################################################################
#look at just BIC model

BICobsCountyMeans.df<- data.frame("year"=rep(1981:2018,2),
                                 "source"=c(rep("Observed",nyrs)),
                                 "anomaly"=c(obsyearanomalyBIC))

#look at just BIC model
BICYearMean_MeanQs_StatModel.df<- data.frame("year"=c(rep(2020:2049,4),rep(2070:2099,4)),
                                            "source"=c(rep("95% Cred Int",2*nfutureyears),rep("Mean",nfutureyears),rep("Median",nfutureyears),
                                                       rep("95% Cred Int",2*nfutureyears),rep("Mean",nfutureyears),rep("Median",nfutureyears)),
                                            "anomaly"=c(f1BIC_yearmean.025lb,f1BIC_yearmean.975lb,f1BIC_yearmeanmean,f1BIC_yearmeanmedian,
                                                        f2BIC_yearmean.025lb,f2BIC_yearmean.975lb,f2BIC_yearmeanmean,f2BIC_yearmeanmedian))


BICYearMean_MeanQs_ObsStatModel.df<- rbind(BICobsCountyMeans.df,BICYearMean_MeanQs_StatModel.df)


filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots/BICYieldAnomaliesByYearByStatModel.jpeg",sep="")
jpeg(file = filename,width = 1000,height=500)
#Visualization for observations and predictions
#ggplot(YearMean_MeanQs_ObsStatModel.df, aes(x = year, y = anomaly)) + 
#geom_line(aes(color = source, linetype = source)) + 
ggplot(BICYearMean_MeanQs_ObsStatModel.df, aes(x = year, y = anomaly, group=source)) + 
  geom_point(aes(shape=source, color=source)) +
  scale_color_manual(values = c("blue","black","red","brown")) +
                                        #scale_linetype_manual(values=c("dotted", "dashed",
  #                               "dotted", "dashed",
  #                               "dotted", "dashed"))+
  scale_shape_manual(values=c(16,15,15,15))+
  geom_line(aes(group = year))+
  ggtitle("PICAR: projected mean detrended yield")+ 
  xlab("year") + ylab("mean detrended yield (bushels/acre)") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()


minmean<- min(BICYearMean_MeanQs_ObsStatModel.df$anomaly[which(BICYearMean_MeanQs_ObsStatModel.df$source=="Mean")])

BICYearMean_MeanQs_ObsStatModel.df$year[which(BICYearMean_MeanQs_ObsStatModel.df$anomaly==minmean)] #min year is 2097

###############################################################################
#by what percent is detrended yield decreasing?
100*(mean(f1BIC_yearmean.025lb)-mean(obsyearanomalyBIC))/mean(obsyearanomalyBIC)
100*(mean(f1BIC_yearmean.975lb)-mean(obsyearanomalyBIC))/mean(obsyearanomalyBIC)
100*(mean(f1BIC_yearmeanmean)-mean(obsyearanomalyBIC))/mean(obsyearanomalyBIC)


100*(mean(f2BIC_yearmean.025lb)-mean(obsyearanomalyBIC))/mean(obsyearanomalyBIC)
100*(mean(f2BIC_yearmean.975lb)-mean(obsyearanomalyBIC))/mean(obsyearanomalyBIC)
100*(mean(f2BIC_yearmeanmean)-mean(obsyearanomalyBIC))/mean(obsyearanomalyBIC)


summary(100*(f1BIC_yearmean.025lb-mean(obsyearanomalyBIC))/mean(obsyearanomalyBIC))
summary(100*(f1BIC_yearmean.975lb-mean(obsyearanomalyBIC))/mean(obsyearanomalyBIC))

summary(100*(f2BIC_yearmean.025lb-mean(obsyearanomalyBIC))/mean(obsyearanomalyBIC))
summary(100*(f2BIC_yearmean.975lb-mean(obsyearanomalyBIC))/mean(obsyearanomalyBIC))
################################################################################
