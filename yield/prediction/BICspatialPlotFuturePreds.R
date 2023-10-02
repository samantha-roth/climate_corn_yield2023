#plot 1 year from each decade in space and the mean prediction for the whole time period

rm(list=ls())
library(sp)
#library(rgeos)
library(maps)
library(maptools)
library(ggplot2)
library(usmap)
library(housingData) #for returning county centroid
library(stringr)

library(foreach)
library(doParallel)

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots")
}

#load data
load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/obsNoTimeBIC.RData")

nCounties<- 1821
nfutureyears<- 30
zero.csobs<- c(0,csobs)

ufips<- unique(timeloc.ord$fips)

pastCountyMeans<- rep(NA,nCounties)
for(i in 1:nCounties){
  fipsinds<- which(timeloc.ord$fips==ufips[i])
  pastCountyMeans[i]<- mean(obsNoTimeBIC[fipsinds])
}

#load min and max temperatures
#load("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/minandmaxpreds.RData")
#load("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/allyr_minandmaxpreds.RData")
load("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/output/FutureMeansandDiffs_MinsandMaxes.RData")


modelnames<-c("MIROC5","MRI-CGCM3","IPSL-CM5B-LR","IPSL-CM5A-LR", 
              "HadGEM2-ES365","GFDL-ESM2M","GFDL-ESM2G","CSIRO-Mk3-6-0","bcc-csm1-1",
              "MIROC-ESM", "IPSL-CM5A-MR", "CNRM-CM5","BNU-ESM",
              "MIROC-ESM-CHEM", "inmcm4", "HadGEM2-CC365", "CanESM2", "bcc-csm1-1-m")

futurefips<- rep(unique(timeloc.ord$fips),nfutureyears)
ufips<- unique(timeloc.ord$fips)

sumAllPreds<- matrix(0,nrow= nCounties*nfutureyears, ncol=5)

for(k in 1:length(modelnames)){
  print(modelnames[k])
      yr<-"2020_2049"
      
      load(paste0("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/",modelnames[k],"_",yr,"_predMat.RData"))
      
      sumAllPreds<- sumAllPreds + allPreds

}

meanAllPredMeans<- sumAllPreds[,1]/length(modelnames)

meanAllPredMeans.df<- data.frame("fips"=rep(ufips,nfutureyears),
                                 "year"=rep(2020:2049,each=nCounties),
                                 "meanPreds"=meanAllPredMeans)


#plotyrs<- c(2020,2030,2040,2049)
#for(t in 1:length(plotyrs)){
#  oneyear.df<- meanAllPredMeans.df[which(meanAllPredMeans.df$year==plotyrs[t]),]
#  filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots/BICspatialMeanPredAllModels",plotyrs[t],".jpeg",sep="")
#  jpeg(file = filename,width = 800,height=800)
#  #res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
#  a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
#                                            "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
#                                            "LA","AR","IA"), data=oneyear.df, values="meanPreds") +
#    labs(title = paste("PICAR (BIC)- ",plotyrs[t]," mean yield projection",sep=""))+
#    scale_fill_gradient2(low = "blue",mid="white", high ="red", 
#                         limits=c(nearmin,nearmax), 
#                         name="Yield")+
#    theme(plot.title = element_text(size=24), 
#          legend.text= element_text(size=24),
#          legend.title= element_text(size=24))
#  plot(a)
#  dev.off()
#}
  
  
meanAllPredMeansAllYrs<- rep(NA,nCounties)
for(s in 1:nCounties){
  meanAllPredMeansAllYrs[s]<- mean(meanAllPredMeans[which(futurefips==ufips[s])])
}

meanPredAllYrsAllModels.df<- data.frame("fips"=ufips, "meanPreds"= meanAllPredMeansAllYrs)

meanPredAllYrsAllModels.df$fips<- as.character(meanPredAllYrsAllModels.df$fips)

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots/BICspatialMeanPredAllModelsAllYrs_2020_2049.jpeg",sep="")
jpeg(file = filename,width = 800,height=800)
#res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                          "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                          "LA","AR","IA"), data=meanPredAllYrsAllModels.df, values="meanPreds") +
  labs(title = paste("PICAR: 2020-2049 projected mean yield anomaly",sep=""))+
  scale_fill_gradient2(low = "blue",mid="white", high ="red", 
                       limits=c(minMeanNear,maxMeanNear), 
                       name="bushels/acre")+
  theme(plot.title = element_text(size=28), 
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
plot(a)
dev.off()

#plot difference between past and near future

diffAllPredMeansAllYrs<- meanAllPredMeansAllYrs-pastCountyMeans
diffPredAllYrsAllModels.df<- data.frame("fips"=ufips, "diffPreds"= diffAllPredMeansAllYrs)

length(which(diffAllPredMeansAllYrs<0))/length(diffAllPredMeansAllYrs)

diffAllPredMeansAllYrs2020<- diffAllPredMeansAllYrs

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots/BICspatialDiffPredAllModelsAllYrs_2020_2049.jpeg",sep="")
jpeg(file = filename,width = 800,height=800)
a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                          "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                          "LA","AR","IA"), data=diffPredAllYrsAllModels.df, values="diffPreds") +
  labs(title = paste("PICAR: 2020-2049 projected mean change",sep=""))+
  scale_fill_gradient2(low = "blue",mid="white", high ="red", 
                       limits=c(minDiffNear,maxDiffNear), 
                       name="bushels/acre")+
  theme(plot.title = element_text(size=28), 
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
plot(a)
dev.off()
################################################################################
#further future
sumAllPreds<- matrix(0,nrow= nCounties*nfutureyears, ncol=5)

for(k in 1:length(modelnames)){
  print(modelnames[k])
  yr<-"2070_2099"
  
  load(paste("Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/output/",modelnames[k],"_",yr,"_predMat.RData",sep=""))
  
  sumAllPreds<- sumAllPreds + allPreds
  
  meanAllTimeEachCounty<- rep(NA,nCounties)
  
  
}

meanAllPredMeans<- sumAllPreds[,1]/length(modelnames)

meanAllPredMeans.df<- data.frame("fips"=rep(ufips,nfutureyears),
                                 "year"=rep(2070:2099,each=nCounties),
                                 "meanPreds"=meanAllPredMeans)

#plotyrs<- c(2070,2080,2090,2099)
#for(t in 1:length(plotyrs)){
#  oneyear.df<- meanAllPredMeans.df[which(meanAllPredMeans.df$year==plotyrs[t]),]
#  filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots/BICspatialMeanPredAllModels",plotyrs[t],".jpeg",sep="")
#  jpeg(file = filename,width = 800,height=800)
#  a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
#                                            "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
#                                            "LA","AR","IA"), data=oneyear.df, values="meanPreds") +
#    labs(title = paste("PICAR (BIC)- ",plotyrs[t]," mean yield projection",sep=""))+
#    scale_fill_gradient2(low = "blue",mid="white", high ="red", 
#                         limits=c(farmin,farmax), 
#                         name="Yield")+
#    theme(plot.title = element_text(size=24), 
#          legend.text= element_text(size=24),
#          legend.title= element_text(size=24))
#  plot(a)
#  dev.off()
#}


meanAllPredMeansAllYrs<- rep(NA,nCounties)
for(s in 1:nCounties){
  meanAllPredMeansAllYrs[s]<- mean(meanAllPredMeans[which(futurefips==ufips[s])])
}

meanPredAllYrsAllModels.df<- data.frame("fips"=ufips, "meanPreds"= meanAllPredMeansAllYrs)

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots/BICspatialMeanPredAllModelsAllYrs_2070_2099.jpeg",sep="")
jpeg(file = filename,width = 800,height=800)
#res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                          "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                          "LA","AR","IA"), data=meanPredAllYrsAllModels.df, values="meanPreds") +
  labs(title = paste("PICAR: 2070-2099 projected mean yield anomaly",sep=""))+
  scale_fill_gradient2(low = "blue",mid="white", high ="red", 
                       limits=c(minMeanFar,maxMeanFar), 
                       name="bushels/acre")+
  theme(plot.title = element_text(size=28), 
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
plot(a)
dev.off()


diffAllPredMeansAllYrs<- meanAllPredMeansAllYrs-pastCountyMeans
diffPredAllYrsAllModels.df<- data.frame("fips"=ufips, "diffPreds"= diffAllPredMeansAllYrs)

length(which(diffAllPredMeansAllYrs<0))/length(diffAllPredMeansAllYrs)


diffAllPredMeansAllYrs2070<- diffAllPredMeansAllYrs

length(which(diffAllPredMeansAllYrs2070< diffAllPredMeansAllYrs2020))/ length(diffAllPredMeansAllYrs2070)

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/prediction/plots/BICspatialDiffPredAllModelsAllYrs_2070_2099.jpeg",sep="")
jpeg(file = filename,width = 800,height=800)
a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                          "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                          "LA","AR","IA"), data=diffPredAllYrsAllModels.df, values="diffPreds") +
  labs(title = paste("PICAR: 2070-2099 projected mean change",sep=""))+
  scale_fill_gradient2(low = "blue",mid="white", high ="red", 
                       limits=c(minDiffFar,maxDiffFar), 
                       name="bushels/acre")+
  theme(plot.title = element_text(size=28), 
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
plot(a)
dev.off()
