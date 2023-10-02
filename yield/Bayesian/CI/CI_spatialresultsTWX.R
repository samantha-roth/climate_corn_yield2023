#plot predictions from BIC_PICAR_model.R

#rm(list=ls())

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/plots")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/plots")}

rm(list=ls())
setwd("/storage/work/svr5482")


library(sp)
#library(rgeos)
library(maps)
library(maptools)
library(ggplot2)
library(usmap)
library(housingData) #for returning county centroid
library(stringr)
#library(foreach)
#library(doParallel)

#residuals when subtracting average prediction from yield

load("Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwTWX.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
load("Climate_CornYield-me/yield/Bayesian/CI/output/allPreds_TWX.RData")

allPredsMod<- allPreds[1:length(obsModLinear),]
allPredsCV<- allPreds[(length(obsModLinear)+1):nrow(allPreds),]

pred_max<- max(allPreds[,1]); pred_min<- min(allPreds[,1])

ufips<- unique(timeloc.ord$fips)

#look at % error performance on held out data
last2yrsinds<- (csobs[ntrainyrs]+1):csobs[nyrs]

E_test<- (obsCVLinear-allPreds[last2yrsinds,1])

#look at % error performance on held out data
first34yrsinds<- 1:length(obsModLinear)

E_train<- (obsModLinear-allPreds[first34yrsinds,1])

load("Climate_CornYield-me/yield/compareModels/minandmaxresidsandpreds.RData")

#res_s2<- res[,ncol(res)]

#cores=detectCores()
#cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
#registerDoParallel(cl)


for(i in (ntrainyrs+1):nyrs){
  #foreach(i = 1:nyrs)%dopar%{
  if(i<=ntrainyrs){
    meanpred<- allPredsMod[(cs.obs.train[i]+1):cs.obs.train[i+1],1]
    
    meanpred.df<- cbind(timeloc.ord[(cs.obs.train[i]+1):cs.obs.train[i+1],],meanpred) 
    meanpred.df$fips<- as.numeric(meanpred.df$fips)
    
    filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/plots/meanpred_train_",1980+i,"_TWX.jpeg",sep="")
    jpeg(file = filename,width = 800,height=800)
    #res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
    a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                              "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                              "LA","AR","IA"), data=meanpred.df, values="meanpred") +
      labs(title = paste("County intercepts (TWX): ", 1980+i," yield",sep=""))+
      scale_fill_gradient2(low = "blue",mid="white", high ="red", 
                           limits=c(min_pred,max_pred), 
                           name="bushels/acre")+
      theme(plot.title = element_text(size=28), 
            legend.text= element_text(size=24),
            legend.title= element_text(size=24))
    plot(a)
    dev.off()
    
    resid<- obsModLinear[(cs.obs.train[i]+1):cs.obs.train[i+1]]-meanpred
    
    resid.df<- cbind(timeloc.ord[(cs.obs.train[i]+1):cs.obs.train[i+1],],resid) 
    resid.df$fips<- as.numeric(resid.df$fips)
    
    filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/plots/resid_train_",1980+i,"_TWX.jpeg",sep="")
    jpeg(file = filename,width = 800,height=800)
    #res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
    a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                              "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                              "LA","AR","IA"), data=resid.df, values="resid") +
      labs(title = paste("County intercepts (TWX): ", 1980+i," yield residual",sep=""))+
      scale_fill_gradient2(low = "blue",mid="white", high ="red", 
                           limits=c(min_resid,max_resid), name="bushels/acre")+
      theme(plot.title = element_text(size=28), 
            legend.text= element_text(size=24),
            legend.title= element_text(size=24))
    plot(a)
    dev.off()
    
  }
  if(i>ntrainyrs){
    
    meanpred<- allPredsCV[(cs.obs.test[i-ntrainyrs]+1):cs.obs.test[i-ntrainyrs+1],1]
    
    meanpred.df<- cbind(timeloc.ord[which(timeloc.ord$year==(1980+i)),],meanpred) 
    meanpred.df$fips<- as.numeric(meanpred.df$fips)
    
    filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/plots/meanpred_test_",1980+i,"_TWX.jpeg",sep="")
    jpeg(file = filename,width = 800,height=800)
    #res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
    a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                              "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                              "LA","AR","IA"), data=meanpred.df, values="meanpred") +
      labs(title = paste("TopoWX: ", 1980+i," yield",sep=""))+
      scale_fill_gradient2(low = "blue",mid="white", high ="red", 
                           limits=c(min_pred,max_pred), 
                           name="bushels/acre")+
      theme(plot.title = element_text(size=28), 
            legend.text= element_text(size=24),
            legend.title= element_text(size=24))
    plot(a)
    dev.off()
    
    resid<- obsCVLinear[(cs.obs.test[i-ntrainyrs]+1):cs.obs.test[i-ntrainyrs+1]]-meanpred
    
    resid.df<- cbind(timeloc.ord[which(timeloc.ord$year==(1980+i)),],resid) 
    resid.df$fips<- as.numeric(resid.df$fips)
    
    filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/plots/resid_test_",1980+i,"_TWX.jpeg",sep="")
    jpeg(file = filename,width = 800,height=800)
    #res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
    a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                              "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                              "LA","AR","IA"), data=resid.df, values="resid") +
      labs(title = paste("TopoWX: ", 1980+i," yield residual",sep=""))+
      scale_fill_gradient2(low = "blue",mid="white", high ="red", 
                           limits=c(min_resid,max_resid), name="bushels/acre")+
      theme(plot.title = element_text(size=28), 
            legend.text= element_text(size=24),
            legend.title= element_text(size=24))
    plot(a)
    dev.off()
    
  }
  
}


#stopCluster(cl)
