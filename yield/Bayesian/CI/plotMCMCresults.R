#look at results from SRMeanModel.R

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/Bayesian/CI/plots")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/Bayesian/CI/plots")}

rm(list=ls())
setwd("/storage/work/svr5482")
load("Climate_CornYield-me/Bayesian/CI/output/samples_cfmpw_fast.RData")
load("Climate_CornYield-me/Bayesian/CI/output/time_cfmpw_fast.RData")


#plot residuals in space for each year

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

#residuals when subtracting average prediction from yield


load("Climate_CornYield-me/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

# Remove unecessary objects
rm(distMatCVList, distMatModList)
#rm(distMatHaversin) ; rm(distMatHaversin2)
rm(test_data,train_data)
rm(ifyouaintfirst) ; rm(yourelast)
rm(comboLocation) ; rm(dat)


res_beta<- res[,1:ncol(XMat)]
res_s2<- res[,ncol(res)]

#cores=detectCores()
#cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
#registerDoParallel(cl)

#foreach(i = 1:nyrs)%dopar%{
for(i in 1:nyrs){
  if(i<=ntrainyrs){
    pt<-proc.time() # Start Time
    predmat_train<- XMat[(cs.obs.train[i]+1):cs.obs.train[i+1],]%*%t(res_beta)
    meanpred_train<- rowMeans(predmat_train)
    resid<- obsModLinear[(cs.obs.train[i]+1):cs.obs.train[i+1]]-meanpred_train
    
    resid.df<- cbind(timeloc.ord[(cs.obs.train[i]+1):cs.obs.train[i+1],],resid) 
    resid.df$fips<- as.numeric(resid.df$fips)
    
    filename<-paste("/storage/work/svr5482/Climate_CornYield-me/Bayesian/CI/plots/resid_train_",1980+i,".jpeg",sep="")
    jpeg(file = filename,width = 800,height=800)
    #res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
    a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                              "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                              "LA","AR","IA"), data=resid.df, values="resid") +
      labs(title = paste(1980+i," Yield Resid (Train)- County Intercepts Model",sep=""))+
      scale_fill_gradient2(low = "blue",mid="white", high ="red", limits=c(min(resid.df$resid),max(resid.df$resid)), name="Residual")+
      theme(plot.title = element_text(size=14))
    plot(a)
    dev.off()
    ptFinal<-proc.time()-pt ; ptFinal<-ptFinal[3] 
  }
  if(i>ntrainyrs){
    predmat_test<- XMatCV[(cs.obs.test[i-ntrainyrs]+1):cs.obs.test[i-ntrainyrs+1],]%*%t(res_beta)
    meanpred_test<- rowMeans(predmat_test)
    resid<- obsCVLinear[(cs.obs.test[i-ntrainyrs]+1):cs.obs.test[i-ntrainyrs+1]]-meanpred_test
    
    resid.df<- cbind(timeloc.ord[which(timeloc.ord$year==(1980+i)),],resid) 
    resid.df$fips<- as.numeric(resid.df$fips)
    
    filename<-paste("/storage/work/svr5482/Climate_CornYield-me/Bayesian/CI/plots/resid_test_",1980+i,".jpeg",sep="")
    jpeg(file = filename,width = 800,height=800)
    #res<- as.data.frame(slr_res.df[which(slr_res.df$year==(1980+i)),])
    a=plot_usmap(regions="counties",include=c("PA","NY","NJ","MD","DE","DC","NC","VA","SC","WV","OH",
                                              "MI","GA","KY","IN","IL","AL","TN","WI","MS","MN","MO",
                                              "LA","AR","IA"), data=resid.df, values="resid") +
      labs(title = paste(1980+i," Yield Resid (Test)- County Intercepts",sep=""))+
      scale_fill_gradient2(low = "blue",mid="white", high ="red", limits=c(min(resid.df$resid),max(resid.df$resid)), name="Residual")+
      theme(plot.title = element_text(size=14))
    plot(a)
    dev.off()
    
  }
  
  
}


#stopCluster(cl)
