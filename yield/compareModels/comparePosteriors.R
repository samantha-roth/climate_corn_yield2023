rm(list=ls())

setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output",sep=""))
load("samples_cfmpw_fast.RData")

CI_res<- res

setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20",sep=""))
load("samples_cfmpw_allyrs.RData")

BIC_res<- res

setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.1/rank190",sep=""))
load("samples_cfmpw_allyrs.RData")

AIC_res<- res

rm(res)
################################################################################

Pr.df<- data.frame("value"= c(CI_res[,2],BIC_res[,2],AIC_res[,2]),
                    "source"= c(rep("CI",nrow(CI_res)),
                                rep("BIC",nrow(BIC_res)),
                                rep("AIC",nrow(AIC_res))))

Pr2.df<- data.frame("value"= c(CI_res[,3],BIC_res[,3],AIC_res[,3]),
                    "source"= c(rep("CI",nrow(CI_res)),
                                rep("BIC",nrow(BIC_res)),
                                rep("AIC",nrow(AIC_res))))

T1.df<- data.frame("value"= c(CI_res[,4],BIC_res[,4],AIC_res[,4]),
                           "source"= c(rep("CI",nrow(CI_res)),
                                       rep("BIC",nrow(BIC_res)),
                                       rep("AIC",nrow(AIC_res))))

T2.df<- data.frame("value"= c(CI_res[,5],BIC_res[,5],AIC_res[,5]),
                    "source"= c(rep("CI",nrow(CI_res)),
                                rep("BIC",nrow(BIC_res)),
                                rep("AIC",nrow(AIC_res))))


jpeg(filename=paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/Pr_densities.jpeg",sep=""),
     width = 800, height = 600)
ggplot(Pr.df, aes(x=value, fill=source)) +
  geom_density(alpha=.25)+
  ggtitle("Pr1")+
  #geom_text(x=.031, y=650, label=expression("n"["ch"]^"*1"*"=.0305"))+
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

jpeg(filename=paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/Pr2_densities.jpeg",sep=""),
     width = 800, height = 600)
ggplot(Pr2.df, aes(x=value, fill=source)) +
  geom_density(alpha=.25)+
  ggtitle("Pr2")+
  #geom_text(x=.031, y=650, label=expression("n"["ch"]^"*1"*"=.0305"))+
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

jpeg(filename=paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/T1_densities.jpeg",sep=""),
     width = 800, height = 600)
ggplot(T1.df, aes(x=value, fill=source)) +
  geom_density(alpha=.25)+
  ggtitle("T1")+
  #geom_text(x=.031, y=650, label=expression("n"["ch"]^"*1"*"=.0305"))+
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

jpeg(filename=paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/T2_densities.jpeg",sep=""),
     width = 800, height = 600)
ggplot(T2.df, aes(x=value, fill=source)) +
  geom_density(alpha=.25)+
  ggtitle("T2")+
  #geom_text(x=.031, y=650, label=expression("n"["ch"]^"*1"*"=.0305"))+
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()


################################################################################
#heat function fit by each method

heatfunc<- function(c1,c2,x){
  if(x<10) result<- 0
  if(x>=10){
    if(x<29) result<- c1*x
    if(x>=29) result<- c1*x + c2*(x-29)
    
  }
  return(result)
}

#.50

CIq.50heatfunc<- function(x) heatfunc(quantile(CI_res[,4],.50)/24000,mean(CI_res[,5])/24000,x)
BICq.50heatfunc<- function(x) heatfunc(quantile(BIC_res[,4],.50)/24000,mean(BIC_res[,5])/24000,x)
AICq.50heatfunc<- function(x) heatfunc(quantile(AIC_res[,4],.50)/24000,mean(AIC_res[,5])/24000,x)

#convert to impact of exposure for one hour to a temperature interval

CIq.50heatline<- sapply(10:48,CIq.50heatfunc)
BICq.50heatline<- sapply(10:48,BICq.50heatfunc)
AICq.50heatline<- sapply(10:48,AICq.50heatfunc)

#.05

CIq.05heatfunc<- function(x) heatfunc(quantile(CI_res[,4],.05)/24000,mean(CI_res[,5])/24000,x)
BICq.05heatfunc<- function(x) heatfunc(quantile(BIC_res[,4],.05)/24000,mean(BIC_res[,5])/24000,x)
AICq.05heatfunc<- function(x) heatfunc(quantile(AIC_res[,4],.05)/24000,mean(AIC_res[,5])/24000,x)

#convert to impact of exposure for one hour to a temperature interval

CIq.05heatline<- sapply(10:48,CIq.05heatfunc)
BICq.05heatline<- sapply(10:48,BICq.05heatfunc)
AICq.05heatline<- sapply(10:48,AICq.05heatfunc)

#.95

CIq.95heatfunc<- function(x) heatfunc(quantile(CI_res[,4],.95)/24000,mean(CI_res[,5])/24000,x)
BICq.95heatfunc<- function(x) heatfunc(quantile(BIC_res[,4],.95)/24000,mean(BIC_res[,5])/24000,x)
AICq.95heatfunc<- function(x) heatfunc(quantile(AIC_res[,4],.95)/24000,mean(AIC_res[,5])/24000,x)

#convert to impact of exposure for one hour to a temperature interval

CIq.95heatline<- sapply(10:48,CIq.95heatfunc)
BICq.95heatline<- sapply(10:48,BICq.95heatfunc)
AICq.95heatline<- sapply(10:48,AICq.95heatfunc)

heatlines.df<- data.frame(source=rep(c(rep("CI",length(10:48)),
                                   rep("PICAR (BIC)",length(10:48)),
                                   rep("PICAR (AIC)",length(10:48))),3),
                          temp=rep(10:48,9),
                          quantile=as.character(rep(c(.5,.05,.95),each=3*length(10:48))),
                          heatline=c(CIq.50heatline,BICq.50heatline,AICq.50heatline,
                                     CIq.05heatline,BICq.05heatline,AICq.05heatline,
                                     CIq.95heatline,BICq.95heatline,AICq.95heatline))


filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/fitheatfunction_qs.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
#Visualization for observations and predictions
ggplot(heatlines.df, aes(x = temp, y = heatline)) + 
  geom_line(aes(color = quantile, linetype = source)) + 
  scale_color_manual(values = c("blue","purple","red")) +
  scale_linetype_manual(values=c("dotted","dashed", "dotdash"))+
  ggtitle("Fit function of heat")+ 
  xlab("Degrees Celsius") + ylab("Impact on yield of one hour of exposure") +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()


heatlines.dfNOAIC<- data.frame(source=rep(c(rep("Cty Int",length(10:48)),
                                       rep("PICAR",length(10:48))),3),
                          temp=rep(10:48,6),
                          quantile=as.character(rep(c(.5,.05,.95),each=2*length(10:48))),
                          heatline=c(CIq.50heatline,BICq.50heatline,
                                     CIq.05heatline,BICq.05heatline,
                                     CIq.95heatline,BICq.95heatline))

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/NOAICfitheatfunction_qs.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
#Visualization for observations and predictions
ggplot(heatlines.dfNOAIC, aes(x = temp, y = heatline)) + 
  geom_line(aes(color = quantile, linetype = source)) + 
  scale_color_manual(values = c("blue","black","red")) +
  scale_linetype_manual(values=c("dotted","dashed"))+
  ggtitle("Fit function of heat")+ 
  xlab("Degrees Celsius") + ylab("Impact on yield (bushels/acre): 1 hr exposure") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

################################################################################
#pr function

load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
summary(XMat[,2]) #summarize precipitation
#pr function fit by each method

prfunc<- function(c1,c2,x){
  result<- c1*x + c2*x^2
  return(result)
}

#.50

CIq.50prfunc<- function(x) prfunc(quantile(CI_res[,2],.50),quantile(CI_res[,3],.50),x)
BICq.50prfunc<- function(x) prfunc(quantile(BIC_res[,2],.50),quantile(BIC_res[,3],.50),x)
AICq.50prfunc<- function(x) prfunc(quantile(AIC_res[,2],.50),quantile(AIC_res[,3],.50),x)

#convert to impact of exposure for one hour to a temperature interval

CIq.50prline<- sapply(10:48,CIq.50prfunc)
BICq.50prline<- sapply(10:48,BICq.50prfunc)
AICq.50prline<- sapply(10:48,AICq.50prfunc)

#.05

CIq.05prfunc<- function(x) prfunc(quantile(CI_res[,2],.05),mean(CI_res[,3]),x)
BICq.05prfunc<- function(x) prfunc(quantile(BIC_res[,2],.05),mean(BIC_res[,3]),x)
AICq.05prfunc<- function(x) prfunc(quantile(AIC_res[,2],.05),mean(AIC_res[,3]),x)

#convert to impact of exposure for one hour to a temperature interval

CIq.05prline<- sapply(10:48,CIq.05prfunc)
BICq.05prline<- sapply(10:48,BICq.05prfunc)
AICq.05prline<- sapply(10:48,AICq.05prfunc)

#.95

CIq.95prfunc<- function(x) prfunc(quantile(CI_res[,2],.95),mean(CI_res[,3]),x)
BICq.95prfunc<- function(x) prfunc(quantile(BIC_res[,2],.95),mean(BIC_res[,3]),x)
AICq.95prfunc<- function(x) prfunc(quantile(AIC_res[,2],.95),mean(AIC_res[,3]),x)


base <-ggplot() + xlim(min(XMat[,2]), max(XMat[,2]))

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/fitprfunction_qs.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
base +
  geom_function(aes(linetype = "CI", colour = ".50"), fun = CIq.50prfunc) +
  geom_function(aes(linetype = "PICAR (BIC)", colour = ".50"), fun = BICq.50prfunc) +
  geom_function(aes(linetype = "PICAR (AIC)", colour = ".50"), fun = AICq.50prfunc) +
  geom_function(aes(linetype = "CI", colour = ".05"), fun = CIq.05prfunc) +
  geom_function(aes(linetype = "PICAR (BIC)", colour = ".05"), fun = BICq.05prfunc) +
  geom_function(aes(linetype = "PICAR (AIC)", colour =".05"), fun = AICq.05prfunc) +
  geom_function(aes(linetype = "CI", colour = ".95"), fun = CIq.95prfunc) +
  geom_function(aes(linetype = "PICAR (BIC)", colour = ".95"), fun = BICq.95prfunc) +
  geom_function(aes(linetype = "PICAR (AIC)", colour = ".95"), fun = AICq.95prfunc) +
  scale_color_manual(values = c("blue","purple","red"), name= "quantile") +
  scale_linetype_manual(values=c("dotted","dashed", "dotdash"),name = "source")+
  ggtitle("Fit function of precipitation")+ 
  xlab("Precipitation (L)") + ylab("Impact on yield of precipitation") +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

#NOAIC

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/NOAICfitprfunction_qs.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
base +
  geom_function(aes(linetype = "Cty Int", colour = ".50"), fun = CIq.50prfunc) +
  geom_function(aes(linetype = "PICAR", colour = ".50"), fun = BICq.50prfunc) +
  geom_function(aes(linetype = "Cty Int", colour = ".05"), fun = CIq.05prfunc) +
  geom_function(aes(linetype = "PICAR", colour = ".05"), fun = BICq.05prfunc) +
  geom_function(aes(linetype = "Cty Int", colour = ".95"), fun = CIq.95prfunc) +
  geom_function(aes(linetype = "PICAR", colour = ".95"), fun = BICq.95prfunc) +
  scale_color_manual(values = c("blue","black","red"), name= "quantile") +
  scale_linetype_manual(values=c("dotted","dashed"),name = "source")+
  ggtitle("Fit function of precipitation")+ 
  xlab("Precipitation (L)") + ylab("Impact on yield (bushels/acre): precipitation") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()


################################################################################
#quadratic time trend by state function across each stat model

states<- c("AL","AR","DE","GA","IL","IN","IA","KY","LA","MD","MI","MN",
           "MS","MO","NJ","NY","NC","OH","PA","SC","TN","VA","WV","WI")

#load data
load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

nStates<- 24

which(colnames(XMat)=="state1yr");which(colnames(XMat)=="state1yr_sq")

CImeanstatetimeeffects<- 
  colMeans(CI_res[,which(colnames(XMat)=="state1yr"):which(colnames(XMat)=="state55yr_sq")])
BICmeanstatetimeeffects<- 
  colMeans(BIC_res[,which(colnames(XMat)=="state1yr"):which(colnames(XMat)=="state55yr_sq")])
AICmeanstatetimeeffects<- 
  colMeans(AIC_res[,which(colnames(XMat)=="state1yr"):which(colnames(XMat)=="state55yr_sq")])

XMat<- as.data.frame(XMat)
summary(XMat$state1yr[which(XMat$state1yr>0)])*38+1980

################################################################################
#future 1
yr<-"2020_2049"
load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/holdLastYear/future/Crop_",yr,"_cfmpwSpatialData.RData",sep=""))
XMatCV<- as.data.frame(XMatCV)
summary(XMatCV$state1yr[which(XMatCV$state1yr>0)])*38+1980


f1years<- unique(XMatCV$state1yr[which(XMatCV$state1yr>0)])

CImeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)
BICmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)
AICmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)

for(t in 1:length(f1years)){
  for(s in 1:nStates){
    CImeanstatequad[t,s]<- CImeanstatetimeeffects[s]*f1years[t]+ CImeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    BICmeanstatequad[t,s]<-BICmeanstatetimeeffects[s]*f1years[t]+BICmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    AICmeanstatequad[t,s]<-AICmeanstatetimeeffects[s]*f1years[t]+AICmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
  }
}

CImeanstatequadvec<- as.vector(CImeanstatequad)
BICmeanstatequadvec<- as.vector(BICmeanstatequad)
AICmeanstatequadvec<- as.vector(AICmeanstatequad)


CImstq.df<- data.frame(timeEffect= CImeanstatequadvec, 
                       state= as.character(rep(states,each=length(f1years))),
                       year= rep(f1years*38+1980,nStates))
BICmstq.df<- data.frame(timeEffect= BICmeanstatequadvec, 
                        state= as.character(rep(states,each=length(f1years))),
                        year= rep(f1years*38+1980,nStates))
AICmstq.df<- data.frame(timeEffect= AICmeanstatequadvec, 
                        state= as.character(rep(states,each=length(f1years))),
                        year= rep(f1years*38+1980,nStates))


#filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/CIfitStateTimefunction_2020_2049.jpeg",sep="")
#jpeg(file = filename,width = 800,height=600)
##Visualization for observations and predictions
#ggplot(CImstq.df, aes(x = year, y = timeEffect)) + 
#  geom_line(aes(color = state)) + 
#  #scale_color_manual(values = c("blue","purple","red")) +
#  #scale_linetype_manual(values=c("dotted","dashed", "dotdash"))+
#  ggtitle("Fit function of time by state (2020-2049)")+ 
#  xlab("Year") + ylab("Impact on yield of time") +
#  theme_bw()+
#  theme(plot.title = element_text(size=24), 
#        axis.title = element_text(size=24),
#        axis.text = element_text(size = 20),
#       legend.text= element_text(size=24),
#        legend.title= element_text(size=24))
#dev.off()

################################################################################
#future 2
yr<-"2070_2099"
load(paste("/storage/work/svr5482/Climate_CornYield-me/PICAR/holdLastYear/future/Crop_",yr,"_cfmpwSpatialData.RData",sep=""))
XMatCV<- as.data.frame(XMatCV)
summary(XMatCV$state1yr[which(XMatCV$state1yr>0)])*38+1980


f1years<- unique(XMatCV$state1yr[which(XMatCV$state1yr>0)])

CImeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)
BICmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)
AICmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)

for(t in 1:length(f1years)){
  for(s in 1:nStates){
    CImeanstatequad[t,s]<- CImeanstatetimeeffects[s]*f1years[t]+ CImeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    BICmeanstatequad[t,s]<-BICmeanstatetimeeffects[s]*f1years[t]+BICmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    AICmeanstatequad[t,s]<-AICmeanstatetimeeffects[s]*f1years[t]+AICmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
  }
}

CImeanstatequadvec<- as.vector(CImeanstatequad)
BICmeanstatequadvec<- as.vector(BICmeanstatequad)
AICmeanstatequadvec<- as.vector(AICmeanstatequad)


CImstq.df<- data.frame(timeEffect= CImeanstatequadvec, 
                       state= as.character(rep(states,each=length(f1years))),
                       year= rep(f1years*38+1980,nStates))
BICmstq.df<- data.frame(timeEffect= BICmeanstatequadvec, 
                        state= as.character(rep(states,each=length(f1years))),
                        year= rep(f1years*38+1980,nStates))
AICmstq.df<- data.frame(timeEffect= AICmeanstatequadvec, 
                        state= as.character(rep(states,each=length(f1years))),
                        year= rep(f1years*38+1980,nStates))


#filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/CIfitStateTimefunction_2070_2099.jpeg",sep="")
#jpeg(file = filename,width = 800,height=600)
##Visualization for observations and predictions
#ggplot(CImstq.df, aes(x = year, y = timeEffect)) + 
#  geom_line(aes(color = state)) + 
#  #scale_color_manual(values = c("blue","purple","red")) +
#  #scale_linetype_manual(values=c("dotted","dashed", "dotdash"))+
#  ggtitle("Fit function of time by state (2070-2099)")+ 
#  xlab("Year") + ylab("Impact on yield of time") +
#  theme(plot.title = element_text(size=24), 
#        axis.title = element_text(size=24),
#        axis.text = element_text(size = 20),
#        legend.text= element_text(size=24),
#        legend.title= element_text(size=24))
#dev.off()

################################################################################
#historical period

f1years<- (1:18)/38

CImeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)
BICmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)
AICmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)

for(t in 1:length(f1years)){
  for(s in 1:nStates){
    CImeanstatequad[t,s]<- CImeanstatetimeeffects[s]*f1years[t]+ CImeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    BICmeanstatequad[t,s]<-BICmeanstatetimeeffects[s]*f1years[t]+BICmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    AICmeanstatequad[t,s]<-AICmeanstatetimeeffects[s]*f1years[t]+AICmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
  }
}

CImeanstatequadvec<- as.vector(CImeanstatequad)
BICmeanstatequadvec<- as.vector(BICmeanstatequad)
AICmeanstatequadvec<- as.vector(AICmeanstatequad)


CImstq.df<- data.frame(timeEffect= CImeanstatequadvec, 
                       state= as.character(rep(states,each=length(f1years))),
                       year= rep(f1years*38+1980,nStates))
BICmstq.df<- data.frame(timeEffect= BICmeanstatequadvec, 
                        state= as.character(rep(states,each=length(f1years))),
                        year= rep(f1years*38+1980,nStates))
AICmstq.df<- data.frame(timeEffect= AICmeanstatequadvec, 
                        state= as.character(rep(states,each=length(f1years))),
                        year= rep(f1years*38+1980,nStates))


filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/CIfitStateTimefunction_1981_2018.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
#Visualization for observations and predictions
ggplot(CImstq.df, aes(x = year, y = timeEffect)) + 
  geom_line(aes(color = state)) + 
  #scale_color_manual(values = c("blue","purple","red")) +
  #scale_linetype_manual(values=c("dotted","dashed", "dotdash"))+
  ggtitle("Fit function of time by state (1981-2018)")+ 
  xlab("Year") + ylab("Impact on yield of time") +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

################################################################################
#all time

f1years<- ((1981:2099)-1980)/38

CImeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)
BICmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)
AICmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)

for(t in 1:length(f1years)){
  for(s in 1:nStates){
    CImeanstatequad[t,s]<- CImeanstatetimeeffects[s]*f1years[t]+ CImeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    BICmeanstatequad[t,s]<-BICmeanstatetimeeffects[s]*f1years[t]+BICmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    AICmeanstatequad[t,s]<-AICmeanstatetimeeffects[s]*f1years[t]+AICmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
  }
}

CImeanstatequadvec<- as.vector(CImeanstatequad)
BICmeanstatequadvec<- as.vector(BICmeanstatequad)
AICmeanstatequadvec<- as.vector(AICmeanstatequad)


CImstq.df<- data.frame(timeEffect= CImeanstatequadvec, 
                       state= as.character(rep(states,each=length(f1years))),
                       year= rep(f1years*38+1980,nStates))
BICmstq.df<- data.frame(timeEffect= BICmeanstatequadvec, 
                        state= as.character(rep(states,each=length(f1years))),
                        year= rep(f1years*38+1980,nStates))
AICmstq.df<- data.frame(timeEffect= AICmeanstatequadvec, 
                        state= as.character(rep(states,each=length(f1years))),
                        year= rep(f1years*38+1980,nStates))


filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/CIfitStateTimefunction_1981_2099.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
#Visualization for observations and predictions
ggplot(CImstq.df, aes(x = year, y = timeEffect)) + 
  geom_line(aes(color = state)) + 
  #scale_color_manual(values = c("blue","purple","red")) +
  #scale_linetype_manual(values=c("dotted","dashed", "dotdash"))+
  ggtitle("County intercepts: fit function of time by state")+ 
  xlab("Year") + ylab("Impact on yield (bushels/acre): time") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/BICfitStateTimefunction_1981_2099.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
#Visualization for observations and predictions
ggplot(BICmstq.df, aes(x = year, y = timeEffect)) + 
  geom_line(aes(color = state)) + 
  #scale_color_manual(values = c("blue","purple","red")) +
  #scale_linetype_manual(values=c("dotted","dashed", "dotdash"))+
  ggtitle("PICAR: fit function of time by state")+ 
  xlab("Year") + ylab("Impact on yield (bushels/acre): time") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/AICfitStateTimefunction_1981_2099.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
#Visualization for observations and predictions
ggplot(AICmstq.df, aes(x = year, y = timeEffect)) + 
  geom_line(aes(color = state)) + 
  #scale_color_manual(values = c("blue","purple","red")) +
  #scale_linetype_manual(values=c("dotted","dashed", "dotdash"))+
  ggtitle("PICAR (AIC) fit function of time by state")+ 
  xlab("Year") + ylab("Impact on yield (bushels/acre): time") +
  theme_bw()+
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()

CImstq_lastyr<- CImstq.df[CImstq.df$year==2099,]
BICmstq_lastyr<- BICmstq.df[BICmstq.df$year==2099,]
AICmstq_lastyr<- AICmstq.df[AICmstq.df$year==2099,]
