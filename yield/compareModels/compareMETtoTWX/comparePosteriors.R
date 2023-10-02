rm(list=ls())

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX")
}

setwd(paste("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output",sep=""))
load("samples_cfmpw_fast.RData")

CI_res<- res[1:15001,]

load("samples_cfmpw_TWX.RData")

TWX_res<- res[1:15001,]

rm(res)
################################################################################

Pr.df<- data.frame("value"= c(CI_res[,2],TWX_res[,2]),
                   "source"= c(rep("MET",nrow(CI_res)),
                               rep("TWX",nrow(TWX_res))))

Pr2.df<- data.frame("value"= c(CI_res[,3],TWX_res[,3]),
                    "source"= c(rep("MET",nrow(CI_res)),
                                rep("TWX",nrow(TWX_res))))

T1.df<- data.frame("value"= c(CI_res[,4],TWX_res[,4]),
                   "source"= c(rep("MET",nrow(CI_res)),
                               rep("TWX",nrow(TWX_res))))

T2.df<- data.frame("value"= c(CI_res[,5],TWX_res[,5]),
                   "source"= c(rep("MET",nrow(CI_res)),
                               rep("TWX",nrow(TWX_res))))


#jpeg(filename=paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX/Pr_densities.jpeg",sep=""),
#     width = 800, height = 600)
#ggplot(Pr.df, aes(x=value, fill=source)) +
#  geom_density(alpha=.25)+
#  ggtitle("Pr1")+
#  #geom_text(x=.031, y=650, label=expression("n"["ch"]^"*1"*"=.0305"))+
#  theme(plot.title = element_text(size=24), 
#        axis.title = element_text(size=24),
#        axis.text = element_text(size = 20),
#        legend.text= element_text(size=24),
#        legend.title= element_text(size=24))
#dev.off()

#jpeg(filename=paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX/Pr2_densities.jpeg",sep=""),
#     width = 800, height = 600)
#ggplot(Pr2.df, aes(x=value, fill=source)) +
#  geom_density(alpha=.25)+
#  ggtitle("Pr2")+
#  #geom_text(x=.031, y=650, label=expression("n"["ch"]^"*1"*"=.0305"))+
#  theme(plot.title = element_text(size=24), 
#        axis.title = element_text(size=24),
#        axis.text = element_text(size = 20),
#        legend.text= element_text(size=24),
#        legend.title= element_text(size=24))
#dev.off()

#jpeg(filename=paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX/T1_densities.jpeg",sep=""),
#     width = 800, height = 600)
#ggplot(T1.df, aes(x=value, fill=source)) +
#  geom_density(alpha=.25)+
#  ggtitle("T1")+
#  #geom_text(x=.031, y=650, label=expression("n"["ch"]^"*1"*"=.0305"))+
#  theme(plot.title = element_text(size=24), 
#        axis.title = element_text(size=24),
#        axis.text = element_text(size = 20),
#        legend.text= element_text(size=24),
#        legend.title= element_text(size=24))
#dev.off()

#jpeg(filename=paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX/T2_densities.jpeg",sep=""),
#     width = 800, height = 600)
#ggplot(T2.df, aes(x=value, fill=source)) +
#  geom_density(alpha=.25)+
#  ggtitle("T2")+
#  #geom_text(x=.031, y=650, label=expression("n"["ch"]^"*1"*"=.0305"))+
#  theme(plot.title = element_text(size=24), 
#        axis.title = element_text(size=24),
#        axis.text = element_text(size = 20),
#        legend.text= element_text(size=24),
#        legend.title= element_text(size=24))
#dev.off()


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


CIheatlines<- matrix(NA,nrow=nrow(CI_res)-1,ncol=48-9)
TWXheatlines<- matrix(NA,nrow=nrow(TWX_res)-1,ncol=48-9)

for(i in 2:nrow(CI_res)){
  
  CIheatfunc<- function(x) heatfunc(CI_res[i,4]/24000,CI_res[i,5]/24000,x)
  TWXheatfunc<- function(x) heatfunc(TWX_res[i,4]/24000,TWX_res[i,5]/24000,x)
  
  CIheatlines[i-1,]<- sapply(10:48,CIheatfunc)
  TWXheatlines[i-1,]<- sapply(10:48,TWXheatfunc)
  
}

#is yield ever increasing with exposure to temperature above 29C? NO
#is yield ever decreasing with exposure to temperature from 10 to 29C? NO

#CI

CI_20minus10<- apply(CIheatlines,1, function(x) x[20] - x[1] )
CI_39minus20<- apply(CIheatlines,1, function(x) x[39] - x[20] )

which(CI_20minus10<=0); which(CI_39minus20>=0)

which.min(CI_20minus10); which.max(CI_20minus10)
which.min(CI_39minus20); which.max(CI_39minus20)

min(CI_20minus10); max(CI_20minus10)
min(CI_39minus20); max(CI_39minus20)

#TWX

TWX_20minus10<- apply(TWXheatlines,1, function(x) x[20] - x[1] )
TWX_39minus20<- apply(TWXheatlines,1, function(x) x[39] - x[20] )

which(TWX_20minus10<=0); which(TWX_39minus20>=0)

which.min(TWX_20minus10); which.max(TWX_20minus10)
which.min(TWX_39minus20); which.max(TWX_39minus20)

min(TWX_20minus10); max(TWX_20minus10)
min(TWX_39minus20); max(TWX_39minus20)

#.50

CIq.50heatfunc<- function(x) heatfunc(quantile(CI_res[,4],.50)/24000,quantile(CI_res[,5],.5)/24000,x)
TWXq.50heatfunc<- function(x) heatfunc(quantile(TWX_res[,4],.50)/24000,quantile(TWX_res[,5],.5)/24000,x)

#convert to impact of exposure for one hour to a temperature interval

CIq.50heatline<- sapply(10:48,CIq.50heatfunc)
TWXq.50heatline<- sapply(10:48,TWXq.50heatfunc)

#.05

CIq.05heatfunc<- function(x) heatfunc(quantile(CI_res[,4],.05)/24000,quantile(CI_res[,5],.05)/24000,x)
TWXq.05heatfunc<- function(x) heatfunc(quantile(TWX_res[,4],.05)/24000,quantile(TWX_res[,5],.05)/24000,x)
#convert to impact of exposure for one hour to a temperature interval

CIq.05heatline<- sapply(10:48,CIq.05heatfunc)
TWXq.05heatline<- sapply(10:48,TWXq.05heatfunc)

#.95

CIq.95heatfunc<- function(x) heatfunc(quantile(CI_res[,4],.95)/24000,quantile(CI_res[,5],.95)/24000,x)
TWXq.95heatfunc<- function(x) heatfunc(quantile(TWX_res[,4],.95)/24000,quantile(TWX_res[,5],.95)/24000,x)

#convert to impact of exposure for one hour to a temperature interval

CIq.95heatline<- sapply(10:48,CIq.95heatfunc)
TWXq.95heatline<- sapply(10:48,TWXq.95heatfunc)

heatlines.df<- data.frame(source=rep(c(rep("MET",length(10:48)),
                                       rep("TWX",length(10:48))),3),
                          temp=rep(10:48,6),
                          quantile=as.character(rep(c(.5,.05,.95),each=2*length(10:48))),
                          heatline=c(CIq.50heatline,TWXq.50heatline,
                                     CIq.05heatline,TWXq.05heatline,
                                     CIq.95heatline,TWXq.95heatline))


filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX/fitheatfunction_qs.jpeg",sep="")
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
TWXq.50prfunc<- function(x) prfunc(quantile(TWX_res[,2],.50),quantile(TWX_res[,3],.50),x)

#convert to impact of exposure for one hour to a temperature interval

CIq.50prline<- sapply(10:48,CIq.50prfunc)
TWXq.50prline<- sapply(10:48,TWXq.50prfunc)

#.05

CIq.05prfunc<- function(x) prfunc(quantile(CI_res[,2],.05),quantile(CI_res[,3],.05),x)
TWXq.05prfunc<- function(x) prfunc(quantile(TWX_res[,2],.05),quantile(TWX_res[,3],.05),x)

#convert to impact of exposure for one hour to a temperature interval

CIq.05prline<- sapply(10:48,CIq.05prfunc)
TWXq.05prline<- sapply(10:48,TWXq.05prfunc)

#.95

CIq.95prfunc<- function(x) prfunc(quantile(CI_res[,2],.95),quantile(CI_res[,3],.95),x)
TWXq.95prfunc<- function(x) prfunc(quantile(TWX_res[,2],.95),quantile(TWX_res[,3],.95),x)

base <-ggplot() + xlim(min(XMat[,2]), max(XMat[,2]))

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX/fitprfunction_qs.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
base +
  geom_function(aes(linetype = "MET", colour = ".50"), fun = CIq.50prfunc) +
  geom_function(aes(linetype = "TWX", colour = ".50"), fun = TWXq.50prfunc) +
  geom_function(aes(linetype = "MET", colour = ".05"), fun = CIq.05prfunc) +
  geom_function(aes(linetype = "TWX", colour = ".05"), fun = TWXq.05prfunc) +
  geom_function(aes(linetype = "MET", colour = ".95"), fun = CIq.95prfunc) +
  geom_function(aes(linetype = "TWX", colour = ".95"), fun = TWXq.95prfunc) +
  scale_color_manual(values = c("blue","purple","red"), name= "quantile") +
  scale_linetype_manual(values=c("dotted","dashed"),name = "source")+
  ggtitle("Fit function of precipitation")+ 
  xlab("Precipitation (L)") + ylab("Impact on yield of precipitation") +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
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
TWXmeanstatetimeeffects<- 
  colMeans(TWX_res[,which(colnames(XMat)=="state1yr"):which(colnames(XMat)=="state55yr_sq")])

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
TWXmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)

for(t in 1:length(f1years)){
  for(s in 1:nStates){
    CImeanstatequad[t,s]<- CImeanstatetimeeffects[s]*f1years[t]+ CImeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    TWXmeanstatequad[t,s]<-TWXmeanstatetimeeffects[s]*f1years[t]+TWXmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    
  }
}

CImeanstatequadvec<- as.vector(CImeanstatequad)
TWXmeanstatequadvec<- as.vector(TWXmeanstatequad)



CImstq.df<- data.frame(timeEffect= CImeanstatequadvec, 
                       state= as.character(rep(states,each=length(f1years))),
                       year= rep(f1years*38+1980,nStates))
TWXmstq.df<- data.frame(timeEffect= TWXmeanstatequadvec, 
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
TWXmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)

for(t in 1:length(f1years)){
  for(s in 1:nStates){
    CImeanstatequad[t,s]<- CImeanstatetimeeffects[s]*f1years[t]+ CImeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    TWXmeanstatequad[t,s]<-TWXmeanstatetimeeffects[s]*f1years[t]+TWXmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
  }
}

CImeanstatequadvec<- as.vector(CImeanstatequad)
TWXmeanstatequadvec<- as.vector(TWXmeanstatequad)


CImstq.df<- data.frame(timeEffect= CImeanstatequadvec, 
                       state= as.character(rep(states,each=length(f1years))),
                       year= rep(f1years*38+1980,nStates))
TWXmstq.df<- data.frame(timeEffect= TWXmeanstatequadvec, 
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
TWXmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)

for(t in 1:length(f1years)){
  for(s in 1:nStates){
    CImeanstatequad[t,s]<- CImeanstatetimeeffects[s]*f1years[t]+ CImeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    TWXmeanstatequad[t,s]<-TWXmeanstatetimeeffects[s]*f1years[t]+TWXmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
  }
}

CImeanstatequadvec<- as.vector(CImeanstatequad)
TWXmeanstatequadvec<- as.vector(TWXmeanstatequad)


CImstq.df<- data.frame(timeEffect= CImeanstatequadvec, 
                       state= as.character(rep(states,each=length(f1years))),
                       year= rep(f1years*38+1980,nStates))
TWXmstq.df<- data.frame(timeEffect= TWXmeanstatequadvec, 
                        state= as.character(rep(states,each=length(f1years))),
                        year= rep(f1years*38+1980,nStates))


filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX/CIfitStateTimefunction_1981_2018.jpeg",sep="")
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

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX/TWXfitStateTimefunction_1981_2018.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
#Visualization for observations and predictions
ggplot(TWXmstq.df, aes(x = year, y = timeEffect)) + 
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
TWXmeanstatequad<- matrix(NA,nrow=length(f1years),ncol=nStates)

for(t in 1:length(f1years)){
  for(s in 1:nStates){
    CImeanstatequad[t,s]<- CImeanstatetimeeffects[s]*f1years[t]+ CImeanstatetimeeffects[s+nStates]*(f1years[t]^2)
    TWXmeanstatequad[t,s]<-TWXmeanstatetimeeffects[s]*f1years[t]+TWXmeanstatetimeeffects[s+nStates]*(f1years[t]^2)
  }
}

CImeanstatequadvec<- as.vector(CImeanstatequad)
TWXmeanstatequadvec<- as.vector(TWXmeanstatequad)

CImstq.df<- data.frame(timeEffect= CImeanstatequadvec, 
                       state= as.character(rep(states,each=length(f1years))),
                       year= rep(f1years*38+1980,nStates))
TWXmstq.df<- data.frame(timeEffect= TWXmeanstatequadvec, 
                        state= as.character(rep(states,each=length(f1years))),
                        year= rep(f1years*38+1980,nStates))


filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/METvsTWX/TWXfitStateTimefunction_1981_2099.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
#Visualization for observations and predictions
ggplot(TWXmstq.df, aes(x = year, y = timeEffect)) + 
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

filename<-paste("/storage/work/svr5482/Climate_CornYield-me/yield/compareModels/plots/CIfitStateTimefunction_1981_2099.jpeg",sep="")
jpeg(file = filename,width = 800,height=600)
#Visualization for observations and predictions
ggplot(CImstq.df, aes(x = year, y = timeEffect)) + 
  geom_line(aes(color = state)) + 
  #scale_color_manual(values = c("blue","purple","red")) +
  #scale_linetype_manual(values=c("dotted","dashed", "dotdash"))+
  ggtitle("TWX: fit function of time by state")+ 
  xlab("Year") + ylab("Impact on yield (bushels/acre): time") +
  theme_bw() +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))
dev.off()


CImstq_lastyr<- CImstq.df[CImstq.df$year==2099,]
TWXmstq_lastyr<- TWXmstq.df[TWXmstq.df$year==2099,]
