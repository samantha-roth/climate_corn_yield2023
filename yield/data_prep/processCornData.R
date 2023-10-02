#created: 03/31/2021, updated 09/07/2022
#combine data sources into one data frame for the years 1981-2018
rm(list = ls())
graphics.off()

load("/storage/work/svr5482/Climate_CornYield-master/ANSI")

#number of years, counties, growing seaon length
yrnum<- 40 #1979-2018
countynum<- 1878


##Excerpt from METrawdata_dataframe.R by Haochen Ye (https://github.com/yhaochen/Climate_CornYield)
load("/storage/work/svr5482/Climate_CornYield-master/CountyANSI")
load("/storage/work/svr5482/Climate_CornYield-master/StateANSI")
load("/storage/work/svr5482/Climate_CornYield-master/ANSI")

S1<-read.table("/storage/work/svr5482/Climate_CornYield-master/harvest_area.csv",header=TRUE,sep=",") #previously S
S2<- read.table("/storage/work/svr5482/Climate_CornYield-master/harvest_2013-2018.csv",header=TRUE,sep=",") 
S<- rbind(S2[which(S2$Year<= (1978+yrnum) ),],S1)

area<-rep(NA,yrnum*countynum)
for (i in 1:countynum){
  stateindex<-which(S$State.ANSI==StateANSI[i])
  countyindex<-which(S$County.ANSI==CountyANSI[i])
  yearindex<-intersect(stateindex,countyindex)
  year<-S$Year[yearindex]
  areatoget<-rep(NA,yrnum)
  for (j in 1:yrnum){
    ind<-which(year==1978+j)
    if (!identical(ind,integer(0))){
      areatoget[j]<-S$Value[yearindex[ind]]
    }
  }
  area[((i-1)*yrnum+1):(i*yrnum)]<-areatoget
}

W1<-read.table("/storage/work/svr5482/Climate_CornYield-master/yielddata.csv",header=TRUE,sep=",")
W2<-read.table("/storage/work/svr5482/Climate_CornYield-master/yielddata_2013-2020.csv",header=TRUE,sep=",")
W<- rbind(W2[which(W2$Year<= (1978+yrnum) ),],W1[which(W1$Year<2013 ),])

yield<-rep(NA,yrnum*countynum)
for (i in 1:countynum){
  stateindex<-which(W$State.ANSI==StateANSI[i])
  countyindex<-which(W$County.ANSI==CountyANSI[i])
  yearindex<-intersect(stateindex,countyindex)
  year<-W$Year[yearindex]
  yieldtoget<-rep(NA,yrnum)
  for (j in 1:yrnum){
    ind<-which(year==1978+j)
    if (!identical(ind,integer(0))){
      yieldtoget[j]<-W$Value[yearindex[ind]]
    }
  }
  yield[((i-1)*yrnum+1):(i*yrnum)]<-yieldtoget
}
##End excerpt

df2<- data.frame(StateANSI=rep(StateANSI,each=yrnum),countyANSI=rep(CountyANSI,each=yrnum), fips=rep(ANSI,each=yrnum),
                 year=rep(c(1:yrnum),countynum),yield=yield,area=area)

df<- df2
df<-df[complete.cases(df), ] #Yield data are 1981-2016 (instead of 2012 now)
df$year=df$year+1978

save(df,file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/cornData.RData")
