#create_df_metdata.R but for the TopoWX data
rm(list=ls())

load(paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmax.RData"))
load(paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmin.RData"))

#use Growing season computed with TopoWX data
load("/storage/work/svr5482/Countywise/TopoWX/GS_start")
load("/storage/work/svr5482/Countywise/TopoWX/GS_end")

load("/storage/work/svr5482/Climate_CornYield-master/ANSI")

#REDUCE NUMBER OF YEARS STARTING HERE

#number of years, counties, growing seaon length
yrnum<- 36 #1981-2016
countynum<- 1878
gslen<- 185 #in days


#create a vector that keeps track of the year each day is in
yeardays<- NA
for (i in 1:yrnum){ #changed 37 to 39
  if(!((1980+i)%%4)) yeardays<- c(yeardays, rep(1980+i,366))
  if((1980+i)%%4) yeardays<- c(yeardays, rep(1980+i,365))
}
yeardays<- yeardays[-1]

#goal: create data frame where each row is for a county in a given year, 1981-2016
#need variable to represent amt of time spent in each 1 C interval during growing season
#growing season can be defined as March - August or 
#the 6 months after the 21 day moving average hits temp threshold
#also need as variables: year, fips, StateANSI, CountyANSI, Pr (during growing season)

countyyears=rep(ANSI,each=yrnum)

##need a yrnum*countynum row dataframe (one row for each year in each county)
df<- data.frame(
  "year"= rep(1980+(1:yrnum), countynum),
  "fips"= countyyears
)

#create a dataframe to hold the growing season daily maximum temperatures
tmax_gs_temps<- matrix(NA, nrow= countynum, ncol= gslen*yrnum)
tmin_gs_temps<- matrix(NA, nrow= countynum, ncol= gslen*yrnum)

#create a dataframe for the daily max and daily min temps during the growing season
#in each year in each county
for(i in 1:countynum){
  for(j in 1:yrnum){
    year<- which(yeardays==1980+j)
    tmx<- tmax[i,year]
    tmn<- tmin[i,year]
    tmx_GS<- tmx[c(GS_start[i,j]:GS_end[i,j])]
    tmn_GS<- tmn[c(GS_start[i,j]:GS_end[i,j])]
    tmax_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]<- tmx_GS
    tmin_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]<- tmn_GS
  }
}

length(which(is.na(tmax_gs_temps)))
length(which(is.na(tmin_gs_temps)))

##Find the max max temp and min min temp for all the growing seasons, set interval
low<- floor(min(tmin_gs_temps))
high<- ceiling(max(tmax_gs_temps))
T_interval=c(low, high)

#Calculate the amount of time spent in each 1 C interval
source("/storage/work/svr5482/Climate_CornYield-me/data_prep/T_distribution.R")

temp_dist_GS<- matrix(NA, nrow= countynum*yrnum, ncol= (high-low))
for(i in 1:countynum){
  for(j in 1:yrnum){
    Tmax<- tmax_gs_temps[i,(1+(j-1)*gslen):(j*gslen)]
    Tmin<- tmin_gs_temps[i,(1+(j-1)*gslen):(j*gslen)]
    td<- T_distribution(Tmax= Tmax, Tmin= Tmin, T_interval= T_interval)
    temp_dist_GS[(i-1)*yrnum+j,]<- td
  }
}


tempnames<- seq(low,high-1, by=1)
colnames(temp_dist_GS)<- as.character(tempnames)

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX")}

save(temp_dist_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/temp_dist_GS2.RData")

################################################################################
#p2

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/temp_dist_GS2.RData")

load("/storage/work/svr5482/Climate_CornYield-master/CountyANSI")
load("/storage/work/svr5482/Climate_CornYield-master/StateANSI")
load("/storage/work/svr5482/Climate_CornYield-master/ANSI")

yrnum=40
countynum=1878

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

df2$year=df2$year+1978
df2<- df2[which(df2$year%in%seq(1981,2016,by=1)),]
df<- cbind(df2,temp_dist_GS)
df<-df[complete.cases(df), ]

temp_dist_GS<- df[,7:80]

save(temp_dist_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/temp_dist_GS2.RData")

#load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_df")

#timeloc_df<- timeloc_df[which(timeloc_df$year%in%seq(1981,2016,by=1)),]
