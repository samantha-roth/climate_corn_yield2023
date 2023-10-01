#created: 03/31/2021, updated 09/07/2022
#combine data sources into one data frame for the years 1981-2016
rm(list = ls())
graphics.off()

setwd("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me")

#load all relevant observations of each variable for each day in each county in the 38 years of observations
#load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Metpr")
#load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Mettmax")
#load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Mettmin")

#up to 2018
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/upto2018/Metpr")
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/upto2018/Mettmax")
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/upto2018/Mettmin")

#load the Data dataframe as a reference guide to format my dataframe
#load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/Data_Metobs")

#load the day of the year of the beginning and end of the growing season 
#for each year (1979-2016) for each of the 1878 counties
#load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/MetGS_start")
#load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/MetGS_end")

#up to 2018
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/upto2018/MetGS_start")
load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/upto2018/MetGS_end")

load("/storage/work/svr5482/Climate_CornYield-master/ANSI")

#number of years, counties, growing seaon length
yrnum<- 40 #1979-2018
countynum<- 1878
gslen<- 185 #in days

#create a vector that keeps track of the year each day is in
yeardays<- NA
for (i in 1:yrnum){ #changed 37 to 39
  if(!((1978+i)%%4)) yeardays<- c(yeardays, rep(1978+i,366))
  if((1978+i)%%4) yeardays<- c(yeardays, rep(1978+i,365))
}
yeardays<- yeardays[-1]

#goal: create data frame where each row is for a county in a given year, 1981-2012
#need variable to represent amt of time spent in each 1 C interval during growing season
#growing season can be defined as March - August or 
#the 6 months after the 21 day moving average hits temp threshold
#also need as variables: year, fips, StateANSI, CountyANSI, Pr (during growing season)

countyyears=rep(ANSI,each=yrnum)

##need a yrnum*countynum row dataframe (one row for each year in each county)
df<- data.frame(
  "year"= rep(1978+(1:yrnum), countynum),
  "fips"= countyyears
)

#create a dataframe to hold the growing season daily maximum temperatures
tmax_gs_temps<- matrix(NA, nrow= countynum, ncol= gslen*yrnum)
tmin_gs_temps<- matrix(NA, nrow= countynum, ncol= gslen*yrnum)

#create a dataframe for the daily max and daily min temps during the growing season
#in each year in each county
for(i in 1:countynum){
  for(j in 1:yrnum){
    year<- which(yeardays==1978+j)
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

##save growing season daily max and min temps
#save(tmax_gs_temps, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmax_gs_temps")
#save(tmin_gs_temps, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmin_gs_temps")

##after running lines 1-84, can comment out lines 50-84 and just run the lines below
##load growing season daily max and min temps
#load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmax_gs_temps")
#load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/tmin_gs_temps")

##Find the max max temp and min min temp for all the growing seasons, set interval
low<- floor(min(tmin_gs_temps))
high<- ceiling(max(tmax_gs_temps))
T_interval=c(low, high)


##Calculate the amount of time spent in each 1 C interval
##source("/gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/T_distribution.R") #function to be used
##source("/storage/work/svr5482/Climate_CornYield-me/T_distribution.R")
#source("/storage/work/svr5482/Climate_CornYield-me/data_prep/T_distribution.R")

#temp_dist_GS<- matrix(NA, nrow= countynum*yrnum, ncol= (high-low))
#for(i in 1:countynum){
#  #time1<- Sys.time()
#  for(j in 1:yrnum){
#    Tmax<- tmax_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]
#    Tmin<- tmin_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]
#    td<- T_distribution(Tmax= Tmax, Tmin= Tmin, T_interval= T_interval)
#    temp_dist_GS[(i-1)*yrnum+j,]<- td
#  }
#  #time<- Sys.time()- time1
#}

#save(temp_dist_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/temp_dist_GS")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/temp_dist_GS")

#precipitation stuff
#get the total precipitation for each county for each year during the growing season
totpr_GS<- rep(NA, countynum*yrnum)

for(i in 1:countynum){
  for(j in 1:yrnum){
    year<- which(yeardays==1978+j)
    prec<- pr[i,year]
    totpr_GS[(i-1)*yrnum+j]<- sum(prec[c(GS_start[i,j]:GS_end[i,j])])
  }
}

#save(totpr_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/totpr_GS")
#load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/totpr_GS")

totpr_GS2<- totpr_GS^2
df<- cbind(totpr_GS, totpr_GS2, temp_dist_GS)


##Excerpt from METrawdata_dataframe.R by Haochen Ye (https://github.com/yhaochen/Climate_CornYield)
load("/storage/work/svr5482/Climate_CornYield-master/CountyANSI")
load("/storage/work/svr5482/Climate_CornYield-master/StateANSI")
load("/storage/work/svr5482/Climate_CornYield-master/ANSI")

S1<-read.table("/storage/work/svr5482/Climate_CornYield-master/harvest_area.csv",header=TRUE,sep=",") #previously S
S2<- read.table("/storage/work/svr5482/Climate_CornYield-master/harvest_2013-2018.csv",header=TRUE,sep=",") 
S<- rbind(S2[which(S2$Year<= (1978+yrnum)),],S1)

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

df<- cbind(df2,df)
df<-df[complete.cases(df), ] #Yield data are 1981-2018 (instead of 2012 now)
df$year=df$year+1978
save(df,low,high, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")

#rename the temperature distribution columns to correspond to the upper temperature of 
#each 1degree C temperature intervals i.e. the amount of time spent in (-29,-28) is called -28
#mininum temp: -29, maximum temp: 48

#columns 9-85 are amount of time spent in each 1 degree interval
tempnames<- seq(low,high-1, by=1)

tempInd1<- which(colnames(df)=="totpr_GS2")+1
colnames(df)[tempInd1:ncol(df)]<- as.character(tempnames)

PrGSInd<- which(colnames(df)=="totpr_GS")
PrGS2Ind<- which(colnames(df)=="totpr_GS2")
colnames(df)[PrGSInd:PrGS2Ind]<- c("Pr_GS", "Pr_GS2")

##find unique state ansi values
states<- unique(df$StateANSI)

stateyears<- as.data.frame(matrix(0, nrow= nrow(df), ncol= length(states)))
for(i in 1:ncol(stateyears)) colnames(stateyears)[i]<- paste("state",states[i],"yr",sep="")

for(i in 1:ncol(stateyears)){
  stateyears[which(df$StateANSI==states[i]),i]<- df$year[which(df$StateANSI==states[i])]-1980
}

stateyears_sq<- stateyears^2
for(i in 1:ncol(stateyears_sq)) colnames(stateyears_sq)[i]<- paste("state",states[i],"yr_sq",sep="")

#columns 86-133 are stateyears and stateyears_sq
df<- cbind(df, stateyears, stateyears_sq)

##create a fixed effect for each year
##create the portion of the dataframe containing indicators for the fips codes
#uyear<- min(df$year):max(df$year)

#yearcode<- matrix(0, nrow= nrow(df), ncol= length(uyear))
#yearcode<- as.data.frame(yearcode)

#for(i in 1:length(uyear)) colnames(yearcode)[i]<- paste("year",uyear[i],sep="")
#for(i in 1:length(uyear)) yearcode[which(df$year==uyear[i]),i]<- 1

#df<- cbind(df, yearcode)

#create the portion of the dataframe containing indicators for the fips codes
uniquefips<- unique(df$fips)
ufips<- as.character(uniquefips)

fipscode<- matrix(0, nrow= nrow(df), ncol= length(ufips))
fipscode<- as.data.frame(fipscode)

for(i in 1:length(ufips)) colnames(fipscode)[i]<- paste("fips",uniquefips[i],sep="")
for(i in 1:length(ufips)) fipscode[which(df$fips==uniquefips[i]),i]<- 1

df<- cbind(df, fipscode)
save(df,low,high, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")
