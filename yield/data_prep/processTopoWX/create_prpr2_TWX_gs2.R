#add precip to tempdist_GS2

rm(list=ls())

load("/storage/work/svr5482/Countywise/Metdata/Metdataframe/upto2018/Metpr")

startday<- 365+366+1
endday<- 365*28+366*10
pr<- pr[,startday:endday]

#use Growing season computed with TopoWX data
load("/storage/work/svr5482/Countywise/TopoWX/GS_start")
load("/storage/work/svr5482/Countywise/TopoWX/GS_end")

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
load("/storage/work/svr5482/Climate_CornYield-master/ANSI")

countyyears=rep(ANSI,each=yrnum)

#precipitation stuff
#get the total precipitation for each county for each year during the growing season
totpr_GS<- rep(NA, countynum*yrnum)

for(i in 1:countynum){
  for(j in 1:yrnum){
    year<- which(yeardays==1980+j)
    prec<- pr[i,year]
    totpr_GS[(i-1)*yrnum+j]<- sum(prec[c(GS_start[i,j]:GS_end[i,j])])
  }
}


totpr_GS2<- totpr_GS^2
pr_pr2_GS<- cbind(totpr_GS, totpr_GS2)


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
df<- cbind(df2,pr_pr2_GS)
df<-df[complete.cases(df), ]

pr_pr2_GS<- df[,7:8]
colnames(pr_pr2_GS)<- c("Pr_GS", "Pr_GS2")

save(pr_pr2_GS, file= "/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/pr_pr2_GS2.RData")
