#create the rest of the columns of the data frame besides the functions of
#precipitation and temperature


  rm(list=ls())
  k=1
  
  library(housingData)
  
  setwd("/storage/work/svr5482")
  
  load("/storage/work/svr5482/Climate_CornYield-master/CountyANSI")
  load("/storage/work/svr5482/Climate_CornYield-master/StateANSI")
  load("/storage/work/svr5482/Climate_CornYield-master/ANSI")
  
  load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")
  ufips<- unique(df$fips); rm(df)
  
  yrnum<- 30
  obscountynum<- length(ufips)
  countynum<- 1878
  gslen<- 185 #in days
  
  #save growing season daily max and min temps
  if(k==1) yr1= 2020
  if(k==2) yr1= 2069
  
  df2<- data.frame(StateANSI=rep(StateANSI,each=yrnum),
                   countyANSI=rep(CountyANSI,each=yrnum), 
                   fips=rep(ANSI,each=yrnum),
                   year=rep(c(1:yrnum),countynum))
  
  df<- df2[which(df2$fips%in%ufips),]
  
  
  df$year=df$year+yr1-1
  
  #find unique state ansi values
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
  
  fipscode<- matrix(0, nrow= nrow(df), ncol= length(ufips))
  fipscode<- as.data.frame(fipscode)
  
  for(i in 1:length(ufips)) colnames(fipscode)[i]<- paste("fips",ufips[i],sep="")
  for(i in 1:length(ufips)) fipscode[which(df$fips==ufips[i]),i]<- 1
  
  df<- cbind(df, fipscode)
  
  pwl_data<- df[,-c(1:(which(colnames(df)=="year")))]
  
  ind1<- which(colnames(pwl_data)=="state1yr")
  ind2<- which(colnames(pwl_data)=="state55yr")
  ind3<- which(colnames(pwl_data)=="state55yr_sq")
  
  n.styr<- pwl_data[,ind1:ind2]
  
  for(i in 1:ncol(n.styr)){
    #scale instead of standardize to avoid zeros where they shouldn't be
    #n.styr[which(n.styr[,i]>0),i]<- normalize(n.styr[which(n.styr[,i]>0),i], method = "scale")
    #divide all columns by 38 to be consistent with previous weighting
    n.styr[,i]<- n.styr[,i]/38
  }
  n.styr2<- n.styr^2
  
  pwl_norm<- pwl_data
  pwl_norm[,ind1:ind3]<- cbind(n.styr,n.styr2)
  
  
  #create_timeloc_df_future.R
  
  #save locations to be used in analysis in the same order in a more useful form
  loc_df<- as.data.frame(df$fips)
  
  loc_df$lat<- rep(NA,nrow(df))
  loc_df$lon<- rep(NA,nrow(df))
  
  ufips<- as.character(unique(df$fips))
  fips<- as.character(df$fips)
  geofips<- as.character(geoCounty$fips)
  
  for(i in 1:length(ufips)){
    val<- ufips[i]
    loc_df$lat[which(fips==val)]<- geoCounty$lat[which(geofips==val)]
    loc_df$lon[which(fips==val)]<- geoCounty$lon[which(geofips==val)]
  }
  
  colnames(loc_df)[1]<- "fips"
  
  timeloc_df<- cbind(loc_df,df$StateANSI,df$countyANSI,df$year)
  colnames(timeloc_df)[4:6]<- c("StateANSI","countyANSI","year")
  timeloc_df$StateANSI<- as.factor(timeloc_df$StateANSI)
  timeloc_df$countyANSI<- as.factor(timeloc_df$countyANSI)
  
  #format the data so it's in order of year instead of location
  
  year<- as.character(timeloc_df$year)
  year<- as.numeric(year)
  
  year<- year[order(year)]
  
  pwln.ord<- matrix(NA, ncol= ncol(pwl_norm), nrow= nrow(pwl_norm))
  pwln.ord<- as.data.frame(pwln.ord)
  colnames(pwln.ord)<- colnames(pwl_norm)
  
  for(i in 1:yrnum){
    inds<- which(timeloc_df$year==yr1-1+i)
    pwln.ord[which(year==yr1-1+i),]<- pwl_norm[inds,]
  }
  
  #format timeloc_df so it's in order of year instead of location
  
  timeloc.ord<- matrix(NA, ncol= ncol(timeloc_df), nrow= nrow(timeloc_df))
  timeloc.ord<- as.data.frame(timeloc.ord)
  colnames(timeloc.ord)<- colnames(timeloc_df)
  
  timeloc_df$fips<- as.character(timeloc_df$fips)
  timeloc_df$StateANSI<- as.character(timeloc_df$StateANSI)
  timeloc_df$countyANSI<- as.character(timeloc_df$countyANSI)
  #timeloc_df$year<- as.character(timeloc_df$year)
  
  for(i in 1:yrnum){
    inds<- which(timeloc_df$year==yr1-1+i)
    timeloc.ord[which(year==yr1-1+i),]<- timeloc_df[inds,]
  }
  
  
  if(k==1){
    save(timeloc.ord, file= "Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/timeloc_yrorder_2020_2049")
  }
  if(k==2){
    save(timeloc.ord, file= "Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/timeloc_yrorder_2070_2099")
  }
  
  dat1<- pwln.ord
  
  dat<- dat1[,-c(which(colnames(dat1)=="fips01001"))]
  length(which(colnames(dat)=="fips01003"):ncol(dat))
  
  nyrs= 30
  #compute number of observations per year
  obsperyr<- rep(NA,nyrs)
  for(i in 1:nyrs){
    ind<- which(timeloc.ord$year==yr1-1+i)
    obsperyr[i]<- length(ind)
  }
  csobs<- cumsum(obsperyr)
  
  test.yrs<- 1:30
  test.sizes<- obsperyr[test.yrs]
  #train.sizes<- obsperyr[-test.yrs]
  cs.sizes<- cumsum(test.sizes)
  #cs.trsizes<- cumsum(train.sizes)
  #test_indx<- which(timeloc.ord$year%in%(test.yrs+yr1-1))

  ind_fips01003<-which(colnames(dat)=="fips01003")

  cvInd= 1:nrow(dat)

  CVfips<- timeloc.ord$fips

  cs.obs.test<- c(0,cs.sizes)
  ntestyrs<- length(test.yrs)


  XMatCV<-cbind(rep(1, nrow(dat)), dat[,1:ncol(dat)])
  colnames(XMatCV)[1]<- "ones"
  #XMat<-as.matrix(XMat)
  XMatCV<- as.matrix(XMatCV)


  if(k==1){
    save(XMatCV, test.yrs, nyrs, cs.obs.test, obsperyr, file="Climate_CornYield-me/PICAR/holdLastYear/future/Crop_2020_2049_cfmpwSpatialData.RData") # Save Data
  }
  if(k==2){
    save(XMatCV, test.yrs, nyrs, cs.obs.test, obsperyr, file="Climate_CornYield-me/PICAR/holdLastYear/future/Crop_2070_2099_cfmpwSpatialData.RData") # Save Data
  }
  ##############################################################################
  ##############################################################################
  ##############################################################################
  rm(list=ls())
  k=2
  
  library(housingData)
  
  setwd("/storage/work/svr5482")
  
  load("/storage/work/svr5482/Climate_CornYield-master/CountyANSI")
  load("/storage/work/svr5482/Climate_CornYield-master/StateANSI")
  load("/storage/work/svr5482/Climate_CornYield-master/ANSI")
  
  load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")
  ufips<- unique(df$fips); rm(df)
  
  yrnum<- 30
  obscountynum<- length(ufips)
  countynum<- 1878
  gslen<- 185 #in days
  
  #save growing season daily max and min temps
  if(k==1) yr1= 2020
  if(k==2) yr1= 2069
  
  df2<- data.frame(StateANSI=rep(StateANSI,each=yrnum),
                   countyANSI=rep(CountyANSI,each=yrnum), 
                   fips=rep(ANSI,each=yrnum),
                   year=rep(c(1:yrnum),countynum))
  
  df<- df2[which(df2$fips%in%ufips),]
  
  
  df$year=df$year+yr1-1
  
  #find unique state ansi values
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
  
  fipscode<- matrix(0, nrow= nrow(df), ncol= length(ufips))
  fipscode<- as.data.frame(fipscode)
  
  for(i in 1:length(ufips)) colnames(fipscode)[i]<- paste("fips",ufips[i],sep="")
  for(i in 1:length(ufips)) fipscode[which(df$fips==ufips[i]),i]<- 1
  
  df<- cbind(df, fipscode)
  
  pwl_data<- df[,-c(1:(which(colnames(df)=="year")))]
  
  ind1<- which(colnames(pwl_data)=="state1yr")
  ind2<- which(colnames(pwl_data)=="state55yr")
  ind3<- which(colnames(pwl_data)=="state55yr_sq")
  
  n.styr<- pwl_data[,ind1:ind2]
  
  for(i in 1:ncol(n.styr)){
    #scale instead of standardize to avoid zeros where they shouldn't be
    #n.styr[which(n.styr[,i]>0),i]<- normalize(n.styr[which(n.styr[,i]>0),i], method = "scale")
    #divide all columns by 38 to be consistent with previous weighting
    n.styr[,i]<- n.styr[,i]/38
  }
  n.styr2<- n.styr^2
  
  pwl_norm<- pwl_data
  pwl_norm[,ind1:ind3]<- cbind(n.styr,n.styr2)
  
  
  #create_timeloc_df_future.R
  
  #save locations to be used in analysis in the same order in a more useful form
  loc_df<- as.data.frame(df$fips)
  
  loc_df$lat<- rep(NA,nrow(df))
  loc_df$lon<- rep(NA,nrow(df))
  
  ufips<- as.character(unique(df$fips))
  fips<- as.character(df$fips)
  geofips<- as.character(geoCounty$fips)
  
  for(i in 1:length(ufips)){
    val<- ufips[i]
    loc_df$lat[which(fips==val)]<- geoCounty$lat[which(geofips==val)]
    loc_df$lon[which(fips==val)]<- geoCounty$lon[which(geofips==val)]
  }
  
  colnames(loc_df)[1]<- "fips"
  
  timeloc_df<- cbind(loc_df,df$StateANSI,df$countyANSI,df$year)
  colnames(timeloc_df)[4:6]<- c("StateANSI","countyANSI","year")
  timeloc_df$StateANSI<- as.factor(timeloc_df$StateANSI)
  timeloc_df$countyANSI<- as.factor(timeloc_df$countyANSI)
  
  #format the data so it's in order of year instead of location
  
  year<- as.character(timeloc_df$year)
  year<- as.numeric(year)
  
  year<- year[order(year)]
  
  pwln.ord<- matrix(NA, ncol= ncol(pwl_norm), nrow= nrow(pwl_norm))
  pwln.ord<- as.data.frame(pwln.ord)
  colnames(pwln.ord)<- colnames(pwl_norm)
  
  for(i in 1:yrnum){
    inds<- which(timeloc_df$year==yr1-1+i)
    pwln.ord[which(year==yr1-1+i),]<- pwl_norm[inds,]
  }
  
  #format timeloc_df so it's in order of year instead of location
  
  timeloc.ord<- matrix(NA, ncol= ncol(timeloc_df), nrow= nrow(timeloc_df))
  timeloc.ord<- as.data.frame(timeloc.ord)
  colnames(timeloc.ord)<- colnames(timeloc_df)
  
  timeloc_df$fips<- as.character(timeloc_df$fips)
  timeloc_df$StateANSI<- as.character(timeloc_df$StateANSI)
  timeloc_df$countyANSI<- as.character(timeloc_df$countyANSI)
  #timeloc_df$year<- as.character(timeloc_df$year)
  
  for(i in 1:yrnum){
    inds<- which(timeloc_df$year==yr1-1+i)
    timeloc.ord[which(year==yr1-1+i),]<- timeloc_df[inds,]
  }
  
  
  if(k==1){
    save(timeloc.ord, file= "Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/timeloc_yrorder_2020_2049")
  }
  if(k==2){
    save(timeloc.ord, file= "Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/timeloc_yrorder_2070_2099")
  }
  
  dat1<- pwln.ord
  
  dat<- dat1[,-c(which(colnames(dat1)=="fips01001"))]
  length(which(colnames(dat)=="fips01003"):ncol(dat))
  
  nyrs= 30
  #compute number of observations per year
  obsperyr<- rep(NA,nyrs)
  for(i in 1:nyrs){
    ind<- which(timeloc.ord$year==yr1-1+i)
    obsperyr[i]<- length(ind)
  }
  csobs<- cumsum(obsperyr)
  
  test.yrs<- 1:30
  test.sizes<- obsperyr[test.yrs]
  #train.sizes<- obsperyr[-test.yrs]
  cs.sizes<- cumsum(test.sizes)
  #cs.trsizes<- cumsum(train.sizes)
  #test_indx<- which(timeloc.ord$year%in%(test.yrs+yr1-1))
  
  ind_fips01003<-which(colnames(dat)=="fips01003")
  
  cvInd= 1:nrow(dat)
  
  CVfips<- timeloc.ord$fips
  
  cs.obs.test<- c(0,cs.sizes)
  ntestyrs<- length(test.yrs)
  
  
  XMatCV<-cbind(rep(1, nrow(dat)), dat[,1:ncol(dat)])
  colnames(XMatCV)[1]<- "ones"
  #XMat<-as.matrix(XMat)
  XMatCV<- as.matrix(XMatCV)
  
  
  if(k==1){
    save(XMatCV, test.yrs, nyrs, cs.obs.test, obsperyr, file="Climate_CornYield-me/PICAR/holdLastYear/future/Crop_2020_2049_cfmpwSpatialData.RData") # Save Data
  }
  if(k==2){
    save(XMatCV, test.yrs, nyrs, cs.obs.test, obsperyr, file="Climate_CornYield-me/PICAR/holdLastYear/future/Crop_2070_2099_cfmpwSpatialData.RData") # Save Data
  }


rm(list=ls())
setwd("/storage/work/svr5482")

load("Climate_CornYield-me/PICAR/holdLastYear/future/Crop_2020_2049_cfmpwSpatialData.RData")

load("Climate_CornYield-me/PICAR/holdLastYear/future/Crop_2070_2099_cfmpwSpatialData.RData")
