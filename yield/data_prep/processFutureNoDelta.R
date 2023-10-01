#get wsum1, wsum2, Pr_GS and Pr_GS2 for all future scenarios
#34333= 23*366 + 71*365, each observation is for a day from 2006 to 2099


rm(list = ls())
graphics.off()

library(foreach)
library(doParallel)

setwd("/storage/work/svr5482")

if(dir.exists("/storage/work/svr5482/Countywise/SourceData/MACAv2-METDATA_proj/processed_proj")==F){
  dir.create("/storage/work/svr5482/Countywise/SourceData/MACAv2-METDATA_proj/processed_proj")
}

source("Climate_CornYield-me/data_prep/changedT_distribution.R") #function to be used

ma <- function(arr, n){  #moving avg function
  res = rep(NA,length(arr)-n+1)
  for(i in n:length(arr)){
    res[i] = mean(arr[(i-n+1):i])
  }
  res
}

modelnames<-c("MIROC5","MRI-CGCM3","IPSL-CM5B-LR","IPSL-CM5A-LR", 
              "HadGEM2-ES365","GFDL-ESM2M","GFDL-ESM2G","CSIRO-Mk3-6-0","bcc-csm1-1",
              "MIROC-ESM", "IPSL-CM5A-MR", "CNRM-CM5","BNU-ESM",
              "MIROC-ESM-CHEM", "inmcm4", "HadGEM2-CC365", "CanESM2", "bcc-csm1-1-m")

#no "bcc-csm1-1"

#modelnames<-c("MIROC5","MRI-CGCM3","IPSL-CM5B-LR","IPSL-CM5A-LR", 
#              "HadGEM2-ES365","GFDL-ESM2M","GFDL-ESM2G","CSIRO-Mk3-6-0",
#              "MIROC-ESM", "IPSL-CM5A-MR", "CNRM-CM5","BNU-ESM",
#              "MIROC-ESM-CHEM", "inmcm4", "HadGEM2-CC365", "CanESM2", "bcc-csm1-1-m")


load("Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")
ufips<- unique(df$fips); rm(df)
obscountynum<- length(ufips)

#setup parallel backend to use many processors
cores=detectCores()
cl <- parallel::makeCluster(cores[1]-1) # -1 not to overload system
registerDoParallel(cl)


foreach(k = 1:length(modelnames))%dopar%{
  ##############################################################################
  #corresponds to meanclimateprojection.R
  if(dir.exists(paste("/storage/work/svr5482/Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/",modelnames[k],sep=""))==F){
    dir.create(paste("/storage/work/svr5482/Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/",modelnames[k],sep=""))
  }
  
  #future
  foldername<-paste("Countywise/SourceData/MACAv2-METDATA_proj/",modelnames[k],"_proj/",sep="")
  load(paste(foldername,"tmax",sep=""))
  load(paste(foldername,"tmin",sep=""))
  load(paste(foldername,"pr",sep=""))
  
  Tmax_proj<-tmax-273.15
  Tmin_proj<-tmin-273.15
  Pr_proj<-pr
  
  rm(tmin,tmax,pr)
  
  #beginning of 2020 to end of 2049
  Mettmax_2020_2049<- Tmax_proj[,(11*365+3*366+1):(33*365+11*366)]
  Mettmin_2020_2049<- Tmin_proj[,(11*365+3*366+1):(33*365+11*366)]
  Metpr_2020_2049<- Pr_proj[,(11*365+3*366+1):(33*365+11*366)]
  #beginnning of 2070 to end of 2099
  #Mettmax_2070_2099<- Tmax_proj[,(48*365+16*366+1):(71*365+23*366)]
  #Mettmin_2070_2099<- Tmin_proj[,(48*365+16*366+1):(71*365+23*366)]
  #Metpr_2070_2099<- Pr_proj[,(48*365+16*366+1):(71*365+23*366)]
  
  #beginnning of 2069 to end of 2098
  Mettmax_2070_2099<- Tmax_proj[,(47*365+16*366+1):(70*365+23*366)]
  Mettmin_2070_2099<- Tmin_proj[,(47*365+16*366+1):(70*365+23*366)]
  Metpr_2070_2099<- Pr_proj[,(47*365+16*366+1):(70*365+23*366)]
  
  #no NAs
  
  rm(Tmax_proj); rm(Tmin_proj); rm(Pr_proj)
  ##############################################################################
  ##############################################################################
  ##getMeanFutureGS.R portion
  
  yrnum<- 30
  countynum<- 1878
  gslen<- 185 #in days
  
  #Then calculate VPD,EDD,GDD
  for (l in 1:2){
    if (l==1){
      yr<-"2020_2049"
      yr1<- 2020
    }
    if (l==2){
      yr<-"2070_2099"
      yr1<-2069
    }
    
    print(modelnames[k])
    print(yr); print(yr1)
    
    Tmean<-(get(paste("Mettmax_",yr,sep=""))+get(paste("Mettmin_",yr,sep="")))/2
    #noNA
    Tthres<-10
    GS_start<-matrix(NA,nrow=countynum,ncol=yrnum)
    GS_end<-matrix(NA,nrow=countynum,ncol=yrnum)
    m1=1
    m2=1
    for (i in 1:countynum){
      m1=1
      m2=1
      for (j in 1:yrnum){
        ndays=365
        if ((yr1-1+j)%%4==0){ 
          ndays=366
        }
        m2<-m1+ndays-1
        Tmeantoget<-Tmean[i,c(m1:m2)]
        TMA<-ma(Tmeantoget,21)
        GS_start[i,j]<-which(TMA >= Tthres)[1]
        GS_end[i,j]<-GS_start[i,j]+184
        m1=m2+1
      }
    }

    if (l==1){
      save(GS_start,GS_end,file=paste0("/storage/work/svr5482/Countywise/Metdata/GSstarttoend_",modelnames[k],"_2020_2049"))
    }
    if (l==2){
      save(GS_start,GS_end,file=paste0("/storage/work/svr5482/Countywise/Metdata/GSstarttoend_",modelnames[k],"_2070_2099"))
    }
    
    rm(Tmean)
    ##############################################################################
    ##getMeanFutureGS_tmintmaxpr.R portion
    
    
    tmax<- get(paste("Mettmax_",yr,sep=""))
    tmin<- get(paste("Mettmin_",yr,sep=""))
    pr<- get(paste("Metpr_",yr,sep=""))
    
    print(paste0(modelnames[k]," ",yr,": Number of NAs in Tmax: ",length(which(is.na(tmax)))))
    print(paste0(modelnames[k]," ",yr,": Number of NAs in Tmin: ",length(which(is.na(tmin)))))
    print(paste0(modelnames[k]," ",yr,": Number of NAs in pr: ",length(which(is.na(pr)))))
    
    if(l==1){rm(Mettmax_2020_2049,Mettmin_2020_2049,Metpr_2020_2049)}
    if(l==2){rm(Mettmax_2070_2099,Mettmin_2070_2099,Metpr_2070_2099)}
    
    #create a vector that keeps track of the year each day is in
    yeardays<- NA
    for (j in 1:yrnum){
      if((yr1-1+j)%%4==0) yeardays<- c(yeardays, rep((yr1-1+j),366))
      if((yr1-1+j)%%4!=0) yeardays<- c(yeardays, rep((yr1-1+j),365))
    }
    yeardays<- yeardays[-1]
    
    #create a dataframe to hold the growing season daily maximum temperatures
    tmax_gs_temps<- matrix(NA, nrow= countynum, ncol= gslen*yrnum)
    tmin_gs_temps<- matrix(NA, nrow= countynum, ncol= gslen*yrnum)
    
    #create a dataframe for the daily max and daily min temps during the growing season
    #in each year in each county
    for(i in 1:countynum){
      for(j in 1:yrnum){
        year<- which(yeardays==yr1-1+j)
        tmx<- tmax[i,year]
        tmn<- tmin[i,year]
        tmx_GS<- tmx[c(GS_start[i,j]:GS_end[i,j])]
        tmn_GS<- tmn[c(GS_start[i,j]:GS_end[i,j])]
        tmax_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]<- tmx_GS
        tmin_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]<- tmn_GS
      }
    }
    
    print(paste0(modelnames[k]," ",yr,": Number of NAs in GS for Tmax: ",length(which(is.na(tmax_gs_temps)))))
    print(paste0(modelnames[k]," ",yr,": Number of NAs in GS for Tmin: ",length(which(is.na(tmin_gs_temps)))))
    
    #Find the max max temp and min min temp for all the growing seasons, set interval
    low<- floor(min(tmin_gs_temps))
    high<- ceiling(max(tmax_gs_temps))
    T_interval=c(low, high)
    
    temp_dist_GS<- matrix(NA, nrow= countynum*yrnum, ncol= (high-low))
    for(i in 1:countynum){ #takes 1.78 hours
      pt<- Sys.time()
      for(j in 1:yrnum){
        Tmax<- tmax_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]
        Tmin<- tmin_gs_temps[i,(1+(j-1)*gslen):(gslen+(j-1)*gslen)]
        #temp_dist[i,(1+(j-1)*77):(77+(j-1)*77)]<- T_distribution(Tmax= Tmax, Tmin= Tmin, T_interval= T_interval)
        td<- T_distribution(Tmax= Tmax, Tmin= Tmin, T_interval= T_interval)
        #temp_dist_GS<- rbind(temp_dist_GS, td)
        temp_dist_GS[(i-1)*yrnum+j,]<- td
      }
      pt2<- Sys.time()
      pt2-pt
    }
    
    print(paste0(modelnames[k]," ",yr,": Number of NAs in Temp Dist GS: ",length(which(is.na(temp_dist_GS)))))
    
    #get the total precipitation for each county for each year during the growing season
    totpr_GS<- rep(NA, countynum*yrnum)
    for(i in 1:countynum){
      for(j in 1:yrnum){
        year<- which(yeardays==yr1-1+j)
        prec<- pr[i,year]
        totpr_GS[(i-1)*yrnum+j]<- sum(prec[c(GS_start[i,j]:GS_end[i,j])])
      }
    }
    
    print(paste0(modelnames[k]," ",yr,": Number of NAs in Pr GS: ",length(which(is.na(totpr_GS)))))
    
    rm(tmax_gs_temps,tmin_gs_temps); rm(tmin,tmax,pr)
    rm(GS_start, GS_end)
    ############################################################################
    #create_df_future portion
    
    #load("Climate_CornYield-master/CountyANSI")
    #load("Climate_CornYield-master/StateANSI")
    load("Climate_CornYield-master/ANSI")
    
    fips=rep(ANSI,each=yrnum)
    
    totpr_GS2<- totpr_GS^2
    
    #df<- cbind(data.frame(year=rep(c(1:yrnum),countynum)),totpr_GS, totpr_GS2, temp_dist_GS)
    
    df<- cbind(data.frame("Pr_GS"=totpr_GS,"Pr_GS2"=totpr_GS2),temp_dist_GS)
    
    df<- df[which(fips%in%ufips),]
    
    print(paste0(modelnames[k]," ",yr,": Number of NAs in df: ",length(which(is.na(df)))))
    
    #df$year=df$year+yr1-1
    
    #PrGSInd<- which(colnames(df)=="totpr_GS")
    #PrGS2Ind<- which(colnames(df)=="totpr_GS2")
    #colnames(df)[PrGSInd:PrGS2Ind]<- c("Pr_GS", "Pr_GS2")
    
    tempnames<- seq(low,high-1, by=1)
    
    tempInd1<- which(colnames(df)=="Pr_GS2")+1
    colnames(df)[tempInd1:ncol(df)]<- as.character(tempnames)
    
    pwl_data<- df
    
    rm(df); rm(temp_dist_GS)
    
    #scale by dividing by 1000
    prDiv1000<- pwl_data$Pr_GS/1000
    prDiv1000_2<- prDiv1000^2
    
    ind4<- which(colnames(pwl_data)==as.character(paste0(low)))
    ind5<- which(colnames(pwl_data)==as.character(paste0(high-1)))
    n.temp<- pwl_data[,ind4:ind5]
    
    n.temp.scaled<- n.temp/24 #change units from hours to days
    
    pwl_norm<- pwl_data
    pwl_norm[,ind4:ind5]<- n.temp.scaled
    #pwl_norm[,c("Pr_GS","Pr_GS2")]<- cbind(prDiv1000,prDiv1000_2) #for when divided by 1000
    pwl_norm$Pr_GS<- prDiv1000
    pwl_norm$Pr_GS2<- prDiv1000^2
    
    rm(pwl_data,n.temp,n.temp.scaled)
    
    year<- rep(yr1:(yr1+29),each=length(ufips))
    year_unorder<- rep(yr1:(yr1+29),length(ufips))
    
    pwln.ord<- matrix(NA, ncol= ncol(pwl_norm), nrow= nrow(pwl_norm))
    pwln.ord<- as.data.frame(pwln.ord)
    colnames(pwln.ord)<- colnames(pwl_norm)
    
    for(i in 1:yrnum){
      inds<- which(year_unorder==yr1-1+i)
      pwln.ord[which(year==yr1-1+i),]<- pwl_norm[inds,]
    }
    
    print(paste0(modelnames[k]," ",yr,": Number of NAs in pwln.ord: ",length(which(is.na(pwln.ord)))))
    
    rm(pwl_norm)
    
    tempscale= 1000
    
    deg1<- seq(from= 10.5, to= (high-.5), by= 1)
    deg2<- seq(from= 0.5, to= (high-.5-29), by= 1)
    ind1<- which(colnames(pwln.ord)=="10")
    ind2<- which(colnames(pwln.ord)=="28")
    ind3<- which(colnames(pwln.ord)==paste0(high-1))
    ifyouaintfirst<- as.matrix(pwln.ord[,ind1:ind3])
    yourelast<- as.matrix(pwln.ord[,(ind2+1):ind3])
    wsum1<- ifyouaintfirst%*%(deg1/tempscale)
    wsum2<- yourelast%*%(deg2/tempscale)
    
    print(paste0(modelnames[k]," ",yr,": Number of NAs in wsum1: ",length(which(is.na(wsum1)))))
    print(paste0(modelnames[k]," ",yr,": Number of NAs in wsum2: ",length(which(is.na(wsum2)))))
    
    dat1<- cbind(pwln.ord[,c("Pr_GS","Pr_GS2")],wsum1,wsum2)
    
    rm(pwln.ord,ifyouaintfirst,yourelast)
    
    if(dir.exists(paste("Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/",modelnames[k],"/",yr,sep=""))==F){
      dir.create(paste("Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/",modelnames[k],"/",yr,sep=""))
    }
    
    save(dat1,file=paste("Countywise/SourceData/MACAv2-METDATA_proj/processed_proj/",modelnames[k],"/",yr,"/tpr.RData",sep=""))
  }
  
  
}

stopCluster(cl)
