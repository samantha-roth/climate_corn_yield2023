#read the raw Metdata dataframe

rm(list = ls())
graphics.off()
library(maps)
library(maptools)
library(ncdf4)
library(usmap)
source("/storage/work/svr5482/Climate_CornYield-me/data_prep/processTopoWX/functions/latlong2county.R")

file<- nc_open("/gpfs/group/kzk10/default/private/jwo118/topowx-2016/final_output_data/daily/tmax/tmax_1981.nc")
grid_lon<-as.vector(file$dim$lon$vals)
grid_lat<-as.vector(file$dim$lat$vals)
dim_lon<-length(grid_lon)
dim_lat<-length(grid_lat)
grid<-data.frame(x = rep(grid_lon,each=dim_lat), y = rep(grid_lat,dim_lon))
countyname<-latlong2county(grid)
countyname<-matrix(countyname,nrow=dim_lat,ncol=dim_lon) #order is fine bc matches grid


################################################################################
#check how locations are ordered in each dataset

#check if grid_lon is in ascending order
#all(diff(grid_lon)>=0) #TRUE

#check if grid_lat is in ascending order
#all(diff(grid_lat)>=0) #FALSE

#check if grid_lat is in descending order
#all(diff(grid_lat)<0) #TRUE

#ONLY grid_lat is not in ascending order
################################################################################

#compare the two results:
#min(grid$x); max(grid$x)

#min(grid$y); max(grid$y)

grid_lon_inds<- intersect(which(grid_lon > (-101)), which(grid_lon<(-70)))
grid_lat_inds<- intersect(which(grid_lat > 28), which(grid_lat< 49.5))

#grid_lon_inds<- which(grid_lon > (-101))

#grid_lon[min(grid_lon_inds)]; grid_lon[max(grid_lon_inds)]

#grid_lat[min(grid_lat_inds)]; grid_lat[max(grid_lat_inds)]

################################################################################
#INPUT 1: which counties' (grids') data are needed?
countys <- map('county', fill=TRUE, col="transparent", plot=FALSE)
IDs <- sapply(strsplit(countys$names, ":"), function(x) x[1])
countys_sp <- map2SpatialPolygons(countys, IDs=IDs,proj4string=CRS("+proj=longlat +datum=WGS84"))
countyNames <- sapply(countys_sp@polygons, function(x) x@ID)
countytoget<-countyNames[c(1:67,83:157,288:290,359:517,562:854,960:1143,1160:1183,1198:1564,1742:1762,1796:1957,2011:2098,2212:2278,2284:2329,2396:2490,2788:2887,2927:3053)]#PA,NY,NJ,MD,DE,DC,NC,VA,SC,WV,OH,MI,GA,KY,IN,IL,AL,TN,WI,MS,MN,MO,LA,AR,IA
countynum<-length(countytoget)

################################################################################

m1<-1 #used to record which columns to write in each loop
m2<-1

if(dir.exists("/storage/work/svr5482/Countywise/TopoWX")==F){
  dir.create("/storage/work/svr5482/Countywise/TopoWX")}


st1 <- Sys.time()

i=1
#for (i in 1:40){
  
  #read data from 1979 to 2018
  Tmax_file<-paste("/gpfs/group/kzk10/default/private/jwo118/topowx-2016/final_output_data/daily/tmax/tmax_",i+1980,".nc",sep="")
  mettmax<-nc_open(Tmax_file)
  
  Tmin_file<-paste("/gpfs/group/kzk10/default/private/jwo118/topowx-2016/final_output_data/daily/tmin/tmin_",i+1980,".nc",sep="")
  mettmin<-nc_open(Tmin_file)
  
  k=365
  if (i%%4==0) k=366 #when exist Feb29
  
  #27 normal years and 9 leap years
  tmax<-matrix(NA,nrow=countynum,ncol=k)
  tmin<-matrix(NA,nrow=countynum,ncol=k)
  
  m2<-m1+k-1
  
  #determine which grids to get based on which counties we need, and calculate county average
  #Tmax<-ncvar_get(mettmax,varid = "tmax",
  #                start = c(min(grid_lon_inds),min(grid_lat_inds),1),
  #                count = c(length(grid_lon_inds),length(grid_lat_inds),k)) 
  
  #Tmin<-ncvar_get(mettmin,varid = "tmin",
  #                start = c(min(grid_lon_inds),min(grid_lat_inds),1),
  #                count = c(length(grid_lon_inds),length(grid_lat_inds),k)) 
  
  Tmax<-ncvar_get(mettmax,varid = "tmax",
                  start = c(min(grid_lon_inds),min(grid_lat_inds),1),
                  count = c(length(grid_lon_inds),length(grid_lat_inds),k)) 
  
  Tmin<-ncvar_get(mettmin,varid = "tmin",
                  start = c(min(grid_lon_inds),min(grid_lat_inds),1),
                  count = c(length(grid_lon_inds),length(grid_lat_inds),k)) 
  
  for (n in 1:countynum){
    gridstoget<-which(countyname==countytoget[n],arr.ind = T)
    gridnum<-dim(gridstoget)[1]
    Tmaxgrid<-matrix(NA,nrow=gridnum,ncol=k)
    Tmingrid<-matrix(NA,nrow=gridnum,ncol=k)
    
    for (j in 1:gridnum){
      #Tmaxgrid is the daily data of Tmax in given county given year
      #Tmaxgrid[j, ]<-Tmax[gridstoget[j,2]-(min(grid_lon_inds)-1),
      #                    gridstoget[j,1]-(min(grid_lat_inds)-1), ] 
      #Tmingrid[j, ]<-Tmin[gridstoget[j,2]-(min(grid_lon_inds)-1),
      #                    gridstoget[j,1]-(min(grid_lat_inds)-1), ] 
      
      Tmaxgrid[j, ]<-Tmax[gridstoget[j,2]-(min(grid_lon_inds)-1),
                          gridstoget[j,1]-(min(grid_lat_inds)-1), ] 
      Tmingrid[j, ]<-Tmin[gridstoget[j,2]-(min(grid_lon_inds)-1),
                          gridstoget[j,1]-(min(grid_lat_inds)-1), ] 
      
      #dimension=(lon,lat)
      #got rid of -273.15 above bc already celsius
    }
    
    Tmaxgrid<-Tmaxgrid[complete.cases(Tmaxgrid), ]
    Tmingrid<-Tmingrid[complete.cases(Tmingrid), ]
    
    tmax[n,]<-colMeans(Tmaxgrid)
    tmin[n,]<-colMeans(Tmingrid)
    
    print(n)
  }
  m1<-m2+1
  print(i)
  
#}
en1 <- Sys.time()
timeEnd= en1-st1

save(timeEnd,file=paste0("/storage/work/svr5482/Countywise/TopoWX/time",i+1980,".RData"))
save(tmax,file=paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmax",i+1980,".RData"))
save(tmin,file=paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmin",i+1980,".RData"))


#load(paste0("/storage/work/svr5482/Countywise/TopoWX/time",i+1980,".RData"))
#load(paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmin",i+1980,".RData"))
#load(paste0("/storage/work/svr5482/Countywise/TopoWX/Topotmax",i+1980,".RData"))

#NAtmins<- which(is.na(as.matrix(tmin[,1:365])),arr.ind=TRUE)

#length(unique(NAtmins[,1]))

#badloctmin<- unique(NAtmins[,1])

#NAtmaxs<- which(is.na(as.matrix(tmax[,1:365])),arr.ind=TRUE)

#length(unique(NAtmaxs[,1]))
