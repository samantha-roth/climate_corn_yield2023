#generateSamples with Schlenker and Roberts mean function
#and block diagonal covariance structure
rm(list=ls())

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear")}

library(fields) ; library(mvtnorm) ; #library(classInt)
#source(file = "C:/Climate_CornYield-me/PICAR/source/sharedFunctions.R")
source(file="/storage/work/svr5482/Climate_CornYield-me/PICAR/source/sharedFunctions.R")

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/pwl_norm_yrorder")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")

#load("C:/Users/saman/Dropbox/Climate_CornYield-me/SourceData/METDATA/pwl_norm_yrorder")
#load("C:/Users/saman/Dropbox/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
#dist_allyrs is a block diagonal matrix with each block containing the distances
#between counties for each year
#load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/dist_allyrs")

dat1<- pwln.ord
#fipsGetRid<- which(colnames(dat1)=="fips01001")
#length(fipsGetRid:ncol(dat1))

#logyield<- log(dat$yield)
#dat<- cbind(logyield,dat[,-1])

#just commented
#dat<- dat[,-fipsGetRid]

#eliminate one county to avoid multicollinearity
#dat<- dat[,-which(colnames(dat)=="-29")] 
#all growing seasons have same length
#so the amount of time across all categories in any growing season= 185days*24hrs
#this doesn't matter since we don't have to estimate a coefficient for each bin!!

#need to first format the data so that we have the proper model
#create a vector of half-degree values
#deg1<- seq(from= 10.5, to= 47.5, by= 1)
#deg2<- seq(from= .5, to= 18.5, by= 1)

tempscale1= 1000
tempscale2= 1000

deg1<- seq(from= 10.5, to= (high-.5), by= 1)
deg2<- seq(from= 0.5, to= (high-.5-29), by= 1)
#deg1<- seq(from= 10.5, to= 47.5, by= 1)
#deg2<- seq(from= 0.5, to= 18.5, by= 1)
ind1<- which(colnames(dat1)=="10")
ind2<- which(colnames(dat1)=="28")
ind3<- which(colnames(dat1)==paste(high-1,sep=""))

ifyouaintfirst<- as.matrix(dat1[,ind1:ind3])
yourelast<- as.matrix(dat1[,(ind2+1):ind3])

wsum1<- ifyouaintfirst%*%(deg1/tempscale1)
wsum2<- yourelast%*%(deg2/tempscale2)

ind4<- which(colnames(dat1)=="state55yr_sq")
ind5<- which(colnames(dat1)=="state1yr_sq")

#maxzeros<- rep(NA, ind4-ind5+1)
#for(i in ind5:ind4){
#  maxzeros[i-(ind5-1)]<- length(which(dat1[,i]==0))
#}

#maxmaxzeros<- which(maxzeros==max(maxzeros))
#minobsstate.ind<- maxmaxzeros+(ind5-1)
#colnames(dat1)[minobsstate.ind]
#get rid of the quadratic time trend for delaware since it has the least observations
#getrid<- c(which(colnames(dat)=="state10yr"),which(colnames(dat)=="state10yr_sq")) #old
#getrid<- c(fipsGetRid, which(colnames(dat1)=="state10yr"),which(colnames(dat1)=="state10yr_sq")) #new

otherInds<- (ind3+1):ncol(dat1)

#otherIndsKeep<- otherInds[-getrid]
#otherInds[-getrid]

#dat<- cbind(dat[,1:3],wsum1,wsum2,dat[,(ind3+1):ncol(dat)])
#dat<- cbind(dat[,c("yield","Pr_GS","Pr_GS2")],wsum1,wsum2,dat[,(ind3+1):(getrid[1]-1)],
#            dat[,(getrid[1]+1):(getrid[2]-1)],dat[,(getrid[2]+1):ncol(dat)])

dat1to3wsum1<- cbind(dat1[,1:3],wsum1)
dat1to3wsums<- cbind(dat1to3wsum1,wsum2)
dat2<- cbind(dat1to3wsums,dat1[,otherInds])

#dat3<- dat2[,-c(which(colnames(dat2)=="state10yr"),which(colnames(dat2)=="state10yr_sq"),which(colnames(dat2)=="fips01001"))]
dat3<- dat2[,-c(which(colnames(dat2)=="fips01001"))]
length(which(colnames(dat3)=="fips01003"):ncol(dat3))
dat<- dat3

rm(dat1); rm(dat2); rm(dat3); rm(dat1to3wsum1); rm(dat1to3wsums)
#dat<- cbind(dat1[,c("yield","Pr_GS","Pr_GS2")],wsum1,wsum2,dat1[,otherIndsKeep])

#for when I'm not including indicator variable for each county
#dat<- cbind(dat[,1:3],wsum1,wsum2,dat[,(ind3+1):ncol(dat)])
#dat<- cbind(dat[,1:3],wsum1,wsum2,dat[,(ind3+1):(getrid[1]-1)],
#            dat[,(getrid[1]+1):(getrid[2]-1)],dat[,(getrid[2]+1):ind4])

#timeloc.ord$year<- as.numeric(timeloc.ord$year)

#compute number of observations per year
obsperyr<- rep(NA,nyrs)

for(i in 1:nyrs){
  ind<- which(timeloc.ord$year==1980+i)
  obsperyr[i]<- length(ind)
  #latlon<- timeloc.ord[ind,c("lat","lon")]
  #d<- dist(latlon, method = "euclidean")
}
csobs<- cumsum(obsperyr)

#set state with State ANSI code 55 to be default to avoid multicollinearity
#so we only go to column 41, not 42
rm(pwln.ord)
#rm(ifyouaintfirst);rm(yourelast);rm(wsum1);rm(wsum2)
# create training set, test set

#holding out 10% of the data from each year ensures we're not
#training our model based on some years more than others

################################################################################
################METHOD WHERE 10% OF DATA EACH YEAR IS HELD OUT##################
################################################################################

#test.sizes<- round(obsperyr/10)
#train.sizes<- obsperyr-test.sizes
#cs.sizes<- cumsum(test.sizes)
#cs.trsizes<- cumsum(train.sizes)
#test_indx<- rep(NA, cs.sizes[nyrs])

#for(i in 1:nyrs){
#  if(i==1){
#    cvind<- c(1:csobs[1])
#    set.seed(i+1050)
#    test_indx[1:cs.sizes[1]]<- sample(cvind, test.sizes[i], replace = FALSE, prob = NULL)
#  } 
#  if(i>1){
#    cvind<-c( (csobs[i-1]+1):csobs[i])
#    set.seed(i+1050)
#    test_indx[(cs.sizes[i-1]+1):cs.sizes[i]]<- sample(cvind, test.sizes[i], replace = FALSE, prob = NULL)
#  } 
#}

################################################################################
################ALTERNATE METHOD WHERE 3 YEARS ARE HELD OUT#####################
#######################INSTEAD OF 10% OF EACH YEAR##############################
################################################################################

#set.seed(85)
#test.yrs<- sample(1:nyrs,size=3,replace=FALSE)
#test.sizes<- obsperyr[test.yrs]
#train.sizes<- obsperyr[-test.yrs]
#cs.sizes<- cumsum(test.sizes)
#cs.trsizes<- cumsum(train.sizes)

#test_indx<- 0
#for(i in 1:length(test.yrs)){
#  test_indx<- c(test_indx, which(timeloc.ord$year==(test.yrs[i]+1980)))
#}
#test_indx<- test_indx[-1]

################################################################################

################################################################################
################ALTERNATE METHOD WHERE LAST YEAR HELD OUT#######################
#######################INSTEAD OF 10% OF EACH YEAR##############################
################################################################################

test.yrs<- (nyrs-3):nyrs
test.sizes<- obsperyr[test.yrs]
train.sizes<- obsperyr[-test.yrs]
cs.sizes<- cumsum(test.sizes)
cs.trsizes<- cumsum(train.sizes)
test_indx<- which(timeloc.ord$year%in%(test.yrs+1980))

################################################################################

test_data<- dat[test_indx,]
train_data<- dat[-test_indx,]

#check to make sure no columns of all zeros were accidentally created in the training data
#source(file= "/storage/work/svr5482/Climate_CornYield-me/PICAR/MforAllYrs/source/MLE_FindRank_Linear.R")
ind_fips01003<-which(colnames(dat)=="fips01003")
#zerocols<-RemoveZeroCols(train_data)
#zerocols #good
n= nrow(train_data); modInd= 1:n; cvInd= (n+1):nrow(dat)

# set the grid locations for training, testing data. Combine
#timeloc.ord<- timeloc.ord[1:csobs[nyrs],]


#don't need to save these
#gridLocation<- cbind(timeloc.ord$lon[-test_indx], timeloc.ord$lat[-test_indx])
#CVgridLocation<- cbind(timeloc.ord$lon[test_indx], timeloc.ord$lat[test_indx])
#comboLocation<-rbind(gridLocation,CVgridLocation)

#fips<- timeloc.ord$fips[-test_indx]
#CVfips<- timeloc.ord$fips[test_indx]
#combofips<- c(fips,CVfips)
#save(comboLocation,file="C:/Users/saman/Dropbox/Climate_CornYield-me/PICAR/pw_multyrs-comboLocation.RData") #sampling parts of years
#save(comboLocation,file="C:/Users/saman/Dropbox/Climate_CornYield-me/PICAR/test_fullyrs/pw_multyrs-comboLocation.RData") #sampling parts of years

################################################################################
######################METHOD WHERE 10% OF EACH YEAR HELD OUT####################
################################################################################

#calculate the distances between the locations within each year
#distMatModList<- list()
#distMatCVList<- list()
##library(rdist)
#for(i in 1:nyrs){
#  if(i==1){
#    distMatModList[[i]]<- as.matrix(rdist(gridLocation[1:cs.trsizes[i],]))
#    distMatCVList[[i]]<- as.matrix(rdist(CVgridLocation[1:cs.sizes[i],]))
#  }
#  if(i>1){
#    distMatModList[[i]]<- as.matrix(rdist(gridLocation[(cs.trsizes[i-1]+1):cs.trsizes[i],]))
#    distMatCVList[[i]]<- as.matrix(rdist(CVgridLocation[(cs.sizes[i-1]+1):cs.sizes[i],]))
#  }
#}

################################################################################
################ALTERNATE METHOD WHERE 3 YEARs ARE HELD OUT#####################
#######################INSTEAD OF 10% OF EACH YEAR##############################
################################################################################
##calculate the distances between the locations within each year
#distMatModList<- list()
#distMatCVList<- list()

#for(i in 1:length(test.yrs)){
#  if(i==1){
#    distMatCVList[[i]]<- as.matrix(rdist(CVgridLocation[1:cs.sizes[i],]))
#  }
#  if(i>1){
#    distMatCVList[[i]]<- as.matrix(rdist(CVgridLocation[(cs.sizes[i-1]+1):cs.sizes[i],]))
#  }
#}
#for(i in 1:(32-length(test.yrs))){
#  if(i==1){
#    distMatModList[[i]]<- as.matrix(rdist(gridLocation[1:cs.trsizes[i],]))
#  }
#  if(i>1){
#    distMatModList[[i]]<- as.matrix(rdist(gridLocation[(cs.trsizes[i-1]+1):cs.trsizes[i],]))
#  }
#}

################################################################################


################################################################################
##############ALTERNATE METHOD WHERE LAST FEW YEARS HELD OUT####################
#######################INSTEAD OF 10% OF EACH YEAR##############################
################################################################################

cs.obs.train<- c(0,cs.trsizes)
cs.obs.test<- c(0,cs.sizes)

ntestyrs<- length(test.yrs)
ntrainyrs<- nyrs-ntestyrs

##calculate the distances between the locations within each year
#distMatModList<- list()
#distMatCVList<- list()

#for(i in 1:ntestyrs){
#  distMatCVList[[i]]<- as.matrix(fields::rdist(CVgridLocation[(cs.obs.test[i]+1):cs.obs.test[i+1],]))
#}

#for(i in 1:ntrainyrs){
#  distMatModList[[i]]<- as.matrix(fields::rdist(gridLocation[(cs.obs.train[i]+1):cs.obs.train[i+1],]))
#}

################################################################################

# Observations - using log yield instead of yield now!!
obsFullLinear<- c(train_data$yield, test_data$yield)
# Observations using logyield instead of yield
#obsFullLinear<- c(train_data$logyield, test_data$logyield)
#obsFullLinear<- log(spdata2$yield)
# Observations that we will use to create the model
obsModLinear<-obsFullLinear[modInd] 
obsCVLinear<- obsFullLinear[cvInd]

# Covariates
XMat<-cbind(rep(1, nrow(train_data)), train_data[,2:ncol(train_data)])
colnames(XMat)[1]<- "ones"
XMatCV<-cbind(rep(1, nrow(test_data)), test_data[,2:ncol(test_data)])
colnames(XMatCV)[1]<- "ones"

XMat<-as.matrix(XMat)
XMatCV<- as.matrix(XMatCV)

#for(i in 1:which(colnames(XMat==)))

rm(yourelast); rm(ifyouaintfirst); rm(wsum1); rm(wsum2)
rm(train_data,test_data); rm(timeloc.ord); rm(dat)
#rm(fipsGetRid); rm(fipsGetRid2); rm(otherInds)
#use this is just holding out the last year
save.image(file="/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData") # Save Data

colmaxes<- rep(NA,ncol(XMat))
for(i in 1:ncol(XMat)){
  colmaxes[i]<- max(XMat[,i])
}

summary(colmaxes)

colmins<- rep(NA,ncol(XMat))
for(i in 1:ncol(XMat)){
  colmins[i]<- min(XMat[,i])
}

summary(colmins)
