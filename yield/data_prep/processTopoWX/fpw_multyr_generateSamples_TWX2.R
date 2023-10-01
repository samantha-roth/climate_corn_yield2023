#fpw_multyr_generateSamples.R but for TopoWX data

#generateSamples with Schlenker and Roberts mean function
#and block diagonal covariance structure
rm(list=ls())

if(dir.exists("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear")==F){
  dir.create("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear")}

library(fields) ; library(mvtnorm) #library(classInt)
#source(file = "C:/Climate_CornYield-me/PICAR/source/sharedFunctions.R")
source(file="/storage/work/svr5482/Climate_CornYield-me/PICAR/source/sharedFunctions.R")

load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwSpatialData.RData")

obsCVLinear<- obsCVLinear[1:cs.sizes[2]]
obsperyr<- obsperyr[1:36]
obsFullLinear<- c(obsModLinear,obsCVLinear)

XMatCV<- XMatCV[1:cs.sizes[2],]

rm(list=c("cs.obs.test", "cs.obs.train", "cs.sizes", "cs.trsizes", "csobs","cvInd",
          "deg1","deg2","high", "i","low","modInd", "n", "ntestyrs", "ntrainyrs", "nyrs",
          "test_indx", "test.sizes", "test.yrs", "train.sizes"))

nyrs=36

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/pwl_norm_yrorder2.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/prpr2_norm_yrorder2.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
timeloc.ord<- timeloc.ord[which(timeloc.ord$year%in%seq(1981,2016,by=1)),]

lowhigh<- as.numeric(colnames(pwln.ord)[c(1,ncol(pwln.ord))])

low<- lowhigh[1]; high<- lowhigh[2]


tempscale= 1000

deg1<- seq(from= 10.5, to= (high-.5), by= 1)
deg2<- seq(from= 0.5, to= (high-.5-29), by= 1)
#deg1<- seq(from= 10.5, to= 47.5, by= 1)
#deg2<- seq(from= 0.5, to= 18.5, by= 1)
ind1<- which(colnames(pwln.ord)=="10")
ind2<- which(colnames(pwln.ord)=="28")
ind3<- which(colnames(pwln.ord)==paste(high-1,sep=""))

ifyouaintfirst<- as.matrix(pwln.ord[,ind1:ind3])
yourelast<- as.matrix(pwln.ord[,(ind2+1):ind3])

wsum1<- ifyouaintfirst%*%(deg1/tempscale)
wsum2<- yourelast%*%(deg2/tempscale)

wsums<- cbind(wsum1,wsum2)
tpr<- cbind(prpr2.ord,wsums)

#compute number of observations per year
obsperyr<- rep(NA,nyrs)

for(i in 1:nyrs){
  ind<- which(timeloc.ord$year==1980+i)
  obsperyr[i]<- length(ind)
  #latlon<- timeloc.ord[ind,c("lat","lon")]
  #d<- dist(latlon, method = "euclidean")
}
csobs<- cumsum(obsperyr)

################################################################################
################ALTERNATE METHOD WHERE LAST YEAR HELD OUT#######################
#######################INSTEAD OF 10% OF EACH YEAR##############################
################################################################################

#but keep the years trained on the same as for METDATA, 
#so testing years will only be 2015-2016

test.yrs<- (nyrs-1):nyrs
test.sizes<- obsperyr[test.yrs]
train.sizes<- obsperyr[-test.yrs]
cs.sizes<- cumsum(test.sizes)
cs.trsizes<- cumsum(train.sizes)
test_indx<- which(timeloc.ord$year%in%(test.yrs+1980))

################################################################################

test_tpr<- tpr[test_indx,]
train_tpr<- tpr[-test_indx,]

n= nrow(train_tpr); modInd= 1:n; cvInd= (n+1):nrow(tpr)

################################################################################
##############ALTERNATE METHOD WHERE LAST FEW YEARS HELD OUT####################
#######################INSTEAD OF 10% OF EACH YEAR##############################
################################################################################

cs.obs.train<- c(0,cs.trsizes)
cs.obs.test<- c(0,cs.sizes)

ntestyrs<- length(test.yrs)
ntrainyrs<- nyrs-ntestyrs

################################################################################

XMat[,2:5]<- train_tpr
XMatCV[,2:5]<- test_tpr

rm(list=c("ifyouaintfirst","yourelast","wsum1","wsum2","tpr","train_tpr","test_tpr",
          "pwln.ord","prpr2.ord"))

save.image(file="/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/holdLastYear/Crop_allyrs_cfmpwTWX.RData")
