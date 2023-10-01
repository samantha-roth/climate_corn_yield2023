#get heat function 

rm(list=ls())

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/pwl_norm_yrorder2.RData")

lowhigh<- as.numeric(colnames(pwln.ord)[c(1,ncol(pwln.ord))])

low<- lowhigh[1]; high<- lowhigh[2]

dat1<- pwln.ord
tempscale= 1000

deg1<- seq(from= 10.5, to= (high-.5), by= 1)
deg2<- seq(from= 0.5, to= (high-.5-29), by= 1)
ind1<- which(colnames(dat1)=="10")
ind2<- which(colnames(dat1)=="28")
ind3<- which(colnames(dat1)==as.character(paste(high-1,sep="")))
ifyouaintfirst<- as.matrix(dat1[,ind1:ind3])
yourelast<- as.matrix(dat1[,(ind2+1):ind3])
wsum1<- ifyouaintfirst%*%(deg1/tempscale)
wsum2<- yourelast%*%(deg2/tempscale)

length(which(is.na(wsum1)))
length(which(is.na(wsum2)))

save(wsum1,wsum2,file="/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/wt.tsums2.RData")
