#normalize the data for improved computation
rm(list = ls())
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/pwl_data")
#library(BBmisc)

#normalize precipitation
#n.pr<- normalize(pwl_data$Pr_GS, method = "standardize")
#n.pr2<- n.pr^2

#instead scale by dividing by 1000
prDiv1000<- pwl_data$Pr_GS/1000
prDiv1000_2<- prDiv1000^2

ind1<- which(colnames(pwl_data)=="state1yr")
ind2<- which(colnames(pwl_data)=="state55yr")
ind3<- which(colnames(pwl_data)=="state55yr_sq")

n.styr<- pwl_data[,ind1:ind2]

for(i in 1:ncol(n.styr)){
  #scale instead of standardize to avoid zeros where they shouldn't be
  #n.styr[which(n.styr[,i]>0),i]<- normalize(n.styr[which(n.styr[,i]>0),i], method = "scale")
  #divide all columns by 38 instead of scaling each column individually
  n.styr[,i]<- n.styr[,i]/38
}
n.styr2<- n.styr^2

ind4<- which(colnames(pwl_data)=="-29")
ind5<- which(colnames(pwl_data)=="47")
n.temp<- pwl_data[,ind4:ind5]


#tempTimes<- colSums(n.temp)
#mostCommonTempCol<- which.max(tempTimes)
#mostCommonTemp.std<- scale(n.temp[,mostCommonTempCol],center=TRUE,scale=TRUE)

#otherCols<- c(1:(mostCommonTempCol-1),(mostCommonTempCol+1):ncol(n.temp))
#for(i in otherCols){
#  n.temp[,i]<- scale(n.temp[,i], 
#                     center = attr(mostCommonTemp.std, "scaled:center"), 
#                     scale = attr(mostCommonTemp.std, "scaled:scale"))
#}

n.temp.scaled<- n.temp/(24) #change units from hours to days

#for(i in 1:ncol(n.temp)){
#  n.temp[,i]<- scale(n.temp[,i], 
#                     center = TRUE, 
#                     scale = TRUE)
#}

pwl_norm<- pwl_data
pwl_norm[,ind1:ind3]<- cbind(n.styr,n.styr2)
pwl_norm[,ind4:ind5]<- n.temp.scaled
#pwl_norm[,c("Pr_GS","Pr_GS2")]<- cbind(n.pr,n.pr2) #for when scaled
pwl_norm[,c("Pr_GS","Pr_GS2")]<- cbind(prDiv1000,prDiv1000_2) #for when divided by 1000

save(pwl_norm,file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/pwl_norm")

#save(n.temp.scaled,file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/n.temp.scaled")
