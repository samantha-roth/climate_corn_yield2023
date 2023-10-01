#set up dataframe for piecewise linear model
rm(list = ls())
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/df_tempdist_totpr")

#keep only relevant columns of dataframe
yieldCol<- which(colnames(df)=="yield")
PrGSCol<- which(colnames(df)=="Pr_GS")
pwl_data<- df[,c(yieldCol,PrGSCol:ncol(df))]

#So the maximum is 48C and the minimum is -29C
#need to relabel the variables that keep track of
#amount of time spent in each 1C interval

#3 is the index of the first temperature "bucket"
#there are 77 buckets
#colnames(pwl_data)[4:80]<- seq(from= -29,to=47,by=1)
#buckets are named using their lower bound
#save the new dataset

save(pwl_data,file="/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/pwl_data")

#load("/storage/work/svr5482/Climate_CornYield-me/SourceData/METDATA/timeloc_yrorder")
