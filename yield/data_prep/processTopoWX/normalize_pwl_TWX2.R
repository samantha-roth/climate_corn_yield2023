rm(list=ls())

load("/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/temp_dist_GS2.RData")
load("/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/pr_pr2_GS2.RData")

n.temp<- temp_dist_GS
n.temp.scaled<- n.temp/(24)

prDiv1000<- pr_pr2_GS$Pr_GS/1000
prDiv1000_2<- prDiv1000^2

prpr2.scaled<- cbind(prDiv1000,prDiv1000_2)

save(n.temp.scaled,file="/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/n.temp.scaled2.RData")
save(prpr2.scaled,file="/storage/work/svr5482/Climate_CornYield-me/SourceData/TopoWX/prpr2.scaled2.RData")
