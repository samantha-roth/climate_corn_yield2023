#compare WAICs

rm(list=ls())

load("/storage/work/svr5482/Climate_CornYield-me/yield/Bayesian/CI/output/WAIC1_CIModel.RData")
WAIC_CI<- WAIC1
load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.2/rank20/WAIC1.RData")
WAIC_BIC<- WAIC1
load("/storage/work/svr5482/Climate_CornYield-me/yield/PICAR/timeConstant/noCIs/cutoff.1/rank190/WAIC1.RData")
WAIC_AIC<- WAIC1
