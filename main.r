setwd(".")
source("FLD.r")
library(MASS)
dataset=read.table("wine.data",sep=",")
x<-dataset[,-1]
y<-dataset[,1]
new_dim<-3
new_x<-fld(x,y,new_dim)

