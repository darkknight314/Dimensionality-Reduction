setwd(".")
library(MASS)
dataset=read.table("wine.data",sep=",")
global_mean<-colMeans(dataset)[-1]
#split into classes
dataset<-split(dataset,dataset$V1)
num_of_classes<-length(dataset)
org_dim<-dim(data.frame(dataset[1]))[2]-1
new_dim<-3
Sw<-matrix(0,org_dim,org_dim)
Sb<-matrix(0,org_dim,org_dim)
for (i in 1:num_of_classes){
  #CALCULATION OF WITHIN CLASS COVARIANCE
  class_data<-data.frame(dataset[i])
  #Select x attributes out of dataset
  class_data<-class_data[-1]
  #Setting mean to 0
  class_mean<-colMeans(class_data)
  std_data<-sweep(class_data,2,class_mean)
  #Variance within classes
  Sk<-t(std_data)%*%as.matrix(std_data)
  #Sum of within class variance
  Sw<-Sw+Sk
  #CALCULATION OF BETWEEN CLASS COVARIANCE
  central_mean<-(class_mean-global_mean)%o%(class_mean-global_mean)
  central_mean<-nrow(class_data)*central_mean
  Sb<-Sb+central_mean
}
solve(Sw)
comp<-ginv(Sw)%*%Sb
evals<-eigen(comp)$values
evecs<-eigen(comp)$vectors
r_evals<-vector("numeric")
evecs<-Re(evecs)
first<-1
#Filtering out the real eigen values and vectors
for(i in 1:length(evals)){
  if(Im(evals[i])==0){
    if(first==1){
      r_evals<-Re(evals[i])
      r_evecs<-evecs[,i]
      first<-0
      print(r_evecs)
    }
    else{
      r_evals<-append(r_evals,Re(evals[i]))
      r_evecs<-rbind(r_evecs,evecs[,i])
      print(r_evecs)
    }
  }
}
#Sorting and selction of top eigen values
r_evecs<-t(r_evecs)
sorted_eval<-order(-r_evals)
#W is the final transformation matrix new_dim x org_dim
w<-r_evecs[,sorted_eval[1]]
for(i in 2:new_dim){
  w<-rbind(w,r_evecs[,sorted_eval[i]])
}