#install.packages('e1071')
#install.packages('kernlab')
#install.packages('remotes')
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#Include files
source("FLD.r")
source("SVD.r")
source("PCA.r")
source("NCA.r")
source("classifySVM.r")

#Include libraries
library(e1071)
library(MASS)
library(caTools)
library(kernlab)
library(class)

dataset=read.table("wine.data",sep=",")
x<-dataset[,-1]
y<-dataset[,1]
new_dim<-10

#Original
org<-classifySVM(dataset)

#Applying FLD
fld_dataset<-fld_reduce(x,y,new_dim)
fld1<-classifySVM(fld_dataset)

#Applying SVD
svd_dataset<-svd_reduce(x,y,new_dim)
svd<-classifySVM(svd_dataset)

#Applying PCA
pca_dataset<-pca_reduce(x,y,new_dim)
pca<-classifySVM(pca_dataset)

#Applyng NCA
nca_dataset<-nca_reduce(x,y,new_dim)
nc<-classifySVM(nca_dataset)

cat("Accuracy for original dataset: ", org$acc,"\n")
cat("Confusion matrix for original dataset\n")
print(org$cm)
cat("Accuracy for FLD reduced dataset: ", fld$acc,"\n")
cat("Confusion matrix for FLD reduced dataset\n")
print(fld$cm)
cat("Accuracy for SVD reduced dataset: ", svd$acc,"\n")
cat("Confusion matrix for SVD reduced dataset\n")
print(svd$cm)
cat("Accuracy for PCA reduced dataset: ", pca$acc,"\n")
cat("Confusion matrix for PCA reduced dataset\n")
print(pca$cm)
cat("Accuracy for PCA reduced dataset: ", nca$acc,"\n")
cat("Confusion matrix for PCA reduced dataset\n")
print(nca$cm)