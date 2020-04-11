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
source("classifyKNN.r")
#Include libraries
library(caTools)
library(e1071)
library(class)


dataset=read.table("sonar.all-data",sep=",")
y<-dataset[,61]
dataset<-cbind(y,dataset[,-61])
x<-dataset[,-1]
new_dim<-20

#Original
org1<-classifySVM(dataset)
org2<-classifyKNN(dataset)

#Applying FLD
fld_dataset<-fld_reduce(x,y,new_dim)
fld1<-classifySVM(fld_dataset)
fld2<-classifyKNN(fld_dataset)

#Applying SVD
svd_dataset<-svd_reduce(x,y,new_dim)
svd1<-classifySVM(svd_dataset)
svd2<-classifyKNN(svd_dataset)

#Applying PCA
pca_dataset<-pca_reduce(x,y,new_dim)
pca1<-classifySVM(pca_dataset)
pca2<-classifyKNN(pca_dataset)

#Applyng NCA
nca_dataset<-nca_reduce(x,y,new_dim)
nca1<-classifySVM(nca_dataset)
nca2<-classifyKNN(nca_dataset)

cat("SVM and KNN accuracy for original dataset: ", org1$acc,org2$acc,"\n")
cat("SVM and KNN accuracy for FLD reduced dataset: ", fld1$acc,fld2$acc,"\n")
cat("SVM and KNN accuracy for SVD reduced dataset: ", svd1$acc,svd2$acc,"\n")
cat("SVM and KNN accuracy for PCA reduced dataset: ", pca1$acc,pca2$acc,"\n")
cat("SVM and KNN accuracy for NCA reduced dataset: ", nca1$acc,nca2$acc,"\n")

cat("Confusion matrix for original dataset\n")
print(org1$cm)
print(org2$cm)
cat("Confusion matrix for FLD reduced dataset\n")
print(fld1$cm)
cat("Confusion matrix for SVD reduced dataset\n")
print(svd1$cm)
cat("Confusion matrix for PCA reduced dataset\n")
print(pca1$cm)
cat("Confusion matrix for NCA reduced dataset\n")
print(nca1$cm)
