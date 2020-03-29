#install.packages('e1071')
setwd(".")
#Include files
source("FLD.r")
source("SVD.r")
source("PCA.r")
#Include libraries
library(e1071)
library(MASS)
library(caTools)
dataset=read.table("wine.data",sep=",")
x<-dataset[,-1]
y<-dataset[,1]
new_dim<-5


#Classifier for original
set.seed(13)
split<-sample.split(dataset,SplitRatio = 0.7)
training_set<-subset(dataset,split==TRUE)
testing_set<-subset(dataset,split==FALSE)
classifier<-svm(formula=V1~.,data=training_set, type='C-classification',kernel='linear')
y_pred<-predict(classifier, newdata = testing_set[,-1])
y_act<-testing_set[,1]
cm<-table(y_act,y_pred)
print("Confusion matrix for original:-")
print(cm)

#Classifier for Fisher Linear Discriminant
fld_dataset<-fld_reduce(x,y,new_dim)
training_set<-subset(fld_dataset,split==TRUE)
testing_set<-subset(fld_dataset,split==FALSE)
classifier<-svm(formula=X1~.,data=training_set, type='C-classification',kernel='linear')
y_pred<-predict(classifier, newdata = testing_set[,-1])
y_act<-testing_set[,1]
cm<-table(y_act,y_pred)
print("Confusion matrix for reduced using FLD:-")
print(cm)

#SVD
svd_dataset<-svd_reduce(x,y,new_dim)
training_set<-subset(svd_dataset,split==TRUE)
testing_set<-subset(svd_dataset,split==FALSE)
classifier<-svm(formula=y~.,data=training_set, type='C-classification',kernel='linear')
y_pred<-predict(classifier, newdata = testing_set[,-1])
y_act<-testing_set[,1]
cm<-table(y_act,y_pred)
print("Confusion matrix for original:-")
print(cm)

#PCA
pca_dataset<-pca_reduce(x,y,new_dim)
training_set<-subset(pca_dataset,split==TRUE)
testing_set<-subset(pca_dataset,split==FALSE)
classifier<-svm(formula=y~.,data=training_set, type='C-classification',kernel='linear')
y_pred<-predict(classifier, newdata = testing_set[,-1])
y_act<-testing_set[,1]
cm<-table(y_act,y_pred)
print("Confusion matrix for original:-")
print(cm)

