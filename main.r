#install.packages('e1071')
setwd(".")
source("FLD.r")
library(e1071)
library(MASS)
dataset=read.table("wine.data",sep=",")
#Classifier for original
set.seed(123)
split=sample.split(dataset,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)
classifier<-svm(formula=V1~.,data=training_set, type='C-classification',kernel='linear')
y_pred<-predict(classifier, newdata = testing_set[,-1])
y_act<-testing_set[,1]
cm=table(y_act,y_pred)
print("Confusion matrix for original:-")
print(cm)

#Classifier for reduced
x<-dataset[,-1]
y<-dataset[,1]
new_dim<-11
reduced_dataset<-data.frame(fld(x,y,new_dim))
training_set=subset(reduced_dataset,split==TRUE)
testing_set=subset(reduced_dataset,split==FALSE)
classifier<-svm(formula=X1~.,data=training_set, type='C-classification',kernel='linear')
y_pred<-predict(classifier, newdata = testing_set[,-1])
y_act<-testing_set[,1]
cm=table(y_act,y_pred)
print("Confusion matrix for reduced:-")
print(cm)

