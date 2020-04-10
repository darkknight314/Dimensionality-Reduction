classifyKNN<-function(dataset){
  colnames(dataset)[1]<-"V1"
  set.seed(152)
  split<-sample.split(dataset,SplitRatio = 0.8)
  training_set<-subset(dataset,split==TRUE)
  testing_set<-subset(dataset,split==FALSE)
  y_pred<-knn(training_set[,-1],testing_set[,-1],training_set[,1],k=3)
  y_act<-testing_set[,1]
  conf.matrix<-table(y_act,y_pred)
  accuracy<-mean(y_act==y_pred)
  return(list("cm"=conf.matrix,"acc"=accuracy))
}