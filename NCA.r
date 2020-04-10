source("NCAsrc.r")

nca_reduce<-function(x,y,new_dim){
  var_list<-nca(x,y,A_init = diag(x=1,nrow = new_dim,ncol = ncol(x)))
  transformation_matrix<-var_list$A_norm
  reduced_x<-transformation_matrix%*%t(x)
  reduced_x<-t(reduced_x)
  reduced_dataset<-data.frame(cbind(y,reduced_x))
  return(reduced_dataset)
}
