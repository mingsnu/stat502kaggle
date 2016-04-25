num_of_product<- function(train.d,test.d){
  train.data<- as.data.frame(train.d)
  test.data<- as.data.frame(test.d)
  
  train_total<- train.d[,grep("ind_var",colnames(train.d))]
  train_used_var<- unique(gsub("*_0","",colnames(train.d)[grep("ind_var",colnames(train.d))]))
  train_used<- train_total[,which(colnames(train_total) %in% train_used_var)]
  train_recommended<- train_total[,which(!(colnames(train_total) %in% train_used_var))]
  train<- data.frame(cbind(train.data$ID,apply(train_used,1,sum),apply(train_recommended,1,sum)))
  names(train)<- c("ID","num_of_used","num_of_recommended")
  
  test_total<- test.d[,grep("ind_var",colnames(test.d))]
  test_used_var<- unique(gsub("*_0","",colnames(test.d)[grep("ind_var",colnames(test.d))]))
  test_used<- test_total[,which(colnames(test_total) %in% test_used_var)]
  test_recommended<- test_total[,which(!(colnames(test_total) %in% test_used_var))]
  test<- data.frame(cbind(test.data$ID,apply(test_used,1,sum),apply(test_recommended,1,sum)))
  names(test)<- c("ID","num_of_used","num_of_recommended")
  
  list(train=train,test=test)
}

# res<- num_of_product(train.d,test.d)
# head(res[[1]])
# head(res[[2]])

# write.csv(res[[1]],file="feature_basd_on_num_of_product_train.csv",row.names=F)
# write.csv(res[[2]],file="feature_basd_on_num_of_product_test.csv",row.names=F)

