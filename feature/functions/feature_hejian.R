
#### feature 1 : number of not 0 balance
###

not0balance=function(train, test)
{
  saldo.name=names(train)[grep("saldo",names(train))]
  train.saldo=train[,saldo.name]
  test.saldo=test[,saldo.name]
  train.f=apply(train.saldo,1,function(x) sum(x!=0))
  test.f=apply(test.saldo,1,function(x) sum(x!=0))
  
  return(list(train.feature=train.f,test.feature=test.f))
  
}

f1=not0balance(train,test)

#lapply(not0balance(train,test),head)


### qunatile difference between saldo variables
quantile.diff=function(train,test)
{
  saldo.name=names(train)[grep("saldo",names(train))]
  train.saldo=train[,saldo.name]
  test.saldo=test[,saldo.name]
  train.f=cbind(apply(train.saldo,1,function(x) max(x)-sort(x)[length(x)-1]),
                apply(train.saldo,1,function(x) max(x)-sort(x)[length(x)-2]),
                apply(train.saldo,1,function(x) max(x)-sort(x)[length(x)-3]))
  test.f=cbind(apply(test.saldo,1,function(x) max(x)-sort(x)[length(x)-1]),
               apply(test.saldo,1,function(x) max(x)-sort(x)[length(x)-2]),
               apply(test.saldo,1,function(x) max(x)-sort(x)[length(x)-3]))
  return(list(train.feature=train.f,test.feature=test.f))
}
f2=quantile.diff(train,test)

quantile.ratio=function(train,test)
{
  saldo.name=names(train)[grep("saldo",names(train))]
  train.saldo=train[,saldo.name]
  test.saldo=test[,saldo.name]
  train.f=cbind(apply(train.saldo,1,function(x) max(x)/sort(x)[length(x)-1]),
                apply(train.saldo,1,function(x) max(x)/sort(x)[length(x)-2]),
                apply(train.saldo,1,function(x) max(x)/sort(x)[length(x)-3]))
  test.f=cbind(apply(test.saldo,1,function(x) max(x)/sort(x)[length(x)-1]),
               apply(test.saldo,1,function(x) max(x)/sort(x)[length(x)-2]),
               apply(test.saldo,1,function(x) max(x)/sort(x)[length(x)-3]))
  return(list(train.feature=train.f,test.feature=test.f))
}

f3=quantile.ratio(train,test)

setwd("F:/Dropbox/502X/KAGGAL/feature")

write.csv(f1[[1]],"numberofnot0balance_train.csv",row.names = F)
write.csv(f1[[2]],"numberofnot0balance_test.csv",row.names = F)
write.csv(f2[[1]],"quantilediffbalance_train.csv",row.names = F)
write.csv(f2[[2]],"quantilediffbalance_test.csv",row.names = F)
write.csv(f3[[1]],"quantileratiobalance_tain.csv",row.names = F)
write.csv(f3[[2]],"quantileratiobalance_test.csv",row.names = F)
