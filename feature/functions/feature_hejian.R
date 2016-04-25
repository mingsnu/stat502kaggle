
#### feature 1 : number of not 0 balance
### saldo

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
f1=lapply(f1,function(x) return(data.frame(ID=1:length(x),nonzerocount.saldo=x)))

#lapply(not0balance(train,test),head)


### qunatile difference between saldo variables
quantile.diff=function(train,test)
{
  saldo.name=names(train)[grep("saldo",names(train))]
  train.saldo=train[,saldo.name]
  test.saldo=test[,saldo.name]
  train.f=cbind(apply(train.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-1]),
                apply(train.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-2]),
                apply(train.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-3]))
  test.f=cbind(apply(test.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-1]),
               apply(test.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-2]),
               apply(test.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-3]))
  return(list(train.feature=train.f,test.feature=test.f))
}
f2=quantile.diff(train,test)

f2=lapply(f2, function(x) {
  x=data.frame(x)
  names(x)=paste("sald0_quantile_diff",1:3,sep="_")
  cbind(ID=1:nrow(x),x)
})

quantile.max=function(train,test)
{
  saldo.name=names(train)[grep("saldo",names(train))]
  train.saldo=train[,saldo.name]
  test.saldo=test[,saldo.name]
  train.f=apply(train.saldo,1,function(x) max(x,na.rm = T))
  test.f=apply(test.saldo,1,function(x) max(x,na.rm = T))
  return(list(train.feature=train.f,test.feature=test.f))
}

 f3=quantile.max(train,test)
 f3=lapply(f3, function(x) {
   x=data.frame(x)
   names(x)="max.saldo"
   cbind(ID=1:nrow(x),x)
 })

###### num

 not0balance=function(train, test)
 {
   saldo.name=names(train)[grep("^num",names(train))]
   train.saldo=train[,saldo.name]
   test.saldo=test[,saldo.name]
   train.f=apply(train.saldo,1,function(x) sum(x!=0))
   test.f=apply(test.saldo,1,function(x) sum(x!=0))
   
   return(list(train.feature=train.f,test.feature=test.f))
   
 }
 
 f4=not0balance(train,test)
 f4=lapply(f4,function(x) return(data.frame(ID=1:length(x),nonzerocount.num=x)))
 
 #lapply(not0balance(train,test),head)
 
 
 ### qunatile difference between saldo variables
 quantile.diff=function(train,test)
 {
   saldo.name=names(train)[grep("^num",names(train))]
   train.saldo=train[,saldo.name]
   test.saldo=test[,saldo.name]
   train.f=cbind(apply(train.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-1]),
                 apply(train.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-2]),
                 apply(train.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-3]))
   test.f=cbind(apply(test.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-1]),
                apply(test.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-2]),
                apply(test.saldo,1,function(x) max(x,na.rm = T)-sort(x,na.last = NA)[length(x)-3]))
   return(list(train.feature=train.f,test.feature=test.f))
 }
 f5=quantile.diff(train,test)
 
 f5=lapply(f5, function(x) {
   x=data.frame(x)
   names(x)=paste("num_quantile_diff",1:3,sep="_")
   cbind(ID=1:nrow(x),x)
 })
 
 quantile.max=function(train,test)
 {
   saldo.name=names(train)[grep("^num",names(train))]
   train.saldo=train[,saldo.name]
   test.saldo=test[,saldo.name]
   train.f=apply(train.saldo,1,function(x) max(x,na.rm = T))
   test.f=apply(test.saldo,1,function(x) max(x,na.rm = T))
   return(list(train.feature=train.f,test.feature=test.f))
 }
 
 f6=quantile.max(train,test)
 f6=lapply(f6, function(x) {
   x=data.frame(x)
   names(x)="max.num"
   cbind(ID=1:nrow(x),x)
 })
 

 #####################################
 ##
 
######
setwd("C:/Users/hjsang/Dropbox/502X/KAGGAL/feature")

write.csv(f1[[1]],"numberofnot0balance_train.csv",row.names = F)
write.csv(f1[[2]],"numberofnot0balance_test.csv",row.names = F)
write.csv(f2[[1]],"quantilediffbalance_train.csv",row.names = F)
write.csv(f2[[2]],"quantilediffbalance_test.csv",row.names = F)
write.csv(f3[[1]],"quantileratiobalance_tain.csv",row.names = F)
write.csv(f3[[2]],"quantileratiobalance_test.csv",row.names = F)

write.csv(f4[[1]],"numberofnot0num_train.csv",row.names = F)
write.csv(f4[[2]],"numberofnot0num_test.csv",row.names = F)
write.csv(f5[[1]],"quantilediffnum_train.csv",row.names = F)
write.csv(f5[[2]],"quantilediffnum_test.csv",row.names = F)
write.csv(f6[[1]],"quantilerationum_tain.csv",row.names = F)
write.csv(f6[[2]],"quantilerationum_test.csv",row.names = F)


