library(dplyr)
trn = readRDS("../../data/train.rds")
dim(trn) # [1] 76020   337
trn[trn[,2] < 0, 2] = NA
trn.unique.length = apply(trn, 2, function(x) length(unique(x)))
data.frame(var = names(trn.unique.length), uni_length = trn.unique.length)
plot(trn.unique.length)
plot(trn.unique.length, ylim=c(0,2000))
plot(trn.unique.length, ylim=c(0,200))
plot(trn.unique.length, ylim=c(0,10))
var.cat = trn[which(trn.unique.length ==9)]
apply(var.cat, 2, function(x) sort(unique(x)))

var.cat = trn[which(trn.unique.length ==21)]
apply(var.cat, 2, function(x) sort(unique(x)))


i=4
trni = trn[,i]
head(trni)
summary(trn[,i])
trni[trni >2]
hist(trn[,i])
