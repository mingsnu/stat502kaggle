#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
method = as.numeric(args[1])
library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(dplyr)
library(data.table)

#fdm = read.csv("tuning/Freedom/freedom_m20.csv", stringsAsFactors = FALSE)
fdm = read.csv("tuning/freedom_m20R_10.csv", stringsAsFactors = FALSE)
R = 10
m = ncol(fdm)
n = nrow(fdm)
fdm.auc = fdm[, (m-R+1):m]

## choose feature combination by using mean value
fdm$aucMean = apply(fdm.auc, 1, mean)
fdm$aucStd = apply(fdm.auc, 1, sd)
fdm %>% filter(aucMean > 0.835) %>% select(starts_with("auc"))
maxmean.idx = which(fdm$aucMean > 0.835)
maxmean.n = length(maxmean.idx)
ftrs.list = list()
for(i in 1:maxmean.n){
  ftrs.list[[i]] = unname(unlist(fdm[maxmean.idx[i], 1:20]))
}
ftrs.list

trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
ftrn = read.csv("../../feature/feature_all_train_ratio_only_wc_all.csv")
# ftst = read.csv("../../feature/feature_all_test_ratio_only_wc_all.csv")
trn.y = trn$TARGET
trn$TARGET = NULL

if(method==1){
  ## using raw data only
  training = trn
#  testing = tst
  training$ID = NULL
#  testing$ID = NULL
  training.dat = Matrix(as.matrix(training), sparse = TRUE)
  dtrain <- xgb.DMatrix(data=training.dat, label=trn.y, missing = -999)
}
if(method==2){
  ## using raw data + all feature directly
  training = left_join(trn, ftrn)
#  testing = left_join(tst, ftst)
  training$ID = NULL
#  testing$ID = NULL
  training.dat = Matrix(as.matrix(training), sparse = TRUE)
  dtrain <- xgb.DMatrix(data=training.dat, label=trn.y, missing = -999)
}

if(method==3){
  ## using selected features from 'freedom'
  training = left_join(trn, ftrn)
#  testing = left_join(tst, ftst)
  training$ID = NULL
#  testing$ID = NULL
  ftr.idx = names(training) %in% unique(unlist(ftrs.list)) #752
  training = training[, ftr.idx]
#  testing = testing[, ftr.idx]
  training.dat = Matrix(as.matrix(training), sparse = TRUE)
  dtrain <- xgb.DMatrix(data=training.dat, label=trn.y, missing = -999)
}
if(method==4){
  ## using raw + selected features
  ftr.idx = which(names(ftrn) %in% c("ID", unique(unlist(ftrs.list))))
  training = left_join(trn, ftrn[, ftr.idx])
#  testing = left_join(tst, ftst[, ftr.idx])
  training$ID = NULL
#  testing$ID = NULL
#  dim(training); dim(testing)
  training.dat = Matrix(as.matrix(training), sparse = TRUE)
  dtrain <- xgb.DMatrix(data=training.dat, label=trn.y, missing = -999)
}


optpar = data.frame(Rounds=2000, Depth = 5, r_sample = 0.7, eta =0.01)
params = list(
  objective = "binary:logistic",
  eta = optpar$eta, 
  max_depth = optpar$Depth, 
  subsample = optpar$r_sample,
  eval_metric = "auc")

res = NULL
for(i in 1:10){
  xgb_cv <- xgb.cv(params = params,
                   data=dtrain,
                   nfold = 4,
                   nrounds = 2000,
                   verbose = 0,
                   early.stop.round = 20)
  m = xgb_cv$test.auc.mean
  std = xgb_cv$test.auc.std
  idx = order(m, decreasing = TRUE)[1:5]
  m.top5  = m[idx]
  std.top5 = std[idx]
  n.top5 = which(m %in% m.top5)
  res = rbind(res, c(mean(m.top5), 
                             mean(std.top5),
                             mean(n.top5)))
}
colnames(res)=c("aucMean", "aucStd", "bestRound")
write.csv(res, paste0("comparison/method", method, ".csv"), row.names = FALSE)

# res = NULL
# for(i in 1:4){
#   res[[i]] = read.csv(paste0("comparison/method", i, ".csv"))
# }
# for(i in 1:3){
#   for(j in (i+1):4){
#     cat(i, " and ", j, ":\n")
#     print(t.test(res[[i]]$aucMean, res[[j]]$aucMean))
#   }
# }
# 
# for(i in 1:3){
#   for(j in (i+1):4){
#     cat(i, " and ", j, ":\n")
#     print(t.test(res[[i]]$aucStd, res[[j]]$aucStd))
#   }
# }
# 
# sapply(1:4, function(i) mean(res[[i]]$aucMean))
