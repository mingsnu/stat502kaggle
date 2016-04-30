#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
out.name = args[1]
cat("Out file name:", out.name, "\n")
ftr.rds = args[2]
R = as.numeric(args[3])
if(is.na(ftr.rds))
  ftrSelected = c("saldo_var30", "var15", "var38") else
    ftrSelected = readRDS(ftr.rds)
cat("Initial features: ", ftrSelected, "\n")
if(is.na(R))
  R = 5
cat("R: ", R, "\n")
library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)
library(data.table)

### Loading data & features
trn = readRDS("train_clean.RDS")
#tst = readRDS("test_clean.RDS")
ftrn = read.csv("../../feature/feature_all_train_ratio_only_wc_99.csv")
#ftst = read.csv("../../feature/feature_all_test_ratio_only_wc_99.csv")
trn = left_join(trn, ftrn)
#tst = left_join(tst, ftst)
trn = data.table(trn)
#tst = data.table(tst)

trn.y = trn$TARGET
impVars = readRDS("impVars.rds")
# impVars = setdiff(impVars, c("var15_ratio", "var38_ratio"))

#### stratified sampling for training & testing
set.seed(20160428)
trn.idx = createDataPartition(y = trn.y, times = R, p = .75)

####
optpar = data.frame(Rounds=2000, Depth = 4, r_sample = 0.8, eta =0.01)

aucMean = seq(0,0.8, length=length(ftrSelected))
aucStd = rep(0.1, length(ftrSelected))
res = data.table(ftrSelected, aucMean, aucStd)
k = 1
while(1){
  k = k + 1
  cat("k: ", k, "\n")
  impVars = setdiff(impVars, ftrSelected)
  if(length(impVars) == 0) stop("Can't believe it, all features are selected!!!")
  ## keep track of the auc value for each important feature
  impVars.auc.mean = c()
  impVars.auc.std = c()
  for(i in 1:length(impVars)){
    ## add a new feature to the existing feature list
    ftr = c(ftrSelected, impVars[i])
    ftr.idx = which(names(trn) %in% ftr)
    if(length(ftr) != length(ftr.idx))
      stop("length(ftr) doesn't equal to length(ftr.idx).")
    ## store auc values for each folder data set
    score = c()
    for(j in 1:R){
      training = trn[trn.idx[[j]], ftr.idx, with=FALSE]
      training.y = trn.y[trn.idx[[j]]]
      testing = trn[-trn.idx[[j]], ftr.idx, with=FALSE]
      testing.y = trn.y[-trn.idx[[j]]]
      
      dtrain <- xgb.DMatrix(data=Matrix(as.matrix(training), sparse = TRUE), label=training.y, missing = -999)
      dtest <- xgb.DMatrix(data=Matrix(as.matrix(testing), sparse = TRUE), label=testing.y, missing = -999)
      watchlist <- list(test=dtest)
      
      print("Train xgboost using xgb.train with watchlist")
      params = list(
        objective = "binary:logistic",
        eta = optpar$eta, 
        max_depth = optpar$Depth, 
        subsample = optpar$r_sample,
        eval_metric = "auc"
      )
      bst <- xgb.train(params = params,
                       data=dtrain,
                       nrounds = 2000,
                       watchlist=watchlist,
                       verbose = 0,
                       early.stop.round = 20)
      score = c(score, bst$bestScore)
    }
    impVars.auc.mean[i] = mean(score)
    impVars.auc.std[i] = sd(score)
  }
  idx = which.max(impVars.auc.mean)
  cat(c(impVars[idx], impVars.auc.mean[idx], impVars.auc.std[idx]), sep=",")
  
  ftrSelected = c(ftrSelected, impVars[idx])
  aucMean = c(aucMean, impVars.auc.mean[idx])
  aucStd = c(aucStd, impVars.auc.std[idx])
  res = data.table(ftrSelected, aucMean, aucStd)
  # res = rbindlist(list(res, list(impVars[idx], impVars.auc.mean[idx], impVars.auc.std[idx])))
  write.csv(res, out.name, row.names = FALSE)
  if(k > 10){
    last10 = res$aucMean[(k-9):k]
    if(sum(sign(diff(last10))) < 0) 
      break
  }
}

