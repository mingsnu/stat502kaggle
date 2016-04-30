#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
m = as.numeric(args[1])
R = as.numeric(args[2])
if(is.na(m))
  m = 20
if(is.na(R))
  R = 5
cat("m: ", m, " R: ", R, "\n")
library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

### Loading data & features
trn = readRDS("train_clean.RDS")
#tst = readRDS("test_clean.RDS")
ftrn = read.csv("../../feature/feature_all_train_ratio_only_wc_99.csv")
#ftst = read.csv("../../feature/feature_all_test_ratio_only_wc_99.csv")
trn = left_join(trn, ftrn)
#tst = left_join(tst, ftst)

trn.y = trn$TARGET
impVars = readRDS("impVars.rds")

#### stratified sampling for train
set.seed(20160428)
trn.idx = createDataPartition(y = trn.y, times = R, p = .7)

############### 
## Number of feature to use
N = 100000
M = length(impVars)
optpar = data.frame(Rounds=2000, Depth = 4, r_sample = 0.8, eta =0.01,
                    best_round = 774 )
res = data.frame(matrix(ncol=m+R, nrow=0))
names(res) = c(paste0("f", 1:m), paste0("auc", 1:R))
k = 0
for(i in 1:N){
  ftr = impVars[sample(1:M, m, prob = M:1)]
  ftr.idx = names(trn) %in% ftr
  score = c()
  for(j in 1:R){
    training = trn[trn.idx[[j]], ftr.idx]
    training.y = trn.y[trn.idx[[j]]]
    testing = trn[-trn.idx[[j]], ftr.idx]
    testing.y = trn.y[-trn.idx[[j]]]
    
    dtrain <- xgb.DMatrix(data=Matrix(as.matrix(training), sparse = TRUE), label=training.y, missing = -999)
    dtest <- xgb.DMatrix(data=Matrix(as.matrix(testing), sparse = TRUE), label=testing.y, missing = -999)
    watchlist <- list(test=dtest)
    
    print("Train xgboost using xgb.train with watchlist")
    params = list(
      objective = "binary:logistic",    # binary classification
      eta = optpar$eta,       # learning rate
      max_depth = optpar$Depth,      # max tree depth
      subsample = optpar$r_sample,
      eval_metric = "auc"     # evaluation/auc metric
    )
    bst <- xgb.train(params = params,
                     data=dtrain,
                     nrounds = 2000,
                     watchlist=watchlist,
                     verbose = 0,
                     early.stop.round = 20)
    if(bst$bestScore < 0.8) next
    score = c(score, bst$bestScore)
  }
  if(length(score) == R){
    k = k + 1
    res[k,] = c(ftr, score)
    write.csv(res, paste0("tuning/freedom_m", m, "R_", R, ".csv"), row.names = FALSE)
  }
}

