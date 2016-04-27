#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
NFOLD = as.numeric(args[1])
if(is.na(NFOLD))
    NFOLD = 10
cat("nfold: ", NFOLD, "\n")
library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

# load in the training data
trn = readRDS("train_clean.RDS")
# tst = readRDS("test_clean.RDS")

ftrn = read.csv("../../feature/feature_all_train_ratio_only_wc_99.csv")
# ftst = read.csv("../../feature/feature_all_test_ratio_only_wc_99.csv")
trn = left_join(trn, ftrn)
# tst = left_join(tst, ftst)
y = trn$TARGET
trn$TARGET=NULL
x = Matrix(as.matrix(trn[, -1]), sparse = TRUE)

# ## xgboost parameters selector
# xgboostPS <- function()
xg_auc = data.frame("Rounds" = numeric(), 
                    "Depth" = numeric(),
                    "mcw" = numeric(),
                    "r_sample" = numeric(),
                    "c_sample" = numeric(), 
                    "eta" = numeric(),
                    "scale_pos_weight" = numeric(),
                    "auc_max" = numeric(),
                    "std" = numeric(),
                    "best_round" = numeric())
# ## First round tunning parameters
# ROUNDS = c(10, 100, 500)
# DEPTH = c(2, 4, 6, 8, 10)
# RSAMPLE = c(0.5, 0.75, 1)
# CSAMPLE = c(0.6, 0.8, 1)
# ETA =  c(0.1, 0.01, 0.001, 0.0001)
# SCALE = 0
## Second round tunning parameters
ROUNDS = 3000
DEPTH = c(4, 5, 6)
MCW = c(0.5, 1, 2)
RSAMPLE = c(0.6, 0.7, 0.8)
CSAMPLE = c(0.5, 0.6, 0.7)
ETA =  c(0.05, 0.01, 0.005)
SPWEIGHT = c(1, 2, 3)

for (rounds in ROUNDS){
  for (depth in DEPTH) {
    for(mcw in MCW){
    for (r_sample in RSAMPLE) {
      for (c_sample in CSAMPLE) {
        for(eta_val in ETA){
          for(sp_weight in SPWEIGHT){
            set.seed(1024)
            xgb_cv = xgb.cv(data = x, label = y, 
                            nfold = NFOLD,
                            stratified = TRUE,    
                            nrounds = rounds, 
                            eta = eta_val, 
                            max_depth = depth,
                            min_child_weight = mcw,
                            subsample = r_sample, 
                            colsample_bytree = c_sample,
                            early.stop.round = 30,
                            scale_pos_weight = sp_weight,
                            objective='binary:logistic', 
                            eval_metric = 'auc',
                            verbose = FALSE)
      m = xgb_cv$test.auc.mean
      std = xgb_cv$test.auc.std
      idx = order(m, decreasing = TRUE)[1:5]
      m.top5  = m[idx]
      std.top5 = std[idx]
      n.top5 = which(m %in% m.top5)

            print(paste(rounds, depth, mcw, r_sample, c_sample, eta_val, sp_weight, mean(m.top5), mean(std.top5), mean(n.top5)))
            xg_auc[nrow(xg_auc)+1, ] = c(rounds, 
                                         depth,
                                         mcw,
                                         r_sample, 
                                         c_sample, 
                                         eta_val,
                                         sp_weight,
                                         mean(m.top5),
                                         mean(std.top5),
                                         mean(n.top5))
          }
        }
      }
    }
  }
}
}

write.csv(xg_auc, file = "tuning/xg_feature_all_99_PS1.csv", row.names=FALSE, quote = FALSE)

# xg_auc = read.csv("xgboost_PS.csv")
# head(xg_auc)
# idx = which.max(xg_auc$auc_max)
# xg_auc[idx,]
