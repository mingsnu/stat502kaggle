#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
NFOLD = as.numeric(args[1])
if(is.na(NFOLD))
  NFOLD = 7
cat("nfold: ", NFOLD, "\n")
library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

trn = readRDS("train_final_all.RDS")
#tst = readRDS("test_final_all.RDS")
out.name = "final_all"
y = trn$TARGET
trn$TARGET=NULL
x = Matrix(as.matrix(trn[, -1]), sparse = TRUE)

###########################################################
############# 5 Steps in Tuning parameters ################
###########################################################
### Step 1: Tuning `max_depth` and `min_child_weight`
cat("Step1: Tuning max_depth and min_child\n")
if(!exists(paste0("tuning/", out.name, "_1.csv"))){
  ### initial parameters
  ROUNDS = 3000
  DEPTH = c(4, 5, 6)
  MCW = c(0.5, 1, 2, 3, 4)
  RSAMPLE = 0.7
  CSAMPLE = 0.5
  ETA =  0.01
  SPWEIGHT = 1
  GAMMA = 0
  
  xgb_params = list(
    objective = "binary:logistic",    # binary classification
    eta = ETA,       # learning rate
    # max_depth = 5,      # max tree depth
    # min_child_weight = 1,
    subsample = RSAMPLE,
    colsample_bytree = CSAMPLE,
    gamma = GAMMA,
    eval_metric = "auc"     # evaluation/auc metric
  )
  
  xg_auc1 = data.frame("Depth" = numeric(),
                       "Min_child_weight" = numeric(),
                       "r_sample" = numeric(),
                       "c_sample" = numeric(), 
                       "eta" = numeric(),
                       "scale_pos_weight" = numeric(),
                       "gamma" = numeric(),
                       "auc_max" = numeric(),
                       "std" = numeric(),
                       "best_round" = numeric())
  for(depth in DEPTH)
    for(mcw in MCW){
      xgb_cv = xgb.cv(params = xgb_params,
                      data = x,
                      label = y,
                      max_depth = depth,
                      nrounds = ROUNDS, 
                      nfold = NFOLD,                                                   # number of folds in K-fold
                      stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                      verbose = FALSE,
                      print.every.n = 1, 
                      early.stop.round = 50,
                      min_child_weight = mcw,
                      scale_pos_weight = SPWEIGHT  ## rate of 0/1
      )
      m = xgb_cv$test.auc.mean
      std = xgb_cv$test.auc.std
      idx = order(m, decreasing = TRUE)[1:5]
      m.top5  = m[idx]
      std.top5 = std[idx]
      n.top5 = which(m %in% m.top5)
      
      print(paste(depth, mcw, mean(m.top5), mean(std.top5)))
      xg_auc1[nrow(xg_auc1)+1, ] = c(depth, 
                                     mcw,
                                     RSAMPLE,
                                     CSAMPLE,
                                     ETA,
                                     SPWEIGHT,
                                     GAMMA,
                                     mean(m.top5), 
                                     mean(std.top5),
                                     mean(n.top5))
    }
  write.csv(xg_auc1, paste0("tuning/", out.name, "_1.csv"), row.names = FALSE, quote = FALSE)
}


### Step 2: Tuning `gamma`
cat("Step2: Tuning gamma\n")
if(!exists(paste0("tuning/", out.name, "_2.csv"))){
  GAMMA = c(seq(0, 1, by=0.1), 2, 5)
  xg_auc1 = read.csv(paste0("tuning/", out.name, "_1.csv"))
  optpar1 = xg_auc1[which.max(xg_auc1$auc_max),]
  xgb_params = list(
    objective = "binary:logistic",    # binary classification
    eta = ETA,       # learning rate
    max_depth = optpar1$Depth,      # max tree depth
    min_child_weight = optpar1$Min_child_weight,
    subsample = RSAMPLE,
    colsample_bytree = CSAMPLE,
    #    gamma = GAMMA,
    eval_metric = "auc"     # evaluation/auc metric
  )
  
  xg_auc2 = data.frame("Depth" = numeric(),
                       "Min_child_weight" = numeric(),
                       "r_sample" = numeric(),
                       "c_sample" = numeric(), 
                       "eta" = numeric(),
                       "scale_pos_weight" = numeric(),
                       "gamma" = numeric(),
                       "auc_max" = numeric(),
                       "std" = numeric(),
                       "best_round" = numeric())
  
  for(gamma in GAMMA){
    xgb_cv = xgb.cv(params = xgb_params,
                    data = x,
                    label = y,
                    gamma = gamma,
                    nrounds = ROUNDS, 
                    nfold = NFOLD,                                                   # number of folds in K-fold
                    stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                    verbose = FALSE,
                    print.every.n = 1, 
                    early.stop.round = 50,
                    scale_pos_weight = SPWEIGHT  ## rate of 0/1
    )
    m = xgb_cv$test.auc.mean
    std = xgb_cv$test.auc.std
    idx = order(m, decreasing = TRUE)[1:5]
    m.top5  = m[idx]
    std.top5 = std[idx]
    n.top5 = which(m %in% m.top5)
    
    print(paste(gamma, mean(m.top5), mean(std.top5)))
    xg_auc2[nrow(xg_auc2)+1, ] = c(optpar1$Depth, 
                                   optpar1$Min_child_weight,
                                   RSAMPLE,
                                   CSAMPLE,
                                   ETA,
                                   SPWEIGHT,
                                   gamma,
                                   mean(m.top5), 
                                   mean(std.top5),
                                   mean(n.top5))
  }
  write.csv(xg_auc2, paste0("tuning/", out.name, "_2.csv"), row.names = FALSE, quote = FALSE)
}

#### Step3: Tuning `subsample` and `colsample_bytree`
cat("Step3: Tuning `subsample` and `colsample_bytree`\n")
if(!exists(paste0("tuning/", out.name, "_3.csv"))){
  RSAMPLE = seq(0.5, 0.8, by=0.05)
  CSAMPLE = seq(0.3, 0.8, by=0.05)
  
  xg_auc1 = read.csv(paste0("tuning/", out.name, "_1.csv"))
  xg_auc2 = read.csv(paste0("tuning/", out.name, "_2.csv"))
  optpar1 = xg_auc1[which.max(xg_auc1$auc_max),]
  optpar2 = xg_auc2[which.max(xg_auc2$auc_max),]
  xgb_params = list(
    objective = "binary:logistic",    # binary classification
    eta = ETA,       # learning rate
    max_depth = optpar1$Depth,      # max tree depth
    min_child_weight = optpar1$Min_child_weight,
    #    subsample = RSAMPLE,
    #    colsample_bytree = CSAMPLE,
    gamma = optpar2$gamma,
    eval_metric = "auc"     # evaluation/auc metric
  )
  
  xg_auc3 = data.frame("Depth" = numeric(),
                       "Min_child_weight" = numeric(),
                       "r_sample" = numeric(),
                       "c_sample" = numeric(), 
                       "eta" = numeric(),
                       "scale_pos_weight" = numeric(),
                       "gamma" = numeric(),
                       "auc_max" = numeric(),
                       "std" = numeric(),
                       "best_round" = numeric())  
  for(r_sample in RSAMPLE)
    for(c_sample in CSAMPLE){
      xgb_cv = xgb.cv(params = xgb_params,
                      data = x,
                      label = y,
                      subsample = r_sample,
                      colsample_bytree = c_sample,
                      nrounds = ROUNDS, 
                      nfold = NFOLD,                                                   # number of folds in K-fold
                      stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                      verbose = FALSE,
                      print.every.n = 1, 
                      early.stop.round = 50,
                      scale_pos_weight = SPWEIGHT  ## rate of 0/1
      )
      
      m = xgb_cv$test.auc.mean
      std = xgb_cv$test.auc.std
      idx = order(m, decreasing = TRUE)[1:5]
      m.top5  = m[idx]
      std.top5 = std[idx]
      n.top5 = which(m %in% m.top5)
      
      print(paste(r_sample, c_sample, mean(m.top5), mean(std.top5)))
      xg_auc3[nrow(xg_auc3)+1, ] = c(optpar1$Depth, 
                                     optpar1$Min_child_weight,
                                     r_sample,
                                     c_sample,
                                     ETA,
                                     SPWEIGHT,
                                     optpar2$gamma,
                                     mean(m.top5), 
                                     mean(std.top5),
                                     mean(n.top5))
    }
  write.csv(xg_auc3, paste0("tuning/", out.name, "_3.csv"), row.names = FALSE, quote = FALSE)
}

#### Step4: Tuning scale_pos_weight
cat("Step4: Tuning scale_pos_weight\n")
if(!exists(paste0("tuning/", out.name, "_4.csv"))){
  SPWEIGHT = c(0.5, 1, 2, 3, 4)
  xg_auc1 = read.csv(paste0("tuning/", out.name, "_1.csv"))
  xg_auc2 = read.csv(paste0("tuning/", out.name, "_2.csv"))
  xg_auc3 = read.csv(paste0("tuning/", out.name, "_3.csv"))
  optpar1 = xg_auc1[which.max(xg_auc1$auc_max),]
  optpar2 = xg_auc2[which.max(xg_auc2$auc_max),]
  optpar3 = xg_auc3[which.max(xg_auc3$auc_max),]
  xgb_params = list(
    objective = "binary:logistic",    # binary classification
    eta = ETA,       # learning rate
    max_depth = optpar1$Depth,      # max tree depth
    min_child_weight = optpar1$Min_child_weight,
    subsample = optpar3$r_sample,
    colsample_bytree = optpar3$c_sample,
    gamma = optpar2$gamma,
    eval_metric = "auc"     # evaluation/auc metric
  )
  
  xg_auc4 = data.frame("Depth" = numeric(),
                       "Min_child_weight" = numeric(),
                       "r_sample" = numeric(),
                       "c_sample" = numeric(), 
                       "eta" = numeric(),
                       "scale_pos_weight" = numeric(),
                       "gamma" = numeric(),
                       "auc_max" = numeric(),
                       "std" = numeric(),
                       "best_round" = numeric())  
  for(sp_weight in SPWEIGHT){
    xgb_cv = xgb.cv(params = xgb_params,
                    data = x,
                    label = y,
                    nrounds = ROUNDS, 
                    nfold = NFOLD,                                                   # number of folds in K-fold
                    stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                    verbose = FALSE,
                    print.every.n = 1, 
                    early.stop.round = 50,
                    scale_pos_weight = sp_weight ## rate of 0/1
    )
    
    m = xgb_cv$test.auc.mean
    std = xgb_cv$test.auc.std
    idx = order(m, decreasing = TRUE)[1:5]
    m.top5  = m[idx]
    std.top5 = std[idx]
    n.top5 = which(m %in% m.top5)
    
    print(paste(sp_weight, mean(m.top5), mean(std.top5)))
    xg_auc4[nrow(xg_auc4)+1, ] = c(optpar1$Depth, 
                                   optpar1$Min_child_weight,
                                   optpar3$r_sample,
                                   optpar3$c_sample,
                                   ETA,
                                   sp_weight,
                                   optpar2$gamma,
                                   mean(m.top5), 
                                   mean(std.top5),
                                   mean(n.top5))
  }
  write.csv(xg_auc4, paste0("tuning/", out.name, "_4.csv"), row.names = FALSE, quote = FALSE)
}

#### Step5: Tuning learning rate
cat("Step5: Tuning learning rate\n")

if(!exists(paste0("tuning/", out.name, "_5.csv"))){
  
  ETA = seq(0.01, 0.025, by = 0.002)
  
  xg_auc1 = read.csv(paste0("tuning/", out.name, "_1.csv"))
  xg_auc2 = read.csv(paste0("tuning/", out.name, "_2.csv"))
  xg_auc3 = read.csv(paste0("tuning/", out.name, "_3.csv"))
  xg_auc4 = read.csv(paste0("tuning/", out.name, "_4.csv"))
  optpar1 = xg_auc1[which.max(xg_auc1$auc_max),]
  optpar2 = xg_auc2[which.max(xg_auc2$auc_max),]
  optpar3 = xg_auc3[which.max(xg_auc3$auc_max),]
  optpar4 = xg_auc4[which.max(xg_auc4$auc_max),]
  xgb_params = list(
    objective = "binary:logistic",    # binary classification
    # eta = 0.1,       # learning rate
    max_depth = optpar1$Depth,      # max tree depth
    min_child_weight = optpar1$Min_child_weight,
    subsample = optpar3$r_sample,
    colsample_bytree = optpar3$c_sample,
    gamma = optpar2$gamma,
    eval_metric = "auc"     # evaluation/auc metric
  )
  xg_auc5 = data.frame("Depth" = numeric(),
                       "Min_child_weight" = numeric(),
                       "r_sample" = numeric(),
                       "c_sample" = numeric(), 
                       "eta" = numeric(),
                       "scale_pos_weight" = numeric(),
                       "gamma" = numeric(),
                       "auc_max" = numeric(),
                       "std" = numeric(),
                       "best_round" = numeric())  
  for(eta in ETA){
    xgb_cv = xgb.cv(params = xgb_params,
                    data = x,
                    label = y,
                    eta = eta,
                    nrounds = ROUNDS, 
                    nfold = NFOLD,                                                   # number of folds in K-fold
                    stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                    verbose = FALSE,
                    print.every.n = 1, 
                    early.stop.round = 50,
                    scale_pos_weight = optpar4$scale_pos_weight ## rate of 0/1
    )
    m = xgb_cv$test.auc.mean
    std = xgb_cv$test.auc.std
    idx = order(m, decreasing = TRUE)[1:5]
    m.top5  = m[idx]
    std.top5 = std[idx]
    n.top5 = which(m %in% m.top5)
    
    print(paste(eta, mean(m.top5), mean(std.top5)))
    xg_auc5[nrow(xg_auc5)+1, ] = c(optpar1$Depth, 
                                   optpar1$Min_child_weight,
                                   optpar3$r_sample,
                                   optpar3$c_sample,
                                   eta,
                                   optpar4$scale_pos_weight,
                                   optpar2$gamma,
                                   mean(m.top5), 
                                   mean(std.top5),
                                   mean(n.top5))
  }
  write.csv(xg_auc5, paste0("tuning/", out.name, "_5.csv"), row.names = FALSE, quote = FALSE)
}


##########################################################
########### Build model with tuned parameters ############
##########################################################
# trn = readRDS("train_clean.RDS")
# tst = readRDS("test_clean.RDS")
# ftrn = read.csv("../../feature/feature_all_train.csv")
# ftst = read.csv("../../feature/feature_all_test.csv")
# trn = left_join(trn, ftrn)
# tst = left_join(tst, ftst)
# y = trn$TARGET
# trn$TARGET=NULL
# x = Matrix(as.matrix(trn[, -1]), sparse = TRUE)
#
# xg_auc1 = read.csv("tuning/xg_auc1.csv")
# xg_auc2 = read.csv("tuning/xg_auc2.csv")
# xg_auc3 = read.csv("tuning/xg_auc3.csv")
# xg_auc4 = read.csv("tuning/xg_auc4.csv")
# xg_auc5 = read.csv("tuning/xg_auc5.csv")
# optpar1 = xg_auc1[which.max(xg_auc1$auc_max),]
# optpar2 = xg_auc2[which.max(xg_auc2$auc_max),]
# optpar3 = xg_auc3[which.max(xg_auc3$auc_max),]
# optpar4 = xg_auc4[which.max(xg_auc4$auc_max),]
# optpar5 = xg_auc5[which.max(xg_auc5$auc_max),]
# xgb_params = list(
#   objective = "binary:logistic",    # binary classification
#   eta = optpar5$eta,       # learning rate
#   max_depth = optpar1$Depth,      # max tree depth
#   min_child_weight = optpar1$Min_child_weight,
#   subsample = optpar3$r_sample,
#   colsample_bytree = optpar3$c_sample,
#   gamma = optpar2$gamma,
#   eval_metric = "auc"     # evaluation/auc metric
# )

# xgbst = xgboost(params = xgb_params,
#                 data = x,
#                 label = y,
#                 nrounds = optpar5$best_round,    # max number of trees to build
#                 verbose = TRUE,
#                 print.every.n = 1,
#                 early.stop.round = 100,     # stop if no improvement within 0.1*optpar$Rounds trees
#                 scale_pos_weight = optpar4$scale_pos_weight
# )

# #### predicting test data
# x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
# y.tst.pred = predict(xgbst, x.tst)
# res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
# head(res.df)
# plot(res.df$TARGET, aa$TARGET)
# write.csv(res.df, "../../submission/sumision_xgboost0420_3.csv", row.names = FALSE, quote = FALSE)

