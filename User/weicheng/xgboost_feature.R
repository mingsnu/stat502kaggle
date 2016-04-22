library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
source("features_weicheng.R")
f1 = f_var15_ratio(trn, tst)
f2 = f_var38_peak(trn, tst)
f3 = f_var38_ratio(trn, tst)

y = trn$TARGET
trn$TARGET = NULL
trn = trn %>% left_join(f1$trn) %>% left_join(f2$trn) %>% left_join(f3$trn)
tst = tst %>% left_join((f1$tst)) %>% left_join((f2$tst)) %>% left_join((f3$tst))
x = Matrix(as.matrix(trn[, -1]), sparse = TRUE)

xg_auc = read.csv("xgboost_PS2.csv")
tail(xg_auc[order(xg_auc$auc_max),], 20)
optpar = xg_auc[which.max(xg_auc$auc_max),]
optpar = data.frame(Rounds=1000, Depth = 5, r_sample = 0.683, c_sample = 0.7, eta =0.0203,
                    scale_pos_weight = 1, best_round = 488)
optpar = data.frame(Rounds=1000, Depth = 5, r_sample = 0.68, c_sample = 0.68, eta =0.01,
                     best_round = 769)
# 2000 5 0.7 0.7 0.01 1 0 0.84199 0.006977"
# 2000,5,0.68,0.68,0.01,1,0,0.842196,0.006927,769


## xgboost fitting with arbitrary parameters
xgb_params = list(
  objective = "binary:logistic",    # binary classification
  eta = optpar$eta,       # learning rate
  max_depth = optpar$Depth,      # max tree depth
  subsample = optpar$r_sample,
  colsample_bytree = optpar$c_sample,
  eval_metric = "auc"     # evaluation/auc metric
)

xgbst = xgboost(params = xgb_params,
                data = x,
                label = y,
                nrounds = optpar$best_round,    # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 100,     # stop if no improvement within 0.1*optpar$Rounds trees
                scale_pos_weight = optpar$scale_pos_weight
)

importance <- xgb.importance(feature_names = x@Dimnames[[2]], model = xgbst)
head(importance)
xgb.plot.importance(importance_matrix = importance[1:10])

set.seed(1024)
xgb_cv = xgb.cv(params = xgb_params,
                  data = x,
                  label = y,
                  nrounds =optpar$Rounds, 
                  nfold = 10,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 0.1*optpar$Rounds,
                  scale_pos_weight = optpar$scale_pos_weight  ## rate of 0/1
)
#  Best iteration: 585

xgb_cv$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()


## ## For test data
x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
y.tst.pred = predict(xgbst, x.tst)
res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
head(res.df)
write.csv(res.df, "../../submission/sumision_xgboost0420_2.csv", row.names = FALSE, quote = FALSE)

##########################################
## Using selected variables
# vars = importance$Feature
# saveRDS(vars, file="importantVars.RDS")
vars = readRDS("importantVars.RDS")
trn1 = trn[, vars]
tst1 = tst[, vars]
x = Matrix(as.matrix(trn1[, -1]), sparse = TRUE)

### Step 1: Tuning `max_depth` and `min_child_weight`
cat("Step1: Tuning max_depth and min_child\n")
if(!exists("xg_auc1.csv")){
  xgb_params = list(
    objective = "binary:logistic",    # binary classification
    eta = 0.1,       # learning rate
    # max_depth = 5,      # max tree depth
    # min_child_weight = 1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    gamma = 0,
    eval_metric = "auc"     # evaluation/auc metric
  )
  
  xg_auc1 = data.frame("Depth" = numeric(),
                       "Min_child_weight" = numeric(),
                       "auc_max" = numeric(),
                       "std" = numeric(),
                       "best_round" = numeric())
  for(depth in c(4, 5, 6))
    for(mcw in c(1, 2, 3)){
      xgb_cv = xgb.cv(params = xgb_params,
                      data = x,
                      label = y,
                      max_depth = depth,
                      nrounds = 2000, 
                      nfold = 10,                                                   # number of folds in K-fold
                      stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                      verbose = FALSE,
                      print.every.n = 1, 
                      early.stop.round = 50,
                      min_child_weight = mcw,
                      scale_pos_weight = 1  ## rate of 0/1
      )
      print(paste(depth, mcw, max(xgb_cv$test.auc.mean)))
      xg_auc1[nrow(xg_auc1)+1, ] = c(depth, 
                                     mcw,
                                     # max(xgb_cv$dt$test.auc.mean), 
                                     max(xgb_cv$test.auc.mean), 
                                     xgb_cv$test.auc.std[which.max(xgb_cv$test.auc.mean)],
                                     which.max(xgb_cv$test.auc.mean)) 
    }
  write.csv(xg_auc1, "tuning/xg_auc1.csv", row.names = FALSE, quote = FALSE)
}


### Step 2: Tuning `gamma`
cat("Step2: Tuning gamma\n")
if(!exists("tuning/xg_auc2.csv")){
  xg_auc1 = read.csv("tuning/xg_auc1.csv")
  optpar1 = xg_auc1[which.max(xg_auc1$auc_max),]
  xgb_params = list(
    objective = "binary:logistic",    # binary classification
    eta = 0.1,       # learning rate
    max_depth = optpar1$Depth,      # max tree depth
    min_child_weight = optpar1$Min_child_weight,
    subsample = 0.8,
    colsample_bytree = 0.8,
    # gamma = 0,
    eval_metric = "auc"     # evaluation/auc metric
  )
  
  xg_auc2 = data.frame("gamma" = numeric(),
                       "auc_max" = numeric(),
                       "std" = numeric(),
                       "best_round" = numeric())
  for(gamma in seq(0, 1, by=0.2)){
    xgb_cv = xgb.cv(params = xgb_params,
                    data = x,
                    label = y,
                    gamma = gamma,
                    nrounds = 2000, 
                    nfold = 10,                                                   # number of folds in K-fold
                    stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                    verbose = FALSE,
                    print.every.n = 1, 
                    early.stop.round = 50,
                    scale_pos_weight = 1  ## rate of 0/1
    )
    print(paste(gamma, max(xgb_cv$test.auc.mean)))
    xg_auc2[nrow(xg_auc2)+1, ] = c(gamma,
                                   max(xgb_cv$test.auc.mean), 
                                   xgb_cv$test.auc.std[which.max(xgb_cv$test.auc.mean)],
                                   which.max(xgb_cv$test.auc.mean)) 
  }
  write.csv(xg_auc2, "tuning/xg_auc2.csv", row.names = FALSE, quote = FALSE)
}

#### Step3: Tuning `subsample` and `colsample_bytree`
cat("Step3: Tuning `subsample` and `colsample_bytree`\n")
if(!exists("tuning/xg_auc3.csv")){
  xg_auc1 = read.csv("tuning/xg_auc1.csv")
  xg_auc2 = read.csv("tuning/xg_auc2.csv")
  optpar1 = xg_auc1[which.max(xg_auc1$auc_max),]
  optpar2 = xg_auc2[which.max(xg_auc2$auc_max),]
  xgb_params = list(
    objective = "binary:logistic",    # binary classification
    eta = 0.1,       # learning rate
    max_depth = optpar1$Depth,      # max tree depth
    min_child_weight = optpar1$Min_child_weight,
    # subsample = 0.8,
    # colsample_bytree = 0.8,
    gamma = optpar2$gamma,
    eval_metric = "auc"     # evaluation/auc metric
  )
  
  xg_auc3 = data.frame("r_sample" = numeric(),
                       "c_sample" = numeric(), 
                       "auc_max" = numeric(),
                       "std" = numeric(),
                       "best_round" = numeric())
  for(r_sample in c(0.5, 0.6, 0.7, 0.8))
    for(c_sample in c(0.5, 0.6, 0.7, 0.8)){
      xgb_cv = xgb.cv(params = xgb_params,
                      data = x,
                      label = y,
                      subsample = r_sample,
                      colsample_bytree = c_sample,
                      nrounds = 2000, 
                      nfold = 10,                                                   # number of folds in K-fold
                      stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                      verbose = FALSE,
                      print.every.n = 1, 
                      early.stop.round = 50,
                      scale_pos_weight = 1  ## rate of 0/1
      )
      print(paste(r_sample, c_sample, max(xgb_cv$test.auc.mean)))
      xg_auc3[nrow(xg_auc3)+1, ] = c(r_sample, c_sample,
                                     max(xgb_cv$test.auc.mean), 
                                     xgb_cv$test.auc.std[which.max(xgb_cv$test.auc.mean)],
                                     which.max(xgb_cv$test.auc.mean)) 
    }
  write.csv(xg_auc3, "tuning/xg_auc3.csv", row.names = FALSE, quote = FALSE)
}

#### Step4: Tuning scale_pos_weight
cat("Step4: Tuning scale_pos_weight\n")
if(!exists("xg_auc4.csv")){
  xg_auc1 = read.csv("tuning/xg_auc1.csv")
  xg_auc2 = read.csv("tuning/xg_auc2.csv")
  xg_auc3 = read.csv("tuning/xg_auc3.csv")
  optpar1 = xg_auc1[which.max(xg_auc1$auc_max),]
  optpar2 = xg_auc2[which.max(xg_auc2$auc_max),]
  optpar3 = xg_auc3[which.max(xg_auc3$auc_max),]
  xgb_params = list(
    objective = "binary:logistic",    # binary classification
    eta = 0.1,       # learning rate
    max_depth = optpar1$Depth,      # max tree depth
    min_child_weight = optpar1$Min_child_weight,
    subsample = optpar3$r_sample,
    colsample_bytree = optpar3$c_sample,
    gamma = optpar2$gamma,
    eval_metric = "auc"     # evaluation/auc metric
  )
  
  xg_auc4 = data.frame("scale_pos_weight"= numeric(),
                       "auc_max" = numeric(),
                       "std" = numeric(),
                       "best_round" = numeric())
    for(sp_weight in c(1, 2, 3, 4)){
      xgb_cv = xgb.cv(params = xgb_params,
                      data = x,
                      label = y,
                      nrounds = 2000, 
                      nfold = 10,                                                   # number of folds in K-fold
                      stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                      verbose = FALSE,
                      print.every.n = 1, 
                      early.stop.round = 50,
                      scale_pos_weight = sp_weight ## rate of 0/1
      )
      print(paste(sp_weight, max(xgb_cv$test.auc.mean)))
      xg_auc4[nrow(xg_auc4)+1, ] = c(sp_weight,
                                     # max(xgb_cv$dt$test.auc.mean), 
                                     max(xgb_cv$test.auc.mean), 
                                     xgb_cv$test.auc.std[which.max(xgb_cv$test.auc.mean)],
                                     which.max(xgb_cv$test.auc.mean)) 
    }
  write.csv(xg_auc4, "tuning/xg_auc4.csv", row.names = FALSE, quote = FALSE)
}

#### Step5: Tuning learning rate
cat("Step5: Tuning learning rate\n")
if(!exists("xg_auc5.csv")){
  xg_auc1 = read.csv("tuning/xg_auc1.csv")
  xg_auc2 = read.csv("tuning/xg_auc2.csv")
  xg_auc3 = read.csv("tuning/xg_auc3.csv")
  xg_auc4 = read.csv("tuning/xg_auc4.csv")
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
  
  xg_auc5 = data.frame("eta"= numeric(),
                       "auc_max" = numeric(),
                       "std" = numeric(),
                       "best_round" = numeric())
  for(eta in c(0.01, 0.015, 0.02, 0.025, 0.03)){
    xgb_cv = xgb.cv(params = xgb_params,
                    data = x,
                    label = y,
                    eta = eta,
                    nrounds = 2000, 
                    nfold = 10,                                                   # number of folds in K-fold
                    stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                    verbose = FALSE,
                    print.every.n = 1, 
                    early.stop.round = 50,
                    scale_pos_weight = optpar4$scale_pos_weight ## rate of 0/1
    )
    print(paste(eta, max(xgb_cv$test.auc.mean)))
    xg_auc5[nrow(xg_auc5)+1, ] = c(eta,
                                   # max(xgb_cv$dt$test.auc.mean), 
                                   max(xgb_cv$test.auc.mean), 
                                   xgb_cv$test.auc.std[which.max(xgb_cv$test.auc.mean)],
                                   which.max(xgb_cv$test.auc.mean)) 
  }
  write.csv(xg_auc5, "tuning/xg_auc5.csv", row.names = FALSE, quote = FALSE)
}



#########################
xg_auc1 = read.csv("tuning/xg_auc1.csv")
xg_auc2 = read.csv("tuning/xg_auc2.csv")
xg_auc3 = read.csv("tuning/xg_auc3.csv")
xg_auc4 = read.csv("tuning/xg_auc4.csv")
xg_auc5 = read.csv("tuning/xg_auc5.csv")
optpar1 = xg_auc1[which.max(xg_auc1$auc_max),]
optpar2 = xg_auc2[which.max(xg_auc2$auc_max),]
optpar3 = xg_auc3[which.max(xg_auc3$auc_max),]
optpar4 = xg_auc4[which.max(xg_auc4$auc_max),]
optpar5 = xg_auc5[which.max(xg_auc5$auc_max),]
xgb_params = list(
  objective = "binary:logistic",    # binary classification
  eta = optpar5$eta,       # learning rate
  max_depth = optpar1$Depth,      # max tree depth
  min_child_weight = optpar1$Min_child_weight,
  subsample = optpar3$r_sample,
  colsample_bytree = optpar3$c_sample,
  gamma = optpar2$gamma,
  eval_metric = "auc"     # evaluation/auc metric
)
xgbst = xgboost(params = xgb_params,
                data = x,
                label = y,
                nrounds = optpar5$best_round,    # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 100,     # stop if no improvement within 0.1*optpar$Rounds trees
                scale_pos_weight = optpar4$scale_pos_weight
)


## ## For test data
x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
y.tst.pred = predict(xgbst, x.tst)
res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
head(res.df)
plot(res.df$TARGET, aa$TARGET)
write.csv(res.df, "../../submission/sumision_xgboost0420_3.csv", row.names = FALSE, quote = FALSE)
