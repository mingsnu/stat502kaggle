library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")

ftrn = read.csv("../../feature/feature_all_train_ratio_only_wc_99.csv")
ftst = read.csv("../../feature/feature_all_test_ratio_only_wc_99.csv")
trn = left_join(trn, ftrn)
tst = left_join(tst, ftst)
y = trn$TARGET
trn$TARGET=NULL
x = Matrix(as.matrix(trn[, -1]), sparse = TRUE)

xg_auc1 = read.csv("tuning/new_all_feature99_nfold4_round2/xg_auc1.csv")
xg_auc2 = read.csv("tuning/new_all_feature99_nfold4_round2/xg_auc2.csv")
xg_auc3 = read.csv("tuning/new_all_feature99_nfold4_round2/xg_auc3.csv")
xg_auc4 = read.csv("tuning/new_all_feature99_nfold4_round2/xg_auc4.csv")
xg_auc5 = read.csv("tuning/new_all_feature99_nfold4_round2/xg_auc5.csv")
optpar1 = xg_auc1[which.max(xg_auc1$auc_max),]
optpar2 = xg_auc2[which.max(xg_auc2$auc_max),]
optpar3 = xg_auc3[which.max(xg_auc3$auc_max),]
optpar4 = xg_auc4[which.max(xg_auc4$auc_max),]
optpar5 = xg_auc5[which.max(xg_auc5$auc_max),]
optpar1; optpar2; optpar3; optpar4; optpar5
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

importance <- xgb.importance(feature_names = x@Dimnames[[2]], model = xgbst)
impVars = head(importance$Feature, 10)
# [1] "var15_ratio"                           "saldo_var30"                           "var15"                                
# [4] "abs_f_ratio_num_var30_saldo_var30"     "f_diff_var_15_var_36_cutpoint_smaller" "ind_comb_rank"                        
# [7] "f_diff_var_15_var_36_rank"             "var38"                                 "f_diff_var_15_var_38"                 
# [10] "var38_ratio"             
xgb.plot.importance(importance_matrix = importance[1:100])


### get rid of the first few important features
while(1){
  trn = trn[, !names(trn) %in% impVars]
  tst = tst[, !names(tst) %in% impVars]
  
  x = Matrix(as.matrix(trn[, -1]), sparse = TRUE)
  dtrain <- xgb.DMatrix(data=x, label=y, missing = -999)
  xgbst = xgboost(params = xgb_params,
                  data = x,
                  label = y,
                  nrounds = optpar5$best_round,    # max number of trees to build
                  verbose = TRUE,                                         
                  print.every.n = 1,
                  early.stop.round = 50,     # stop if no improvement within 0.1*optpar$Rounds trees
                  scale_pos_weight = optpar4$scale_pos_weight
  )
  
  importance <- xgb.importance(feature_names = x@Dimnames[[2]], model = xgbst)
  head(importance, 10)
  #xgb.plot.importance(importance_matrix = importance[1:20])
  if(max(importance$Gain) < 0.02)
    break
  impVars = c(impVars, importance$Feature[importance$Gain >=0.02])
}
saveRDS(impVars, "impVars.rds")
write.csv(data.frame(impVars=impVars), "impVars.csv", row.names = FALSE)
