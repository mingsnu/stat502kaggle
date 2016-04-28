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
head(importance$Feature, 20)
# [1] "var15"                                 "saldo_var30"                           "var15_ratio"                          
# [4] "f_diff_var_15_var_36_cutpoint_smaller" "abs_f_ratio_num_var30_saldo_var30"     "ind_comb_rank"                        
# [7] "f_diff_var_15_var_38"                  "f_diff_var_15_var_36_rank"             "var38_ratio"                          
# [10] "var38"
# [1] "var15_ratio"                           "var15"                                 "saldo_var30"                          
# [4] "abs_f_ratio_num_var30_saldo_var30"     "f_diff_var_15_var_36_cutpoint_smaller" "ind_comb_rank"                        
# [7] "f_diff_var_15_var_36_rank"             "var38"                                 "var38_ratio"                          
# [10] "f_diff_var_15_var_38"    
# [1] "var15_ratio"                           "saldo_var30"                           "var15"                                
# [4] "f_diff_var_15_var_36_cutpoint_smaller" "abs_f_ratio_num_var30_saldo_var30"     "f_diff_var_15_var_36_rank"            
# [7] "ind_comb_rank"                         "f_diff_var_15_var_38"                  "var38_ratio"                          
# [10] "num_of_used"        
xgb.plot.importance(importance_matrix = importance[1:100])
# write.csv(importance, "importance.csv", row.names = FALSE)

### using first few important feature
trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
ftrn = read.csv("../../feature/feature_all_train_ratio_only_wc_99.csv")
ftst = read.csv("../../feature/feature_all_test_ratio_only_wc_99.csv")

trn = left_join(trn %>% select(-var15), ftrn %>% select(ID, f_diff_var_15_var_36_cutpoint_smaller,
                                     ind_comb_rank, f_diff_var_15_var_38))
tst = left_join(tst %>% select(-var15), ftst  %>% select(ID, f_diff_var_15_var_36_cutpoint_smaller,
                                      ind_comb_rank, f_diff_var_15_var_38))
y = trn$TARGET
trn$TARGET=NULL
x = Matrix(as.matrix(trn[, -1]), sparse = TRUE)
dtrain <- xgb.DMatrix(data=x, label=y, missing = -999)

optpar = data.frame(Rounds=2000, Depth = 4, r_sample = 0.68, c_sample = 0.68, eta =0.01,
                    best_round = 1040)
optpar = data.frame(Rounds=2000, Depth = 5, r_sample = 0.68, c_sample = 0.68, eta =0.01,
                    best_round = 774 )
xgb_params = list(
  objective = "binary:logistic",    # binary classification
  eta = optpar$eta,       # learning rate
  max_depth = optpar$Depth,      # max tree depth
  subsample = optpar$r_sample,
  colsample_bytree = optpar$c_sample,
  eval_metric = "auc"     # evaluation/auc metric
)

xgb_cv = xgb.cv(params = xgb_params,
                data = dtrain,
                nrounds = 2000, 
                nfold = 4,                                                   # number of folds in K-fold
                prediction = TRUE,                                           # return the prediction using the final model 
                showsd = TRUE,                                               # standard deviation of loss across folds
                stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                verbose = TRUE,
                print.every.n = 1, 
                early.stop.round = 50
)

xgbst = xgb.train(params = xgb_params,
                data = dtrain,
                nrounds = optpar$best_round,
                watchlist = list(trn = dtrain),
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 30
)

## ## For test data
x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
y.tst.pred = predict(xgbst, x.tst)
res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
res.df$ID = as.integer(res.df$ID)
head(res.df)
# plot(res.df$TARGET, aa$TARGET)
write.csv(res.df, "../../submission/sumision_xgboost0427_1.csv", row.names = FALSE, quote = FALSE)
