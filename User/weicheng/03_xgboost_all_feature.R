
#########################
trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
ftrn = read.csv("../../feature/feature_all_train.csv")
ftst = read.csv("../../feature/feature_all_test.csv")
trn = left_join(trn, ftrn)
tst = left_join(tst, ftst)

y = trn$TARGET
trn$TARGET=NULL
x = Matrix(as.matrix(trn[, -1]), sparse = TRUE)

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

importance <- xgb.importance(feature_names = x@Dimnames[[2]], model = xgbst)
head(importance$Feature,10)
# [1] "saldo_var30"               "new_var15_zl"              "f_diff_var_15_var_36_rank"
# [4] "var15_ratio"               "var15"                     "ind_comb_rank"            
# [7] "var38"                     "var38_ratio"               "f_diff_var36_38"          
# [10] "X"                        
xgb.plot.importance(importance_matrix = importance[1:100])

### using first few important feature
trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
ftrn = read.csv("../../feature/feature_all_train.csv")
ftst = read.csv("../../feature/feature_all_test.csv")
# trn = left_join(trn, ftrn %>% select(ID, new_var15_zl, f_diff_var_15_var_36_rank,
#                                      var15_ratio, ind_comb_rank, var38_ratio, f_diff_var36_38,
#                                      X))
# tst = left_join(tst, ftst  %>% select(ID, new_var15_zl, f_diff_var_15_var_36_rank,
#                                       var15_ratio, ind_comb_rank, var38_ratio, f_diff_var36_38,
#                                       X))
trn = left_join(trn, ftrn %>% select(ID, new_var15_zl, f_diff_var_15_var_36_rank,
                                      f_diff_var36_38))
tst = left_join(tst, ftst  %>% select(ID, new_var15_zl, f_diff_var_15_var_36_rank,
                                      f_diff_var36_38))
y = trn$TARGET
trn$TARGET=NULL
x = Matrix(as.matrix(trn[, -1]), sparse = TRUE)

optpar = data.frame(Rounds=2000, Depth = 5, r_sample = 0.68, c_sample = 0.68, eta =0.01,
                    best_round = 769)
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
                early.stop.round = 0.1*optpar$Rounds
)


## ## For test data
x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
y.tst.pred = predict(xgbst, x.tst)
res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
res.df$ID = as.integer(res.df$ID)
head(res.df)
# plot(res.df$TARGET, aa$TARGET)
write.csv(res.df, "../../submission/sumision_xgboost0426_2.csv", row.names = FALSE, quote = FALSE)
