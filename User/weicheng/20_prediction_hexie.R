library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(dplyr)
library(data.table)

### Loading data & features
trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
ftrn = read.csv("../../feature/feature_all_train_ratio_only_wc_99.csv")
ftst = read.csv("../../feature/feature_all_test_ratio_only_wc_99.csv")
trn = left_join(trn, ftrn)
tst = left_join(tst, ftst)
trn = data.table(trn)
tst = data.table(tst)

trn.y = trn$TARGET
hexie = read.csv("tuning/hexiehao_R10.csv")
hexie1 = read.csv("tuning/hexiehao_no_ratio_R10.csv")
hexie1$ftrSelected %in% hexie$ftrSelected

ftrSelected = hexie$ftrSelected[1:which.max(hexie$aucMean)]
# varSelected = hexie1$ftrSelected[1:which.max(hexie1$aucMean)]
ftr.idx = which(names(trn) %in% ftrSelected)

training = trn[, ftr.idx, with=FALSE]
testing = tst[, ftr.idx, with=FALSE]

dtrain <- xgb.DMatrix(data=Matrix(as.matrix(training), sparse = TRUE), label=trn.y, missing = -999)

optpar = data.frame(Rounds=2000, Depth = 4, r_sample = 0.8, eta =0.01)

print("Train xgboost using xgb.train with watchlist")
params = list(
  objective = "binary:logistic",
  eta = optpar$eta, 
  max_depth = optpar$Depth, 
  subsample = optpar$r_sample,
  eval_metric = "auc")

bst.cv <- xgb.cv(params = params,
                 data=dtrain,
                 nfold = 4,
                 nrounds = 2000,
                 verbose = 1,
                 early.stop.round = 20)



## ## For test data
x.tst = Matrix(as.matrix(testing), sparse = TRUE)
pred = NULL
for(i in 1:10){
  bst <- xgboost(params = params,
                 data=dtrain,
                 nrounds = 818,
                 verbose = 0)
  y.tst.pred = predict(bst, x.tst)
  pred = cbind(pred, y.tst.pred)
}


res.df = data.frame(ID = tst$ID, TARGET = apply(pred, 1, mean))
res.df$ID = as.integer(res.df$ID)
head(res.df)
# aa = read.csv("submission.csv")
# plot(res.df$TARGET, aa$TARGET)
write.csv(res.df, "../../submission/sumision_xgboost0429.csv", row.names = FALSE, quote = FALSE)
