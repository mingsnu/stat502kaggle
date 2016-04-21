library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

## load in the training/testing data
trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
# trn[is.na(trn)] = -999
# tst[is.na(tst)] = -999
# 
# ##### Extracting TARGET
# y <- trn$TARGET
# trn$TARGET <- NULL
# 
# ##### 0 count per line
# count0 <- function(x) {
#   return( sum(x == 0) )
# }
# trn$n0 <- apply(trn, 1, FUN=count0)
# tst$n0 <- apply(tst, 1, FUN=count0)

# trn$TARGET = y
# saveRDS(trn, "train_clean.RDS")
# saveRDS(tst, "test_clean.RDS")
x = Matrix(as.matrix(trn[, -c(1, ncol(trn))]), sparse = TRUE)
y = trn$TARGET

# trn = trn[,-grep("^ind",names(trn))]
## load parameter selection file
xg_auc = read.csv("xgboost_PS2.csv")
optpar = xg_auc[which.max(xg_auc$auc_max),]
# or manully construct optpar
optpar = data.frame(Rounds = 500, Depth=6, r_sample = 0.75, c_sample=0.75, eta = 0.01)
optpar = data.frame(Rounds = 500, Depth=6, r_sample = 0.65, c_sample=0.8, eta = 0.05, 
                    scale_pos_weight = 0.8, best_round = 130)
optpar = data.frame(Rounds = 1000, Depth=5, r_sample = 0.75, c_sample=0.7, eta = 0.01, 
                    scale_pos_weight = 2, best_round = 725)
optpar = data.frame(Rounds=1000, Depth = 5, r_sample = 0.683, c_sample = 0.7, eta =0.0203,
                    scale_pos_weight = 1, best_round = 384)

# "1000 7 0.65 0.8 0.005 1 0.841415"
# "1000 7 0.65 0.85 0.005 1 0.84142"
# 1000	5	0.75	0.7	0.01	2	0.5	0.842248	725


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
                early.stop.round = 0.1*optpar$Rounds,     # stop if no improvement within 0.1*optpar$Rounds trees
                scale_pos_weight = optpar$scale_pos_weight
)

y.pred <- predict(xgbst, x)
colAUC(y.pred, y) # 0.8858437

# aa = read.csv("submission.csv")
# head(aa)
# aa$TARGET[tst$var15<22] = 0
# colAUC(y.pred, y)

importance <- xgb.importance(feature_names = x@Dimnames[[2]], model = xgbst)
head(importance)
# importanceRaw <- xgb.importance(feature_names = x@Dimnames[[2]], model = xgbst, data = x, label = y)
# # Cleaning for better display
# importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
# head(importanceRaw)
xgb.plot.importance(importance_matrix = importance[1:100])
#xgb.plot.tree(feature_names = sparse_matrix@Dimnames[[2]], model = xgbst, n_first_tree = 2)
#write.csv(importance, "importance.csv", row.names=FALSE, quote=FALSE)

# cross-validate xgboost to get the accurate measure of error
set.seed(1024)
xgb_cv_1 = xgb.cv(params = xgb_params,
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

# plot the AUC for the training and testing samples
# if predict is TRUE, should use $dt to extract the mean and sd
# otherwise use it directly.
xgb_cv_1$dt %>%
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
write.csv(res.df, "../../submission/sumision_xgboost0420.csv", row.names = FALSE, quote = FALSE)

## inTraining <- createDataPartition(trn$TARGET, p = .75, list = FALSE)
## training = trn[inTraining,]
## testing = trn[-inTraining,]

## dtrain <- xgb.DMatrix(data = Matrix(as.matrix(training[,
##  -c(1, ncol(training))]), sparse = TRUE), label = training$TARGET)

## dtest <- xgb.DMatrix(data = Matrix(as.matrix(testing[,
##  -c(1, ncol(testing))]), sparse = TRUE), label = testing$TARGET)

## watchlist <- list(train=dtrain, test=dtest)
## bst <- xgb.train(data=dtrain, max.depth=10, eta=0.3, nthread = 4,
## nround=20, watchlist=watchlist, objective = "binary:logistic", eval_metric="auc")

