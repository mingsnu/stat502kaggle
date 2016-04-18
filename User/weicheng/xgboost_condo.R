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
x = Matrix(as.matrix(trn[, -c(1, ncol(trn))]), sparse = TRUE)
y = trn$TARGET

## load parameter selection file
xg_auc = read.csv("xgboost_PS1.csv")
optpar = xg_auc[which.max(xg_auc$auc_max),]
# or manully construct optpar
optpar = data.frame(Rounds = 500, Depth=6, r_sample = 0.75, c_sample=0.75, eta = 0.01)
optpar = data.frame(Rounds = 500, Depth=6, r_sample = 0.65, c_sample=0.8, eta = 0.05, 
                    scale_pos_weight = 0.8, best_round = 130)
optpar = data.frame(Rounds = 1000, Depth=7, r_sample = 0.65, c_sample=0.8, eta = 0.005, 
                    scale_pos_weight = 1, best_round = 1000)
# "1000 7 0.65 0.8 0.005 1 0.841415"
# "1000 7 0.65 0.85 0.005 1 0.84142"


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
importance <- xgb.importance(feature_names = x@Dimnames[[2]], model = xgbst)
head(importance)
# importanceRaw <- xgb.importance(feature_names = x@Dimnames[[2]], model = xgbst, data = x, label = y)
# # Cleaning for better display
# importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
# head(importanceRaw)
xgb.plot.importance(importance_matrix = importance)
xgb.plot.tree(feature_names = sparse_matrix@Dimnames[[2]], model = xgbst, n_first_tree = 2)


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
importance_matrix <- xgb.importance(model = bstSparse)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


## ## For test data
tst = readRDS("test_clean.RDS")
x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
y.tst.pred = predict(xgbst, x.tst)
res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
head(res.df)
write.csv(res.df, "../../submission/sumision_xgboost0417_1.csv", row.names = FALSE, quote = FALSE)
  
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
