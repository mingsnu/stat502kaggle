library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

# load in the training data
trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
x = Matrix(as.matrix(trn[, -c(1, ncol(trn))]), sparse = TRUE)
y = trn$TARGET

# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "binary:logistic",                                               # binary classification
  eta = 0.01,                                                                  # learning rate
  max.depth = 10,                                                              # max tree depth
  eval_metric = "auc"                                                          # evaluation/loss metric
)

# fit the model with the arbitrary parameters specified above
xgb_1 = xgboost(data = x,
                label = y,
                params = xgb_params_1,
                nrounds = 500,                                                 # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 10                                          # stop if no improvement within 10 trees
)

# cross-validate xgboost to get the accurate measure of error
xgb_cv_1 = xgb.cv(params = xgb_params_1,
                  data = x,
                  label = y,
                  nrounds = 500, 
                  nfold = 10,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 10
)

# plot the AUC for the training and testing samples
xgb_cv_1$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()


## ## For test data
## tst = readRDS("test_clean.rds")
## x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
## y.tst.pred = as.numeric(predict(bstSparse, x.tst) > 0.5)
## res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
## head(res.df)
## write.csv(res.df, "../../submission/sumision_xgboost0413.csv", row.names = FALSE, quote = FALSE)
  
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
