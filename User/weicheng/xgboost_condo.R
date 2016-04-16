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

# load parameter selection file
xg_auc = read.csv("xgboost_PS.csv")
optpar = xg_auc[which.max(xg_auc$auc_max),]
# or manully construct optpar
optpar = data.frame(Rounds = 500, Depth=6, r_sample = 0.75, c_sample=0.75, eat = 0.01)

# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "binary:logistic",    # binary classification
  eta = optpar$eta,       # learning rate
  max.depth = optpar$Depth,      # max tree depth
  r.sample = optpar$r_sample,
  c.sample = optpar$c_sample,
  eval_metric = "auc"     # evaluation/auc metric
)

# fit the model with parameters specified above
xgb_1 = xgboost(data = x,
                label = y,
                params = xgb_params_1,
                nrounds = optpar$Rounds,    # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 0.1*optpar$Rounds     # stop if no improvement within 0.1*optpar$Rounds trees
)

# cross-validate xgboost to get the accurate measure of error
xgb_cv_1 = xgb.cv(params = xgb_params_1,
                  data = x,
                  label = y,
                  nrounds =560, 
                  nfold = 10,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 0.1*optpar$Rounds,
                  max_delta_step = 3
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
tst = readRDS("test_clean.RDS")
x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
y.tst.pred = predict(xgb_1, x.tst)
res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
head(res.df)
write.csv(res.df, "../../submission/sumision_xgboost0416.csv", row.names = FALSE, quote = FALSE)
  
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
