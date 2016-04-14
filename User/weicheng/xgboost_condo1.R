library(Matrix)
library(xgboost)
library(caTools)
library(caret)

trn = readRDS("train_clean.RDS")
inTraining <- createDataPartition(trn$TARGET, p = .75, list = FALSE)
training = trn[inTraining,]
testing = trn[-inTraining,]

dtrain <- xgb.DMatrix(data = Matrix(as.matrix(training[,
 -c(1, ncol(training))]), sparse = TRUE), label = training$TARGET)

dtest <- xgb.DMatrix(data = Matrix(as.matrix(testing[,
 -c(1, ncol(testing))]), sparse = TRUE), label = testing$TARGET)

watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain, max.depth=10, eta=0.3, nthread = 4,
nround=20, watchlist=watchlist, objective = "binary:logistic", eval_metric="auc")
xgb.save(bst, "xgboost_condo1.model")
#bst <- xgb.load("xgboost_condo1.model")
