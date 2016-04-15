library(Matrix)
library(xgboost)
library(caTools)
library(caret)

trn = readRDS("train_clean.RDS")
inTraining <- createDataPartition(trn$TARGET, p = .8, list = FALSE)
training = trn[inTraining,]
testing = trn[-inTraining,]

dtrain <- xgb.DMatrix(data = Matrix(as.matrix(training[,
 -c(1, ncol(training))]), sparse = TRUE), label = training$TARGET)

dtest <- xgb.DMatrix(data = Matrix(as.matrix(testing[,
 -c(1, ncol(testing))]), sparse = TRUE), label = testing$TARGET)

watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain, max.depth=6, eta=0.01,
nround=20, watchlist=watchlist, objective = "binary:logistic", eval_metric="auc")
xgb.save(bst, "xgboost_condo1.model")
#bst <- xgb.load("xgboost_condo1.model")



## For test data
tst = readRDS("test_clean.RDS")
x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
y.tst.pred.prob = predict(bst, x.tst) 
y.tst.pred = as.numeric(predict(bst, x.tst) > 0.5)
res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
head(res.df)
write.csv(res.df, "../../submission/sumision_xgboost0414.csv", row.names = FALSE, quote = FALSE)
