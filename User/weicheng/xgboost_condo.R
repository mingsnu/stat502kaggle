library(Matrix)
library(xgboost)
library(caTools)
library(caret)

trn = readRDS("train_clean.RDS")
x = Matrix(as.matrix(trn[, -c(1, ncol(trn))]), sparse = TRUE)
y = trn$TARGET

# history <- xgb.cv(data = x, label = y, nround=3, nthread = 6, nfold = 10, metrics=list("auc"),
#                   max.depth = 8, eta = 1, objective = "binary:logistic")

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


xgTreeGrid <- expand.grid(nrounds=seq(10, 100, by = 10),
                          max_depth = seq(1, 20, by = 2),
                          eta = seq(0.1, .5, by = 0.05),
                          colsample_bytree = seq(.1,1,by=.2),
                          min_child_weight=1,
                          gamma=0
                          )

nrow(xgTreeGrid)

set.seed(825)
gbmFit2 <- train(TARGET ~ ., data = trn,
                 method = "xgbTree",
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid = xgTreeGrid, nthread = 16, 
                 objective = "binary:logistic", eval_metric="auc" )
gbmFit2



## bstSparse <- xgboost(data = x, label = y, max.depth = 8, eta = 1, 
##                      nthread = 6, nround = 3, objective = "binary:logistic")
## y.pred <- as.numeric(predict(bstSparse, x) > 0.5)
## mean(y.pred != y)
## colAUC(y.pred, y) # 0.6135349

## importance_matrix <- xgb.importance(model = bstSparse)
## print(importance_matrix)
## xgb.plot.importance(importance_matrix = importance_matrix)


## ## For test data
## tst = readRDS("test_clean.rds")
## x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
## y.tst.pred = as.numeric(predict(bstSparse, x.tst) > 0.5)
## res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
## head(res.df)
## write.csv(res.df, "../../submission/sumision_xgboost0413.csv", row.names = FALSE, quote = FALSE)
  
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
