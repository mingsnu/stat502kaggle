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


## available methods: http://topepo.github.io/caret/modelList.html
## Here to illustrate, we will fit a boosted tree model via the gbm package.
gbmFit1 <- train(TARGET ~ ., data = trn,
                 method = "xgbTree",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE, nthread = 6, objective = "binary:logistic",eval_metric="auc")
gbmFit1


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
                 verbose = TRUE,
                 tuneGrid = xgTreeGrid, nthread = 6, 
                 objective = "binary:logistic", eval_metric="auc" )
gbmFit2



bstSparse <- xgboost(data = x, label = y, max.depth = 20, eta = 0.3, 
                     nthread = 6, nround = 10, objective = "binary:logistic")
y.pred <- as.numeric(predict(bstSparse, x) > 0.5)
mean(y.pred != y)
colAUC(y.pred, y) # 0.6135349

importance_matrix <- xgb.importance(model = bstSparse)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


## For test data
tst = readRDS("test_clean.rds")
x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
y.tst.pred = as.numeric(predict(bstSparse, x.tst) > 0.5)
res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
head(res.df)
write.csv(res.df, "../../submission/sumision_xgboost0413.csv", row.names = FALSE, quote = FALSE)

