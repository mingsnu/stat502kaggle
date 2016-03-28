library(Matrix)
library(xgboost)
library(caTools)
trn = readRDS("../../data/train.rds")
x = Matrix(as.matrix(trn[, -c(1, ncol(trn))]), sparse = TRUE)
y = trn$TARGET

bstSparse <- xgboost(data = x, label = y, max.depth = 30, eta = 1, 
                     nthread = 4, nround = 2, objective = "binary:logistic")
y.pred <- as.numeric(predict(bstSparse, x) > 0.5)
mean(y.pred != y)
colAUC(y.pred, y) # 0.6135349

## For test data
tst = readRDS("../../data/tst.rds")
x.tst = Matrix(as.matrix(tst[, -1]), sparse = TRUE)
y.tst.pred = as.numeric(predict(bstSparse, x.tst) > 0.5)
res.df = data.frame(ID = tst$ID, TARGET = y.tst.pred)
head(res.df)
write.csv(res.df, "../../submit/sumision_xgboost0327.csv", row.names = FALSE, quote = FALSE)
