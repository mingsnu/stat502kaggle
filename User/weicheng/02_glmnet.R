library(Matrix)
library(glmnet)
library(caTools)
trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")

na.idx = apply(trn[,-ncol(trn)], 2, function(x) any(is.na(x)))
trn = trn[, !na.idx]
tst = tst[, !na.idx]

x = Matrix(as.matrix(trn[, -c(1, ncol(trn))]), sparse = TRUE)
y = as.factor(trn$TARGET)
fit = glmnet(x, y, family = "binomial", alpha = 0.45)
plot(fit)
plot(fit, xvar = "dev", label = TRUE)


y.tst.pred = predict(fit, newx=Matrix(as.matrix(tst[, -1]), sparse = TRUE), type="class", s=6.490e-06)
res.df = data.frame(ID = tst$ID, TARGET = as.numeric(y.tst.pred))
head(res.df)
write.csv(res.df, "../../submission/sumision_glmnet0413.csv", row.names = FALSE, quote = FALSE)


cvfit = cv.glmnet(x, y, family = "binomial", alpha = 0.45, type.measure = "auc")
y.tst.pred = predict(cvfit, newx=Matrix(as.matrix(tst[, -1]), sparse = TRUE), s = "lambda.min",  type="response")
sum(as.numeric(y.tst.pred>0.1))
res.df = data.frame(ID = tst$ID, TARGET =as.numeric(y.tst.pred>0.1))
head(res.df)
write.csv(res.df, "../../submission/sumision_glmnet0413.csv", row.names = FALSE, quote = FALSE)
