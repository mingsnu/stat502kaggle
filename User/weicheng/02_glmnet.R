library(Matrix)
library(glmnet)
library(caTools)
trn = readRDS("../../data/train.rds")

## logistic lasso model
x = Matrix(as.matrix(trn[, -c(1, ncol(trn))]), sparse = TRUE)
y = as.factor(trn$TARGET)
fit = glmnet(x, y, family = "binomial")
plot(fit)
plot(fit, xvar = "dev", label = TRUE)
y.pred = predict(fit, newx=x, type="class", s=2.724e-05)
head(y.pred)
head(y)
## Use `colAUC` function to calculate the AUC
colAUC(as.numeric(y.pred), y)

## logistic lasso with randomly sampled x
head(x)
