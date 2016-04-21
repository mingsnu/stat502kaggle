library(xgboost)
library(Matrix)

set.seed(1234)

train <- read.csv("../../data/train.csv")
test  <- read.csv("../../data/test.csv")

##### Removing IDs
train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

##### Extracting TARGET
train.y <- train$TARGET
train$TARGET <- NULL

##### 0 count per line
count0 <- function(x) {
    return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
    if (length(unique(train[[f]])) == 1) {
        cat(f, "is constant in train. We delete it.\n")
        train[[f]] <- NULL
        test[[f]] <- NULL
    }
}

##### Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
    f1 <- pair[1]
    f2 <- pair[2]
    
    if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
        if (all(train[[f1]] == train[[f2]])) {
            cat(f1, "and", f2, "are equals.\n")
            toRemove <- c(toRemove, f2)
        }
    }
}

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names]
test <- test[, feature.names]
# saveRDS(train, "train_zft.RDS")
# saveRDS(test, "test_zft.RDS")
train$TARGET <- train.y


train <- sparse.model.matrix(TARGET ~ ., data = train)

dtrain <- xgb.DMatrix(data=train, label=train.y)
watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.0203,
                max_depth           = 5,
                subsample           = 0.683,
                colsample_bytree    = 0.7
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 574, 
                    verbose             = 2,
                    watchlist           = watchlist,
                    maximize            = FALSE
)


clf.cv <- xgb.cv(params = param,
                 data = dtrain,
                 nrounds = 1000, 
                 nfold = 10,                                                   # number of folds in K-fold
                 prediction = TRUE,                                           # return the prediction using the final model 
                 showsd = TRUE,                                               # standard deviation of loss across folds
                 stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                 verbose = TRUE,
                 print.every.n = 1, 
                 early.stop.round = 100
)

test$TARGET <- -1
test <- sparse.model.matrix(TARGET ~ ., data = test)

preds <- predict(clf, test)
submission <- data.frame(ID=test.id, TARGET=preds)
cat("saving the submission file\n")
## write.csv(submission, "../../submission/submission.csv", row.names = F)


importance <- xgb.importance(feature_names = train@Dimnames[[2]], model = clf)
xgb.plot.importance(importance_matrix = importance[1:100])
write.csv(importance, "importance.csv", row.names=FALSE, quote=FALSE)

aa = read.csv("submission.csv")
head(aa)
plot(aa$TARGET, submission$TARGET)
