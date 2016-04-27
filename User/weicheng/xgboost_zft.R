
##### Removing IDs
trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
# trn[is.na(trn)] = -999
# tst[is.na(tst)] = -999

ftrn = read.csv("../../feature/feature_all_train_ratio_only_wc_99.csv")
ftst = read.csv("../../feature/feature_all_test_ratio_only_wc_99.csv")
trn = left_join(trn, ftrn %>% select(ID, ind_comb_rank))
tst = left_join(tst, ftst %>% select(ID, ind_comb_rank))

##### Removing IDs
trn$ID <- NULL
tst.id <- tst$ID
tst$ID <- NULL

##### Extracting TARGET
trn.y <- trn$TARGET
trn$TARGET <- NULL

# ##### 0 count per line
# count0 <- function(x) {
#   return( sum(x == 0) )
# }
# trn$n0 <- apply(trn, 1, FUN=count0)
# tst$n0 <- apply(tst, 1, FUN=count0)


# trn$TARGET <- trn.y
# trn <- sparse.model.matrix(TARGET ~ ., data = trn)

trn = Matrix(as.matrix(trn))

dtrain <- xgb.DMatrix(data=trn, label=trn.y, missing = -999)
watchlist <- list(trn=dtrain)
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.0203,
                max_depth           = 5,
                subsample           = 0.683,
                colsample_bytree    = 0.7
)

clf <- xgboost(params = param,
               data = dtrain,
               nrounds = 500,    # max number of trees to build
               verbose = TRUE,                                         
               print.every.n = 1,
               early.stop.round = 30
)

clf1 <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 357, 
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

clf.cv$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()


# tst$TARGET <- -1
# tst <- sparse.model.matrix(TARGET ~ ., data = tst)
tst = Matrix(as.matrix(tst))

preds <- predict(clf, tst)
submission <- data.frame(ID=test.id, TARGET=preds)

preds1 <- predict(clf1, tst)
submission <- data.frame(ID=test.id, TARGET=preds)
aa = read.csv("submission.csv")
plot(aa$TARGET, preds)



write.csv(submission, "submission.csv", row.names = F)

####################### compare
inTraining <- createDataPartition(trn$TARGET, p = .8, list = FALSE)
training = trn[inTraining,]
testing = Matrix(as.matrix(trn[-inTraining,]))
p1 = predict(clf, testing)
p2 = predict(xgbst, testing)
plot(p1, p2)
colAUC(p1, testing[,ncol(testing)]) # 0.8858437
colAUC(p2, testing[,ncol(testing)]) # 0.8858437


