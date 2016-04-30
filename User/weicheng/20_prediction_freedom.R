library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(dplyr)
library(data.table)

######################################################################
######################## Explore freedom #############################
######################################################################

fdm = read.csv("tuning/Freedom/freedom_m20.csv", stringsAsFactors = FALSE)
fdm = read.csv("tuning/Freedom/freedom_no_ratio_m20.csv", stringsAsFactors = FALSE)

R = 6
m = ncol(fdm)
n <- nrow(fdm)
fdm.auc = fdm[, (m-R+1):m]

## choose feature combination by using mean value
fdm$aucMean = apply(fdm[, 21:26], 1, mean)
fdm$aucStd = apply(fdm[, 21:26], 1, sd)
head(fdm)
fdm %>% filter(aucMean > 0.835)

## choose feature combination by looking at # of top 5
fdm.bestcomb = unlist(lapply(1:R, function(i) which(fdm.auc[,i] %in% sort(fdm.auc[,i])[(n-5):n])))
fdm.bst.tbl = table(fdm.bestcomb)
fdm.bst.tbl[which.max(table(fdm.bestcomb))]

which(fdm$aucMean > 0.835)
fdm[253, 21:26]
fdm[27, 21:26]
fdm[538, 21:26]
aa = unlist(fdm[253, 1:20])
bb = unlist(fdm[27, 1:20])
cc = unlist(fdm[538, 1:20])
intersect(aa,bb)
intersect(aa,cc)
intersect(bb,cc)
ftrs.list = list(unname(aa), unname(bb), unname(cc))

## choose feature combination by looking at extreme cases
ftrs.list = list()
for(i in 1:6){
  ftrs.list[[i]] = unname(unlist(fdm[which.max(fdm.auc[,i]), 1:20]))
}
ftrs.list

######################################################################
######################## Self evaluation #############################
######################################################################

### Loading data & features
trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
ftrn = read.csv("../../feature/feature_all_train_ratio_only_wc_99.csv")
ftst = read.csv("../../feature/feature_all_test_ratio_only_wc_99.csv")
trn = left_join(trn, ftrn)
tst = left_join(tst, ftst)
trn = data.table(trn)
tst = data.table(tst)
trn.y = trn$TARGET

#### stratified sampling for train
set.seed(20160428)
R=5
trn.idx = createDataPartition(y = trn.y, times = R, p = .7)
j=1

training.pred = list()
testing.pred = list()
auc.pred = c()

optpar = data.frame(Rounds=2000, Depth = 4, r_sample = 0.8, eta =0.01)
params = list(
  objective = "binary:logistic", 
  eta = optpar$eta,    
  max_depth = optpar$Depth,   
  subsample = optpar$r_sample,
  eval_metric = "auc"  
)

for(i in 1:length(ftrs.list)){
  cat("Round ", i, ": \n")
  ftr.idx = names(trn) %in% ftrs.list[[i]]
  training = trn[trn.idx[[j]], ftr.idx, with=FALSE]
  training.y = trn.y[trn.idx[[j]]]
  testing = trn[-trn.idx[[j]], ftr.idx, with=FALSE]
  testing.y = trn.y[-trn.idx[[j]]]
  training.dat = Matrix(as.matrix(training), sparse = TRUE)
  testing.dat = Matrix(as.matrix(testing), sparse = TRUE)
  
  dtrain <- xgb.DMatrix(data=training.dat, label=training.y, missing = -999)

  cat("Doing cv ...\n")
  bst.cv <- xgb.cv(params = params,
                   data=dtrain,
                   nfold = 4,
                   nrounds = 2000,
                   verbose = 0,
                   early.stop.round = 20)
  cat("Fitting model ...\n")
  bst <- xgboost(params = params,
                 data=dtrain,
                 nrounds = which.max(bst.cv$test.auc.mean),
                 verbose = 0)
  training.pred[[i]] = predict(bst, training.dat)
  testing.pred[[i]] = predict(bst, testing.dat)
  auc.pred[i] = colAUC(testing.pred[[i]], testing.y)
  cat("The predicted AUC value is: ", auc.pred[i], "\n")
}
names(training.pred) = LETTERS[1:3]
training.pred.df = as.data.frame(training.pred)
names(testing.pred) = LETTERS[1:3]
testing.pred.df = as.data.frame(testing.pred)
colAUC(apply(testing.pred.df, 1, mean), testing.y)

# ##### using auc.pred to fit model again ######
# ##### NOT GOOD #####
# dtrain <- xgb.DMatrix(data=Matrix(as.matrix(training.pred.df), sparse = TRUE), label=training.y, missing = -999)
# params = list(
#   objective = "binary:logistic",
#   booster = "gblinear",
#   alpha = 0.5, 
#   lambda = 1,
#   eval_metric = "auc"  
# )
# bst.cv <- xgb.cv(params = params,
#                  data=dtrain,
#                  nfold = 4,
#                  nrounds = 2000,
#                  verbose = 1,
#                  early.stop.round = 20)
# bst <- xgboost(params = params,
#                data=dtrain,
#                nrounds = which.max(bst.cv$test.auc.mean),
#                verbose = 0)
# testing.pred = predict(bst, Matrix(as.matrix(testing.pred.df), sparse = TRUE))
# colAUC(testing.pred, testing.y)

######################################################################
########################### For real test data #######################
######################################################################
ftrSelected = unname(aa)
ftrSelected = unname(bb)
ftrSelected = unname(cc)
## ftrSelected = c(ftrSelected, "saldo_medio_var5_hace3", "imp_op_var39_ult1", "f_diff_imp_op_var41_efect_comer_ult1") # test-auc:0.837721+0.000559
## ftrSelected = setdiff(ftrSelected, c("new_ind_var5_0", "abs_f_diff_num_var30_4"))
impVars[!impVars %in% cc]

# varSelected = hexie1$ftrSelected[1:which.max(hexie1$aucMean)]
ftr.idx = which(names(trn) %in% ftrSelected)

training = trn[, ftr.idx, with=FALSE]
testing = tst[, ftr.idx, with=FALSE]

dtrain <- xgb.DMatrix(data=Matrix(as.matrix(training), sparse = TRUE), label=trn.y, missing = -999)

optpar = data.frame(Rounds=2000, Depth = 4, r_sample = 0.8, eta =0.01)

print("Train xgboost using xgb.train with watchlist")
params = list(
  objective = "binary:logistic",
  eta = optpar$eta, 
  max_depth = optpar$Depth, 
  subsample = optpar$r_sample,
  eval_metric = "auc")

bst.cv <- xgb.cv(params = params,
                 data=dtrain,
                 nfold = 4,
                 nrounds = 2000,
                 verbose = 1,
                 early.stop.round = 20)

bst <- xgboost(params = params,
               data=dtrain,
               nrounds = 732,
               verbose = 0)
importance <- xgb.importance(feature_names = names(training), model = bst)
head(importance$Feature, 20)
xgb.plot.importance(importance_matrix = importance)

## ## For test data
x.tst = Matrix(as.matrix(testing), sparse = TRUE)
pred = NULL
for(i in 1:10){
  bst <- xgboost(params = params,
                 data=dtrain,
                 nrounds = 732,
                 verbose = 0)
  y.tst.pred = predict(bst, x.tst)
  pred = cbind(pred, y.tst.pred)
}


res.df = data.frame(ID = tst$ID, TARGET = apply(pred, 1, mean))
res.df$ID = as.integer(res.df$ID)
head(res.df)
# aa = read.csv("submission.csv")
# plot(res.df$TARGET, aa$TARGET)
write.csv(res.df, "../../submission/sumision_xgboost0429.csv", row.names = FALSE, quote = FALSE)

