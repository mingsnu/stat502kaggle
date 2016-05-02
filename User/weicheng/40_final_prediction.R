#!/usr/bin/env Rscript
library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

######################################################################
########################### For real test data #######################
######################################################################
training = readRDS("train_final_selected_ftr.RDS")
testing = readRDS("test_final_selected_ftr.RDS")
training.y = training$TARGET
training$TARGET = NULL
training.dat = Matrix(as.matrix(training), sparse = TRUE)
testing.dat = Matrix(as.matrix(testing), sparse = TRUE)
dtrain <- xgb.DMatrix(data=training.dat, label=training.y, missing = -999)

optpar = data.frame(Rounds=2000, Depth = 6, Min_child_weight = 2, c_sample=0.3,
                    r_sample = 0.55, eta =0.016, gamma = 1, best_round = 492) #

params =list(
  objective = "binary:logistic",
  eta =  optpar$eta,       
  max_depth = optpar$Depth,
  min_child_weight = optpar$Min_child_weight,
  subsample = optpar$r_sample,
  colsample_bytree = optpar$c_sample,
  gamma = optpar$gamma,
  eval_metric = "auc"
)

testing.pred = list()
for(i in 1:1000){
  cat("Round:", i, "\n")
  bst = xgboost(params = params,
                data=dtrain,
                  nrounds = optpar$best_round,    # max number of trees to build
                  verbose = FALSE
  )
  training.pred = predict(bst, training.dat)
  cat("The predicted AUC value is: ", colAUC(training.pred, training.y), "\n")
  testing.pred[[i]] = predict(bst, testing.dat)
  if(i/100 == 0){
    testing.pred.df = as.data.frame(testing.pred)
    names(testing.pred.df) = paste0("pred", 1:ncol(testing.pred.df))
    write.csv(testing.pred.df, "predict/predict1000.csv", row.names = FALSE)
  }
}
testing.pred.df = as.data.frame(testing.pred)
names(testing.pred.df) = paste0("pred", 1:ncol(testing.pred.df))
testing.pred.df$ID = testing$ID
write.csv(testing.pred.df, "predict/predict1000_final.csv", row.names = FALSE)
saveRDS(testing.pred.df, "predict/predict1000_final.rds")

formating = function(data, scale = 0.8){
  #data should be a matrix with length 70000+ and 50 column
  ID = data$ID
  data$ID = NULL
  y.mean = apply(data,1,mean)
  y.rank = rank(y.mean)
  group = floor(y.rank / (nrow(data)/9)) +1
  quantile = (.05+(group)*.1)*scale
  
  y.pred = rep(NA, length(y.mean))
  for (i in 1:max(group)){
    ind.i = group == i
    y.pred[ind.i] = apply(data[ind.i,], 1, quantile, prob = quantile[ind.i][1])
  }
  return(data.frame(ID = ID, TARGET = y.pred))
}

res.df = formating(testing.pred.df, 0.8)
res.df$ID = as.integer(res.df$ID)
head(res.df)
# aa = read.csv("submission.csv")
# plot(res.df$TARGET, aa$TARGET)
write.csv(res.df, "../../submission/sumision_xgboost0502.csv", row.names = FALSE, quote = FALSE)

