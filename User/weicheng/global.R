library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

# load in the training data
trn = readRDS("train_clean.RDS")
# tst = readRDS("test_clean.RDS")
x = Matrix(as.matrix(trn[, -c(1, ncol(trn))]), sparse = TRUE)
y = trn$TARGET

# ## xgboost parameters selector
# xgboostPS <- function()
xg_auc = data.frame("Rounds" = numeric(), 
                    "Depth" = numeric(),
                    "r_sample" = numeric(),
                    "c_sample" = numeric(), 
                    "eta" = numeric(),
                    "scale_pos_weight" = numeric(),
                    "auc_max" = numeric(),
                    "best_round" = numeric())
# ## First round tunning parameters
# ROUNDS = c(10, 100, 500)
# DEPTH = c(2, 4, 6, 8, 10)
# RSAMPLE = c(0.5, 0.75, 1)
# CSAMPLE = c(0.6, 0.8, 1)
# ETA =  c(0.1, 0.01, 0.001, 0.0001)
# SCALE = 0
## Second round tunning parameters
ROUNDS = c(300, 500, 1000)
DEPTH = c(5, 6, 7)
RSAMPLE = c(0.65, 0.75, 0.85)
CSAMPLE = c(0.75, 0.8, 0.85)
ETA =  c(0.05, 0.01, 0.005)
SPWEIGHT = c(0.8, 1, 10, sum(y==0)/sum(y==1))

for (rounds in ROUNDS){
  for (depth in DEPTH) {
    for (r_sample in RSAMPLE) {
      for (c_sample in CSAMPLE) {
        for(eta_val in ETA){
          for(sp_weight in SPWEIGHT){
            set.seed(1024)
            xgb_cv = xgb.cv(data = x, label = y, 
                            nfold = 10,
                            stratified = TRUE,    
                            nrounds = rounds, 
                            eta = eta_val, 
                            max_depth = depth,
                            subsample = r_sample, 
                            colsample_bytree = c_sample,
                            early.stop.round = 0.1*rounds,
                            scale_pos_weight = sp_weight,
                            objective='binary:logistic', 
                            eval_metric = 'auc',
                            verbose = TRUE)
            print(paste(rounds, depth, r_sample, c_sample, eta_val, sp_weight, max(xgb_cv$test.auc.mean)))
            xg_auc[nrow(xg_auc)+1, ] = c(rounds, 
                                         depth, 
                                         r_sample, 
                                         c_sample, 
                                         eta_val,
                                         sp_weight,
                                         # max(xgb_cv$dt$test.auc.mean), 
                                         max(xgb_cv$test.auc.mean), 
                                         which.max(xgb_cv$test.auc.mean)) 
          }
        }
      }
    }
  }
}

write.csv(xg_auc, file = "xgboost_PS1.csv", row.names=FALSE, quote = FALSE)

# xg_auc = read.csv("xgboost_PS.csv")
# head(xg_auc)
# idx = which.max(xg_auc$auc_max)
# xg_auc[idx,]
