library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# load in the training data
trn = readRDS("train_clean.RDS")
# tst = readRDS("test_clean.RDS")
x = Matrix(as.matrix(trn[, -c(1, ncol(trn))]), sparse = TRUE)
y = trn$TARGET

# ## xgboost parameters selector
# xgboostPS <- function()
GS_LogLoss = data.frame("Rounds" = numeric(), 
                        "Depth" = numeric(),
                        "r_sample" = numeric(),
                        "c_sample" = numeric(), 
                        "eat" = numeric(),
                        "auc_max" = numeric(),
                        "best_round" = numeric())

# xgb_grid_1 = expand.grid(
#   nrounds = 1000,
#   eta = c(0.01, 0.001, 0.0001),
#   max_depth = c(2, 4, 6, 8, 10),
#   gamma = 1,
#   colsample_bytree = 1, 
#   min_child_weight = 1
# )
# max_delta_step = c(0, 1, 3)

for (rounds in c(10, 100, 500)){
  for (depth in c(2, 4, 6, 8, 10)) {
    for (r_sample in c(0.5, 0.75, 1)) {
      for (c_sample in c(0.6, 0.8, 1)) {
        for(eta_val in c(0.1, 0.01, 0.001, 0.0001)){
          set.seed(1024)
          xgb_cv = xgb.cv(data = x, label = y, 
                          nfold = 10,
                          prediction = TRUE,                                           # return the prediction using the final model 
                          showsd = TRUE,                                               # standard deviation of loss across folds
                          stratified = TRUE,    
                          nrounds = rounds, 
                          eta = eta_val, 
                          max_depth = depth,
                          subsample = r_sample, 
                          colsample_bytree = c_sample,
                          early.stop.round = 0.1*rounds,
                          objective='binary:logistic', 
                          eval_metric = 'auc',
                          verbose = TRUE)
          # plot the AUC for the training and testing samples
         pdf(paste0("imgs/xgboost", rounds, "_", depth, "_", r_sample*100, "_", c_sample*100, "_", eta_val, ".pdf"))
          xgb_cv$dt %>%
            select(-contains("std")) %>%
            mutate(IterationNum = 1:n()) %>%
            gather(TestOrTrain, AUC, -IterationNum) %>%
            ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
            geom_line() + 
            theme_bw()
          # ggsave(paste0("imgs/xgboost", rounds, "_", depth, "_", r_sample*100, "_", c_sample*100, "_", eta_val, ".png"))
          dev.off()
          print(paste(rounds, depth, r_sample, c_sample, eta_val, max(xgb_cv$dt$test.auc.mean)))
          GS_LogLoss[nrow(GS_LogLoss)+1, ] = c(rounds, 
                                               depth, 
                                               r_sample, 
                                               c_sample, 
                                               eta_val,
                                               max(xgb_cv$dt$test.auc.mean), 
                                               which.max(xgb_cv$dt$test.auc.mean))
        }
      }
    }
  }
}

write.csv(GS_LogLoss, file = "GS_LogLoss.csv", row.names=TRUE, quote = FALSE)
