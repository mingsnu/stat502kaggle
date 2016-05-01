library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(dplyr)
library(data.table)

trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
ftrn = read.csv("../../feature/feature_all_train_ratio_only_wc_all.csv")
ftst = read.csv("../../feature/feature_all_test_ratio_only_wc_all.csv")

## using raw data + all feature directly
training = left_join(trn, ftrn)
testing = left_join(tst, ftst)
saveRDS(training, "train_final_all.RDS")
saveRDS(testing, "test_final_all.RDS")
dim(training); dim(testing)

## using raw + selected features
selectedFtr = readRDS("features/finalFtr.rds")
ftr.idx = which(names(ftrn) %in% c("ID", selectedFtr))
training = left_join(trn, ftrn[, ftr.idx])
testing = left_join(tst, ftst[, ftr.idx])
saveRDS(training, "train_final_selected_ftr.RDS")
saveRDS(testing, "test_final_selected_ftr.RDS")
dim(training); dim(testing)
