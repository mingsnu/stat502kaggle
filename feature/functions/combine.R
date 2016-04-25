library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
source("features_weicheng.R")
f1 = f_var15_ratio(trn, tst)
f2 = f_var38_peak(trn, tst)
f3 = f_var38_ratio(trn, tst)

y = trn$TARGET
trn$TARGET = NULL
trn = trn %>% left_join(f1$trn) %>% left_join(f2$trn) %>% left_join(f3$trn)
tst = tst %>% left_join((f1$tst)) %>% left_join((f2$tst)) %>% left_join((f3$tst))
x = Matrix(as.matrix(trn[, -1]), sparse = TRUE)