library(Matrix)
library(xgboost)
library(caTools)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
trn$n0 = NULL
# trn = trn %>% select(ID)
# tst = tst %>% select(ID)

# ## Weicheng's feature
# source("features_weicheng.R")
# f1 = f_var15_ratio(trn, tst)
# f2 = f_var38_peak(trn, tst)
# f3 = f_var38_ratio(trn, tst)
# f5 = f_ind_comb_rank(trn, tst)
# trn = trn %>% select(ID) %>% left_join(f1$trn) %>% left_join(f2$trn) %>% left_join(f3$trn) %>% left_join(f5$trn)
# tst = tst %>% select(ID) %>% left_join((f1$tst)) %>% left_join((f2$tst)) %>% left_join((f3$tst)) %>% left_join(f5$tst)
# 
# write.csv(trn, "../../feature/feature_weich_ensample_train.csv",
#           row.names = FALSE, quote=FALSE)
# write.csv(tst, "../../feature/feature_weich_ensample_test.csv",
#           row.names = FALSE, quote=FALSE)

# ## hejian's feature
# aa = trn %>% select(ID) %>% left_join(read.csv("../../feature/numberofnot0balance_train.csv")) %>%
#   left_join(read.csv("../../feature/quantilediffbalance_train.csv")) %>%
#   left_join(read.csv("../../feature/quantileratiobalance_tain.csv")) %>%
#   left_join(read.csv("../../feature/numberofnot0num_train.csv")) %>%
#   left_join(read.csv("../../feature/quantilediffnum_train.csv")) %>%
#   left_join(read.csv("../../feature/quantilerationum_tain.csv"))
# write.csv(aa, "../../feature/feature_hj_ensample_train.csv", row.names = FALSE)
# 
# bb = tst %>% select(ID) %>% left_join(read.csv("../../feature/numberofnot0balance_test.csv")) %>%
#   left_join(read.csv("../../feature/quantilediffbalance_test.csv")) %>%
#   left_join(read.csv("../../feature/quantileratiobalance_test.csv")) %>%
#   left_join(read.csv("../../feature/numberofnot0num_test.csv")) %>%
#   left_join(read.csv("../../feature/quantilediffnum_test.csv")) %>%
#   left_join(read.csv("../../feature/quantilerationum_test.csv"))
# write.csv(bb, "../../feature/feature_hj_ensample_test.csv", row.names = FALSE)

files = list.files("../../feature/feature_individual")
ftrn.nms = grep("train", files, value = TRUE)
ftst.nms = grep("test", files, value = TRUE)
if(length(ftrn.nms) != length(ftst.nms))
  stop("train and test file number doesn't match!!!!!")
trn = trn %>% select(ID)
tst = tst %>% select(ID)
for(i in 1:length(ftrn.nms)){
  cat("Combining features\n")
  print(ftrn.nms[i])
  ftrn = read.csv(paste0("../../feature/feature_individual/", ftrn.nms[i]))
  print(sum(is.na(ftrn)))
  ftst = read.csv(paste0("../../feature/feature_individual/", ftst.nms[i]))
  print(sum(is.na(ftst)))
  print(names(ftst) %in% names(ftrn))
  trn = left_join(trn, ftrn)
  tst = left_join(tst, ftst)
}
write.csv(trn, "../../feature/feature_all_train_ratio_only_wc_all.csv", row.names = FALSE)
write.csv(tst, "../../feature/feature_all_test_ratio_only_wc_all.csv", row.names = FALSE)
