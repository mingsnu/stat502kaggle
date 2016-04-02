library(dplyr)
trn = readRDS("../../../data/train.rds")
tst = readRDS("../../../data/test.rds")
trn[trn[,2] < 0, 2] = NA
tst[tst[,2] < 0, 2] = NA
trn[trn == 9999999999] = NA
tst[tst == 9999999999] = NA
trn.unique.length = apply(trn[, -ncol(trn)], 2, function(x) length(unique(x)))
dat = rbind(trn[,-ncol(trn)], tst)
# ID                    var3                   var15      imp_ent_var16_ult1 imp_op_var39_comer_ult1 imp_op_var39_comer_ult3 
# 76020                     208                     100                     596                    7551                    9099 