library(dplyr)
trn = readRDS("../../../data/train.rds")
trn[trn[,2] < 0, 2] = NA
trn.unique.length = apply(trn, 2, function(x) length(unique(x)))
# ID                    var3                   var15      imp_ent_var16_ult1 imp_op_var39_comer_ult1 imp_op_var39_comer_ult3 
# 76020                     208                     100                     596                    7551                    9099 