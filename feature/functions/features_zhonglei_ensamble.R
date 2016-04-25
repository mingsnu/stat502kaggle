library(dplyr)
library(data.table)
library(reshape2)
source("features_zhonglei.R")
fun.name=c(
  "f_num_non_zero_sum_ind","f_num_non_zero_sum_saldo","f_num_zero_sum_delta","f_num_neg_1_sum_delta","f_num_3_sum_num","f_num_0_sum_num","f_num_quan_70_sum_imp","f_num_quan_70_sum_saldo",
  "f_diff_imp_op_var39_comer_ult13","f_diff_imp_op_var40_efect_comer_ult1","f_diff_imp_op_var40_comer_ult13","f_diff_imp_op_var41_comer_ult13","f_diff_imp_op_var41_efect_comer_ult1",
  "f_diff_num_op_var40_ult13","f_diff_num_op_var41_ult13","f_diff_imp_trasp_var17_in_out_ult1","f_diff_imp_trasp_var33_in_out_ult1","f_diff_imp_op_var41_efect_ult13",
  "f_diff_imp_var7_emit_receib_ult1","f_diff_num_meses_var13_corto_largo_ult3","f_diff_num_op_var41_comer_efect_ult1","f_diff_num_op_var41_efect_ult13","f_diff_num_op_var41_efect_comer_ult1",
  "f_diff_num_op_var41_comer_efect_ult3","f_diff_num_op_var39_efect_ult13","f_diff_num_var43_emit_recib_ult1","f_diff_num_var45_ult13","f_diff_saldo_medio_var5_ult13","f_diff_saldo_medio_var8_ult13",
  "f_diff_saldo_medio_var5_8_ult11","f_diff_saldo_medio_var12_ult13","f_diff_saldo_medio_var13_corto_ult13","f_diff_saldo_medio_var13_largo_corto_ult3","f_diff_saldo_medio_var13_medio_corto_ult3",
  "f_diff_saldo_medio_var44_ult13","f_diff_imp_aport_amort_var34_ult1","f_diff_imp_aport_var17_13_ult1","f_diff_imp_aport_reemb_var33_ult1",
  "f_change_var2_NA_2","f_change_var2_NA_3","f_diff_num_var30_42","f_diff_num_var30_4","f_diff_num_var30_0_42_0",
  "f_diff_saldo_var30_42","f_diff_var36_38","f_diff_saldo_var5_30","f_diff_saldo_var5_42","f_diff_num_var22_ult3_num_med_var45_ult3",
  "f_diff_saldo_var5_saldo_medio_var5_hace2","f_diff_saldo_var5_saldo_medio_var5_hace3","f_diff_saldo_var5_saldo_medio_var5_ult1","f_diff_saldo_medio_var5_hace2_saldo_medio_var5_ult1",
  "f_diff_saldo_medio_var5_hace3_saldo_medio_var5_ult3","f_diff_var_15_var_38","f_diff_var_15_var_36_less_60")
feature_zl = function(trn,tst,fun.name)
{
  len.fun = length(fun.name)
  feature.trn = matrix(0,nrow = nrow(trn),ncol = len.fun+1)
  feature.tst = matrix(0,nrow = nrow(tst),ncol = len.fun+1)
  feature.trn[,1] = trn[,"ID"]
  feature.tst[,1] = tst[,"ID"]
  for (i in 1:len.fun)
  {
    aaa = do.call(fun.name[i],list(trn=trn,tst=tst))
    feature.tst[,i+1] = aaa$tst
    feature.trn[,i+1] = aaa$trn
    print(i)
  }
  feature.tst = as.data.frame(feature.tst)
  feature.trn = as.data.frame(feature.trn)
  names(feature.tst) =c("ID", fun.name)
  names(feature.trn) =c("ID", fun.name)
  delta.var = f_change_var_delat_NA_neg_05(trn,tst)
  feature.tst = cbind(feature.tst,delta.var$tst)
  feature.trn = cbind(feature.trn,delta.var$trn)
  return(list(
    trn = feature.trn,
    tst = feature.tst
  ))
}

feature.new = feature_zl(trn = train.d,tst = test.d,fun.name = fun.name)

write.csv(feature.new$trn,"../feature_zl_ensample_train.csv",row.names = FALSE)
write.csv(feature.new$tst,"../feature_zl_ensample_test.csv",row.names = FALSE)
