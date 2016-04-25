library(dplyr)
library(data.table)
library(reshape2)
######### Feature1 var15 ##########
index.sort = function(feature.1,feature.2,target)
{
  # This function is used to order the numerical result based on their 1/0 ratio.
  aaa = table(feature.1,target)
  feature.1 = data.frame(num = feature.1,
                         order = 1:length(feature.1))
  
  prob = data.frame(num = as.double(row.names(aaa)),
                    prob=aaa[,2]/aaa[,1])
  prob= prob[sort(prob$prob,index.return=TRUE)$ix,]
  prob$index = 1:nrow(prob)
  
  result = merge(feature.1,prob[,c("num","index")])
  feature.sum = rep(1,length(result$order))
  feature.sum[result$order] = result$index
  
  feature.sum.tst = rep(1,length(feature.2))
  feature.2 = data.frame(num = feature.2,
                         order = 1:length(feature.2))
  result.tst = merge(feature.2,prob[,c("num","index")])
  feature.sum.tst[result.tst$order] = result.tst$index
  return(list(trn = feature.sum,
              tst = feature.sum.tst))
}

non.zero.quantile = function(x,prob)
{
  x=x[x!=0]
  return(quantile(x,prob))
}

generating_feature = function(data)
{
  col.names = read.csv("../../data/code_based_on_simplified_discrete_var_colnames.csv",stringsAsFactors = FALSE)$x # column names with less than 10 nonzero entries.
  data[is.na(data)] = 999999999
  col.names.more.than.10 = setdiff(colnames(data),c(col.names,"TARGET"))
  # Additional feature 1: ten patterns in num_var. This result is for TARGET 1/ TARGET 0
  bin.col.names = read.csv("../../data/code_based_on_simplified_discrete_var_bincolnames.csv",stringsAsFactors = FALSE)$x
  data.bin = data[,bin.col.names]
  ind.bin = data.bin>0
  data.bin[ind.bin] = 1
  result.pattern = data.frame(pattern = paste0("f",apply((data.bin),1,paste0,collapse = "")),
                              order = 1:nrow(data),stringsAsFactors = FALSE)
  feature.1 = rep(1,length(result.pattern$pattern))
  feature.ten.pattern = read.csv("../../data/feature_num_top_10_combination_pattern.csv",stringsAsFactors = FALSE)
  
  library(reshape2)
  result.pattern.me = merge(result.pattern,feature.ten.pattern,by = "pattern",sort = F)
  feature.1[result.pattern.me$order] = result.pattern.me$prop
  
  # Additional feature 2: three patterns in delta_var. This result is for TARGET 1/ TARGET 0
  delta.var.more.than.3 = read.csv("../../data/code_based_on_simplified_discrete_var_deltamorethan3.csv",stringsAsFactors = FALSE)$x
  data.delta = data[,delta.var.more.than.3]
  ind.delta = data.delta == 999999999
  data.delta[ind.delta] = 1
  data.delta = data.delta + 1
  result.pattern.delta = data.frame(pattern = paste0("f",apply((data.delta),1,paste0,collapse = "")),
                                    order = 1:nrow(data),stringsAsFactors = FALSE)
  feature.2 = rep(1,length(result.pattern.delta$pattern))
  feature.three.pattern = read.csv("../../data/feature_delta_top_3_combination_pattern.csv",stringsAsFactors = FALSE)
  
  result.pattern.delta.me = merge(result.pattern.delta,feature.three.pattern,by = "pattern",sort = F)
  feature.2[result.pattern.delta.me$order] = result.pattern.delta.me$prop
  
  # Additional feature 3: eight patterns in num_var for exactly 3 levels. This result is for TARGET 1/ TARGET 0.
  delta.var.more.than.3 = read.csv("../../data/code_based_on_simplified_discrete_var_numexactly3.csv",stringsAsFactors = FALSE)$x
  data.delta = data[,delta.var.more.than.3]
  result.pattern.delta = data.frame(pattern = paste0("f",apply((data.delta),1,paste0,collapse = "")),
                                    order = 1:nrow(data),stringsAsFactors = FALSE)
  feature.3 = rep(1,length(result.pattern.delta$pattern))
  feature.three.pattern = read.csv("../../data/feature_num_top_8_combination_pattern.csv",stringsAsFactors = FALSE)
  
  result.pattern.delta.me = merge(result.pattern.delta,feature.three.pattern,by = "pattern",sort = F)
  feature.3[result.pattern.delta.me$order] = result.pattern.delta.me$prop
  
  return(cbind(ID = data[,"ID"],  bin_top_pattern_paste = feature.1,delta_top_pattern_paste = feature.2,num_three_level_top_pattern_paste=feature.3))
  
}

f_num_non_zero_sum_ind <- function(trn,tst){
  set.name = colnames(trn)
  
  #ind sum
  ind.name = set.name[grep("ind",set.name)]
  feature.1 =(apply(trn[,ind.name]==0,1,sum))
  feature.2 =(apply(tst[,ind.name]==0,1,sum))
  feature.sum = index.sort(feature.1 =feature.1,feature.2 = feature.2,target = trn[,"TARGET"])
  return(feature.sum)
}

f_num_non_zero_sum_saldo <- function(trn,tst){
  set.name = colnames(trn)
  
  #ind sum
  ind.name = set.name[grep("saldo",set.name)]
  feature.1 =(apply(trn[,ind.name]==0,1,sum))
  feature.2 =(apply(tst[,ind.name]==0,1,sum))
  feature.sum = index.sort(feature.1 =feature.1,feature.2 = feature.2,target = trn[,"TARGET"])
  return(feature.sum)
}

f_num_zero_sum_delta <- function(trn,tst){
  set.name = colnames(trn)
  
  #ind sum
  ind.name = set.name[grep("^delta",set.name)]
  feature.1 =(apply(trn[,ind.name]==0,1,sum))
  feature.2 =(apply(tst[,ind.name]==0,1,sum))
  feature.sum = index.sort(feature.1 =feature.1,feature.2 = feature.2,target = trn[,"TARGET"])
  return(feature.sum)
}

f_num_neg_1_sum_delta <- function(trn,tst){
  set.name = colnames(trn)
  
  #ind sum
  ind.name = set.name[grep("^delta",set.name)]
  feature.1 =(apply(trn[,ind.name]==-1,1,sum))
  feature.2 =(apply(tst[,ind.name]==-1,1,sum))
  feature.sum = index.sort(feature.1 =feature.1,feature.2 = feature.2,target = trn[,"TARGET"])
  return(feature.sum)
}

f_num_3_sum_num <- function(trn,tst){
  set.name = colnames(trn)
  
  #ind sum
  ind.name = set.name[grep("^num",set.name)]
  feature.1 =(apply(trn[,ind.name]==3,1,sum))
  feature.2 =(apply(tst[,ind.name]==3,1,sum))
  feature.sum = index.sort(feature.1 =feature.1,feature.2 = feature.2,target = trn[,"TARGET"])
  return(feature.sum)
}

f_num_0_sum_num <- function(trn,tst){
  set.name = colnames(trn)
  
  #ind sum
  ind.name = set.name[grep("^num",set.name)]
  feature.1 =(apply(trn[,ind.name]==0,1,sum))
  feature.2 =(apply(tst[,ind.name]==0,1,sum))
  feature.sum = index.sort(feature.1 =feature.1,feature.2 = feature.2,target = trn[,"TARGET"])
  return(feature.sum)
}

f_num_quan_70_sum_imp <- function(trn,tst){
  set.name = colnames(trn)
  
  #ind sum
  ind.name = set.name[grep("^imp",set.name)]
  data.train = trn[,ind.name]
  quan. = apply(data.train,2,non.zero.quantile,prob = .7) #.7 seems to work.
  comparison.matrix = matrix(rep(quan.,each = nrow(data.train)),ncol = ncol(data.train))
  feature.1 = apply(data.train>comparison.matrix,1,sum)
  
  comparison.matrix.tst = matrix(rep(quan.,each = nrow(tst)),ncol = ncol(data.train))
  feature.2 =(apply(tst[,ind.name]>comparison.matrix.tst,1,sum))
  feature.sum = index.sort(feature.1 =feature.1,feature.2 = feature.2,target = trn[,"TARGET"])
  return(feature.sum)
}

f_num_quan_70_sum_saldo <- function(trn,tst){
  set.name = colnames(trn)
  
  #ind sum
  ind.name = set.name[grep("^saldo",set.name)]
  data.train = trn[,ind.name]
  quan. = apply(data.train,2,non.zero.quantile,prob = .7) #.7 seems to work.
  comparison.matrix = matrix(rep(quan.,each = nrow(data.train)),ncol = ncol(data.train))
  feature.1 = apply(data.train>comparison.matrix,1,sum)
  
  comparison.matrix.tst = matrix(rep(quan.,each = nrow(tst)),ncol = ncol(data.train))
  feature.2 =(apply(tst[,ind.name]>comparison.matrix.tst,1,sum))
  feature.sum = index.sort(feature.1 =feature.1,feature.2 = feature.2,target = trn[,"TARGET"])
  return(feature.sum)
}

f_diff_imp_op_var39_comer_ult13 <-  function(trn,tst){
  name.1 = "imp_op_var39_comer_ult1"
  name.2 = "imp_op_var39_comer_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_imp_op_var40_efect_comer_ult1 <-  function(trn,tst){
  name.1 = "imp_op_var40_comer_ult1"
  name.2 = "imp_op_var40_efect_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_imp_op_var40_comer_ult13 <-  function(trn,tst){
  name.1 = "imp_op_var40_comer_ult1"     
  name.2 = "imp_op_var40_comer_ult3" 
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_imp_op_var41_comer_ult13 <-  function(trn,tst){
  name.1 = "imp_op_var41_comer_ult1"     
  name.2 = "imp_op_var41_comer_ult3" 
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_imp_op_var41_efect_comer_ult1 <-  function(trn,tst){
  name.1 = "imp_op_var41_comer_ult1"
  name.2 = "imp_op_var41_efect_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_op_var40_ult13 <-  function(trn,tst){
  name.1 = "num_op_var40_ult1"
  name.2 = "num_op_var40_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}


f_diff_imp_trasp_var17_in_out_ult1 <-  function(trn,tst){
  name.1 = "imp_trasp_var17_in_ult1" ### Good one if there this two does not match, it is 0.
  name.2 = "imp_trasp_var17_out_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_imp_trasp_var33_in_out_ult1 <-  function(trn,tst){
  name.1 = "imp_trasp_var33_in_ult1" ### Good one if there this two does not match, it is 0.
  name.2 = "imp_trasp_var33_out_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_imp_op_var41_efect_ult13 <-  function(trn,tst){
  name.1 = "imp_op_var41_efect_ult1" ### Good one if there this two does not match, it is 0.
  name.2 = "imp_op_var41_efect_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_op_var40_ult13 <-  function(trn,tst){
  name.1 = "num_op_var40_ult1" ### Good one
  name.2 = "num_op_var40_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_op_var41_ult13 <-  function(trn,tst){
  name.1 = "num_op_var41_ult1" ### Good one
  name.2 = "num_op_var41_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_imp_var7_emit_receib_ult1 <-  function(trn,tst){
  name.1 = "imp_var7_emit_ult1" ### Good one
  name.2 = "imp_var7_recib_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_meses_var13_corto_largo_ult3 <-  function(trn,tst){
  name.1 = "num_meses_var13_corto_ult3" ### Good one Important feature
  name.2 = "num_meses_var13_largo_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  
  feature.sum = index.sort(feature.1 =feature.1,feature.2 = feature.2,target = trn[,"TARGET"])
  return(feature.sum)
}

f_diff_num_op_var41_comer_efect_ult1 <-  function(trn,tst){
  name.1 = "num_op_var41_comer_ult1" ### Good one Important feature
  name.2 = "num_op_var41_efect_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_op_var41_efect_ult13 <-  function(trn,tst){
  name.1 = "num_op_var41_efect_ult1" ### Good one Important feature
  name.2 = "num_op_var41_efect_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_op_var41_efect_comer_ult1<-  function(trn,tst){
  name.1 = "num_op_var41_efect_ult1" ### Good one Important feature
  name.2 = "num_op_var41_comer_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}


f_diff_num_op_var41_comer_efect_ult3<-  function(trn,tst){
  name.1 = "num_op_var41_efect_ult3" ### Good one Important feature
  name.2 = "num_op_var41_comer_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_op_var39_efect_ult13<-  function(trn,tst){
  name.1 = "num_op_var39_efect_ult1" ### Good one Important feature
  name.2 = "num_op_var39_efect_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_var43_emit_recib_ult1<-  function(trn,tst){
  name.1 = "num_var43_emit_ult1" ### Good one Important feature
  name.2 = "num_var43_recib_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_var45_ult13<-  function(trn,tst){
  name.1 = "num_var45_ult1" ### Good one Important feature
  name.2 = "num_var45_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}


f_diff_saldo_medio_var5_ult13<-  function(trn,tst){
  name.1 = "saldo_medio_var5_ult1" ### Good one Important feature
  name.2 = "saldo_medio_var5_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_medio_var8_ult13<-  function(trn,tst){
  name.1 = "saldo_medio_var8_ult1" ### Good one Important feature
  name.2 = "saldo_medio_var8_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_medio_var5_8_ult11<-  function(trn,tst){
  name.1 = "saldo_medio_var8_ult1" ### Good one Important feature
  name.2 = "saldo_medio_var5_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_medio_var12_ult13<-  function(trn,tst){
  name.1 = "saldo_medio_var12_ult1" ### Good one Important feature
  name.2 = "saldo_medio_var12_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_medio_var13_corto_ult13<-  function(trn,tst){
  name.1 = "saldo_medio_var13_corto_ult1" ### Good one Important feature
  name.2 = "saldo_medio_var13_corto_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_medio_var13_largo_corto_ult3<-  function(trn,tst){
  name.1 = "saldo_medio_var13_largo_ult3" ### Good one Important feature
  name.2 = "saldo_medio_var13_corto_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_medio_var13_medio_corto_ult3<-  function(trn,tst){
  name.1 = "saldo_medio_var13_medio_ult1" ### Good one Important feature
  name.2 = "saldo_medio_var13_corto_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_medio_var44_ult13<-  function(trn,tst){
  name.1 = "saldo_medio_var44_ult1" ### Good one Important feature
  name.2 = "saldo_medio_var44_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_imp_aport_amort_var34_ult1<-  function(trn,tst){
  name.1 = "imp_amort_var34_ult1" ### Good one Important feature
  name.2 = "imp_aport_var13_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_imp_aport_var17_13_ult1<-  function(trn,tst){
  name.1 = "imp_aport_var17_ult1" ### Good one Important feature
  name.2 = "imp_aport_var13_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_imp_aport_reemb_var33_ult1<-  function(trn,tst){
  name.1 = "imp_aport_var33_ult1" ### Good one Important feature
  name.2 = "imp_reemb_var33_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}






f_change_var2_NA_2<-  function(trn,tst){
  feature.1 = trn[,"var3"]
  feature.1[feature.1 ==  -999999] = 2
  feature.2 = tst[,"var3"]
  feature.2[feature.2 ==  -999999] = 2
  return(list(trn = feature.1,
              tst = feature.2))
}

f_change_var2_NA_3<-  function(trn,tst){
  feature.1 = trn[,"var3"]
  feature.1[feature.1 ==  -999999] =3
  feature.2 = tst[,"var3"]
  feature.2[feature.2 ==  -999999] = 3
  return(list(trn = feature.1,
              tst = feature.2))
}



f_change_var_delat_NA_neg_05<-  function(trn,tst){
  names.aa = c("delta_imp_aport_var13_1y3"  ,   "delta_imp_aport_var17_1y3" ,    "delta_imp_aport_var33_1y3" ,   
  "delta_imp_compra_var44_1y3" ,   "delta_imp_reemb_var13_1y3"   ,  "delta_imp_reemb_var17_1y3"  ,  
  "delta_imp_trasp_var17_in_1y3" , "delta_imp_trasp_var33_in_1y3" , "delta_imp_trasp_var33_out_1y3",
   "delta_imp_venta_var44_1y3"  ,   "delta_num_aport_var13_1y3" ,    "delta_num_aport_var17_1y3"    ,
 "delta_num_aport_var33_1y3"  ,   "delta_num_compra_var44_1y3")  
  
  feature.1 = trn[,names.aa]
  feature.1[is.na(feature.1)] = -.5
  colnames(feature.1) = paste0("new_",names.aa)
  feature.2 = tst[,names.aa]
  feature.2[is.na(feature.2)] = -.5
  colnames(feature.2) = paste0("new_",names.aa)
  return(list(trn = feature.1,
              tst = feature.2))
}


f_three_patterns<-  function(trn,tst){
  return(list(trn =generating_feature(trn),
              tst =generating_feature(tst)))
}

f_diff_num_var30_42<-  function(trn,tst){
  name.1 = "num_var30" ### Good one Important feature
  name.2 = "num_var42"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_var30_4<-  function(trn,tst){
  name.1 = "num_var4" ### Good one Important feature
  name.2 = "num_var30"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_num_var30_0_42_0<-  function(trn,tst){
  name.1 = "num_var30_0" ### Good one Important feature
  name.2 = "num_var42_0"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_var30_42<-  function(trn,tst){
  name.1 = "saldo_var30" ### Good one Important feature
  name.2 = "saldo_var42"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}


f_diff_var36_38<-  function(trn,tst){
  name.1 = "var36" ### Good one Important feature
  name.2 = "var38"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}


f_diff_saldo_var5_30<-  function(trn,tst){
  name.1 = "saldo_var5" ### Good one Important feature
  name.2 = "saldo_var30"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_var5_42<-  function(trn,tst){
  name.1 = "saldo_var5" ### Good one Important feature
  name.2 = "saldo_var42"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}


f_diff_num_var22_ult3_num_med_var45_ult3<-  function(trn,tst){
  name.1 = "num_var22_ult3" ### Good one Important feature
  name.2 = "num_med_var45_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}


f_diff_saldo_var5_saldo_medio_var5_hace2<-  function(trn,tst){
  name.1 = "saldo_var5" ### Good one Important feature
  name.2 = "saldo_medio_var5_hace2"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_var5_saldo_medio_var5_hace3<-  function(trn,tst){
  name.1 = "saldo_var5" ### Good one Important feature
  name.2 = "saldo_medio_var5_hace3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}


f_diff_saldo_var5_saldo_medio_var5_ult1<-  function(trn,tst){
  name.1 = "saldo_var5" ### Good one Important feature
  name.2 = "saldo_medio_var5_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_medio_var5_hace2_saldo_medio_var5_ult1<-  function(trn,tst){
  name.1 = "saldo_medio_var5_hace2" ### Good one Important feature
  name.2 = "saldo_medio_var5_ult1"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_saldo_medio_var5_hace3_saldo_medio_var5_ult3<-  function(trn,tst){
  name.1 = "saldo_medio_var5_hace3" ### Good one Important feature
  name.2 = "saldo_medio_var5_ult3"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_var_15_var_38<-  function(trn,tst){
  name.1 = "var15" ### Good one Important feature
  name.2 = "var38"
  feature.1 = trn[,name.1] - trn[,name.2]
  feature.2 = tst[,name.1] - tst[,name.2]
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_var_15_var_36_less_60<-  function(trn,tst){
  name.1 = "var15" ### Good one Important feature
  name.2 = "var36"
  feature.1 = abs(trn[,name.1] - trn[,name.2])
  feature.1[feature.1>60] = 0
  feature.2 = abs(tst[,name.1] - tst[,name.2])
  feature.2[feature.2>60] = 0
  return(list(trn = feature.1,
              tst = feature.2))
}

f_diff_var_15_var_36_greater_60<-  function(trn,tst){
  name.1 = "var15" ### Good one Important feature
  name.2 = "var36"
  feature.1 = abs(trn[,name.1] - trn[,name.2])
  feature.1[feature.1<60] = 0
  feature.2 = abs(tst[,name.1] - tst[,name.2])
  feature.2[feature.2<60] = 0
  return(list(trn = feature.1,
              tst = feature.2))
}

