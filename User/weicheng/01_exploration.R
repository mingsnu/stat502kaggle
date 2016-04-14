library(dplyr)
trn = readRDS("../../data/train.rds")
tst = readRDS("../../data/test.rds")
dim(trn) # [1] 76020   337
trn[trn[,2] < 0, 2] = NA
tst[tst[,2] < 0, 2] = NA
trn[trn >= 9999999999] = NA
tst[tst >= 9999999999] = NA
nms = names(trn)


##### Categorical variables
#### Binary variables
nms.var2 = c('ind_var1_0', 'ind_var1', 'ind_var5_0', 'ind_var5', 'ind_var6_0', 'ind_var6', 'ind_var8_0', 
         'ind_var8', 'ind_var12_0', 'ind_var12', 'ind_var13_0', 'ind_var13_corto_0', 'ind_var13_corto',
         'ind_var13_largo_0', 'ind_var13_largo', 'ind_var13_medio_0', 'ind_var13_medio', 'ind_var13', 
         'ind_var14_0', 'ind_var14', 'ind_var17_0', 'ind_var17', 'ind_var18_0', 'ind_var18', 'ind_var19', 
         'ind_var20_0', 'ind_var20', 'ind_var24_0', 'ind_var24', 'ind_var25_cte', 'ind_var26_0', 'ind_var26_cte', 
         'ind_var26', 'ind_var25_0', 'ind_var25', 'ind_var29_0', 'ind_var29', 'ind_var30_0', 'ind_var30', 'ind_var31_0',
         'ind_var31', 'ind_var32_cte', 'ind_var32_0', 'ind_var32', 'ind_var33_0', 'ind_var33', 'ind_var34_0', 'ind_var34', 
         'ind_var37_cte', 'ind_var37_0', 'ind_var37', 'ind_var40_0', 'ind_var39_0', 'ind_var41_0', 'ind_var40', 'ind_var39', 
         'ind_var44_0', 'ind_var44', 'num_var6_0', 'num_var6', 'num_var8', 'num_var13_medio', 'num_var18_0', 'num_var18', 
         'num_var20_0', 'num_var20', 'num_var29_0', 'num_var29', 'num_var34_0', 'num_var34', 'num_var40', 'num_var39', 
         'delta_imp_amort_var18_1y3', 'delta_imp_amort_var34_1y3', 'delta_imp_reemb_var13_1y3', 'delta_imp_reemb_var33_1y3', 
         'delta_imp_trasp_var17_out_1y3', 'delta_imp_trasp_var33_out_1y3', 'delta_num_reemb_var13_1y3', 'delta_num_reemb_var33_1y3', 
         'delta_num_trasp_var17_out_1y3', 'delta_num_trasp_var33_out_1y3', 'imp_reemb_var17_hace3', 'imp_reemb_var33_ult1', 
         'ind_var7_emit_ult1', 'ind_var7_recib_ult1', 'ind_var10_ult1', 'ind_var10cte_ult1', 'ind_var9_cte_ult1', 'ind_var9_ult1', 
         'ind_var43_emit_ult1', 'ind_var43_recib_ult1', 'num_meses_var13_medio_ult3', 'num_reemb_var13_ult1', 'num_reemb_var17_hace3', 
         'num_reemb_var33_ult1', 'num_trasp_var17_out_ult1', 'num_trasp_var33_in_hace3', 'num_trasp_var33_out_ult1', 
         'saldo_medio_var29_hace3')
trn2 = trn[, nms.var2]
### names starting with `ind` in `nms.var2`
nms.ind = grep("^ind_", nms, value=TRUE)
all(nms.ind %in% nms.var2)
## TRUE
### names starting with `num` in `nms.var2`
nms.num2 = grep("^num_", nms.var2, value = TRUE)

## replace non 0 values to 1
trn2  = as.data.frame(lapply(trn2, function(x){replace(x, x != 0, 1)}))
apply(trn2, 2, unique)

## find the duplicated column names
res = NULL
for(i in 1: (ncol(trn2) - 1))
  for(j in (i+1):ncol(trn2)){
    if(identical(trn2[,i], trn2[,j]))
      res = rbind(res, c(i,j))
  }
# matrix(nms.var2[res], nrow(res))
nms.del2 = sort(nms.var2[unique(res[,2])])

## remove the duplicated columns
nms.var2.uni = setdiff(nms.var2, nms.del2)
length(nms.var2.uni) #69


## 
trn2.uni = trn2[, nms.var2.uni]
table(trn2.uni[,1:2])
table(trn2.uni[,3:4])
table(trn2.uni[,5:6])
table(trn2.uni[,7:8])
table(trn2.uni[,9:10])
table(trn2.uni[,11:12])
table(trn2.uni[,11:13])
table(trn2.uni[,14:15])
table(trn2.uni[,15:16])


## Possible features
trn2.uni = trn2[, nms.var2.uni]
tmp <- apply(trn2.uni, 1 , paste0 , collapse = "" )
length(unique(tmp))
tbl = table(tmp, trn$TARGET)
dim(tbl)

trn2.uni = trn2[trn$TARGET==1, nms.var2.uni]
tmp <- apply(trn2.uni, 1 , paste0 , collapse = "" )
length(unique(tmp))
tbl = table(tmp, trn$TARGET)
dim(tbl)


trn2.ind = trn2[, grep("^ind_", nms.var2.uni, value=TRUE)]
tmp1 <- apply(trn2.ind, 1, paste0, collapse = "")
length(unique(tmp1))
tbl1 = table(tmp1, trn$TARGET)
dim(tbl1)

trn2.notind = trn2[, grep("^(?!ind_)", nms.var2.uni, value=TRUE, perl = TRUE)]
tmp2 <- apply(trn2.notind, 1, paste0, collapse = "")
length(unique(tmp2))
tbl2 = table(tmp2, trn$TARGET)
dim(tbl2)

nms.var3andmore = setdiff(nms, nms.var2)

