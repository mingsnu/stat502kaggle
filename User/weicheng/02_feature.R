library(dplyr)
# trn = readRDS("../../data/train.rds")
# tst = readRDS("../../data/test.rds")
# dim(trn) # [1] 76020   337
# trn[trn[,2] < 0, 2] = NA
# tst[tst[,2] < 0, 2] = NA
# trn[trn >= 9999999999] = NA
# tst[tst >= 9999999999] = NA
# nms = names(trn)
# nms.del = read.csv("../../data/del_name_list.csv")[[1]]
# trn.clean = trn[, setdiff(nms, nms.del)]
# tst.clean = tst[, setdiff(nms, c(nms.del, "TARGET"))]
# saveRDS(trn.clean, "train_clean.RDS")
# saveRDS(tst.clean, "test_clean.RDS")
trn = readRDS("train_clean.RDS")
tst = readRDS("test_clean.RDS")
head(trn)
trn.unique.length = apply(trn[, -ncol(trn)], 2, function(x) length(unique(x)))

## focus on binary variables
trn2 = trn[, which(trn.unique.length == 2)]
## replace non 0 values to 1
trn2  = as.data.frame(lapply(trn2, function(x){replace(x, x != 0, 1)}))

######### matrix visulization
f <- function(tbl){
  if(ncol(tbl) != 2) stop("Need two column matrix as input!")
  nms = gsub("NA", "N", rownames(tbl))
  nms.mat = t(sapply(1:length(nms), function(i) as.numeric(strsplit(nms[i], "")[[1]])))
  score = apply(tbl, 1, function(x) {x[2]/sum(x)})
  nms.mat = nms.mat*score
  image(1:nrow(nms.mat), 1:ncol(nms.mat), nms.mat, col=terrain.colors(30))
}

## Possible features
tmp <- apply(trn2, 1 , paste0 , collapse = "" )
# which(nchar(tmp)!=66) 
# trn2[which(nchar(tmp)!=66),]

length(unique(tmp))
tbl = table(tmp, trn$TARGET)
dim(tbl)
tbl = as.matrix(tbl)
f(tbl)
# 
# trn2 = trn2[trn$TARGET==1, nms.var2]
# tmp <- apply(trn2, 1 , paste0 , collapse = "" )
# length(unique(tmp))
# tbl = table(tmp, trn$TARGET)
# dim(tbl)

nms.var2 = names(trn2)
trn2.ind = trn2[, grep("^ind_", nms.var2, value=TRUE)]
nms.trn2.ind = names(trn2.ind)
tmp1 <- apply(trn2.ind, 1, paste0, collapse = "")
length(unique(tmp1))
tbl1 = table(tmp1, trn$TARGET)
dim(tbl1)
tbl1 = as.matrix(tbl1)
f(tbl1)

trn2.notind = trn2[, grep("^(?!ind_)", nms.var2, value=TRUE, perl = TRUE)]
tmp2 <- apply(trn2.notind, 1, paste0, collapse = "")
length(unique(tmp2))
tbl2 = table(tmp2, trn$TARGET)
dim(tbl2)
tbl2 = as.matrix(tbl2)
f(tbl2)


nms.var3andmore = setdiff(nms, nms.var2)



