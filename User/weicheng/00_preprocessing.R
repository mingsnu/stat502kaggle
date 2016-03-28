### Data pre-processing
trn = read.csv("../../data/train.csv")
tst = read.csv("../../data/test.csv")
colSum = colSums(trn)
## Delete columns with 0 variance
tmp = apply(trn, 2, range)
idx = which(tmp[1, ] - tmp[2, ] == 0)
trn = trn[, -idx]
tst = tst[, -idx]
dim(trn) # [1] 76020   337
dim(tst) # [1] 75818   336
head(trn)
saveRDS(trn, "../../data/train.rds")
saveRDS(tst, "../../data/tst.rds")

