library(dplyr)
trn = readRDS("../../data/train.rds")
dim(trn) # [1] 76020   337
i=2
trni = trn[,i]
summary(trn[,i])
trni[trni < 0]
hist(trn[,i])
