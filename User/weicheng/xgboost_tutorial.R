## https://github.com/dmlc/xgboost/blob/master/R-package/vignettes/xgboostPresentation.Rmd
require(xgboost)
require(Matrix)
require(data.table)
library(vcd) # for dataset
data(Arthritis)
df <- data.table(Arthritis, keep.rownames = F)
head(df)
str(df)
head(df[,AgeDiscret := as.factor(round(Age/10,0))])
head(df[,AgeCat:= as.factor(ifelse(Age > 30, "Old", "Young"))])
df[,ID:=NULL]
levels(df[,Treatment])

sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)
head(sparse_matrix)
output_vector = df[,Improved] == "Marked"
## You can see some train-error: 0.XXXXX lines followed by a number. 
## It decreases. Each line shows how well the model explains your data. 
## Lower is better.
bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nround = 10,objective = "binary:logistic")

bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nround = 5,objective = "binary:logistic")
importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)
importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst, data = sparse_matrix, label = output_vector)
# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
head(importanceRaw)
xgb.plot.importance(importance_matrix = importanceRaw)

xgb.plot.tree(feature_names = sparse_matrix@Dimnames[[2]], model = bst, n_first_tree = 2)