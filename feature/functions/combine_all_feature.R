comb_all_feature = function(feature.path="../../feature/")
{
  setwd(feature.path)
  all.file = list.files()
  train.file = all.file[grep("train",all.file)]
  test.file = all.file[grep("test",all.file)]

  
  test.feature = read.csv("feature_based_on_num_of_product_test.csv")$ID
  train.feature = read.csv("feature_based_on_num_of_product_train.csv")$ID
  for (i in 1:length(train.file)){
    file.i = train.file[i]
    col.n = strsplit(file.i,".csv")[[1]]
    feat = as.data.frame(read.csv(file.i))
    if (sum(names(feat) == "ID") == 1)
    {
      feat = feat[,setdiff(names(feat),"ID")]
    }
    names(feat) = paste0( col.n,"_",names(feat))
    train.feature = cbind(train.feature,feat)
    print(i)
  }
}