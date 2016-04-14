library(caret)
library(mlbench)
data(Sonar)
dim(Sonar)
set.seed(107)
## By default, createDataPartition does a stratified random split of the data.
inTrain <- createDataPartition(y = Sonar$Class,
                                p = .75,
                                list = FALSE)
dim(inTrain)
## The format of the results
## The output is a set of integers matrix for the rows of Sonar
## that belong in the training set.
str(inTrain)
# > head(inTrain)
# Resample1
# [1,]         1
# [2,]         2
# [3,]         3
# [4,]         6
# [5,]         7
# [6,]         9

training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]

###### Basic Parameter Tuning
### The function trainControl can be used to specifiy the type of resampling:

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(825)
## available methods: http://topepo.github.io/caret/modelList.html
## Here to illustrate, we will fit a boosted tree model via the gbm package.
gbmFit1 <- train(Class ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1