# train.d = read.csv("train.csv")
# test.d = read.csv("test.csv")
# save(train.d,file = "train.Rdata")
# save(test.d,file = "test.Rdata")
load("../../data/train.Rdata")
load("../../data/test.Rdata")
# 
# col.num = NULL
# for (i in 1:ncol(train.d))
# {
#   aa = unique(train.d[,i])
#   if (length(aa) == 1)
#   {
#     col.num = c(col.num,i)
#   }
# }
# train.d = train.d[,-col.num]
# test.d = test.d[,-col.num]
# train.d = as.matrix(train.d)
# test.d  = as.matrix(test.d)
length.train.d = rep(0,ncol(train.d))
for (i in 1:ncol(train.d))
{
  length.train.d[i] = length(unique(train.d[,i]))
}
cor.result = NULL
for (i in 1:ncol(train.d))
{
  cor.result = c(cor.result,cor(train.d[,i],train.d[,ncol(train.d)]))
}

cor.result = (abs(cor.result) /sum(abs(cor.result)))[-ncol(train.d)]


# useless
for (i in 1:nrow(test.d))
{
  target.i = test.d[i,]
  target.final = 0
  a = proc.time()
  for (j in 2:ncol(test.d))
  {
    t.j = as.double(target.i[j])
    dis.j.train = abs(t.j - train.d[,j])
    t.index = sort(dis.j.train,index.return=TRUE)$ix[1:10]
    target.final = target.final + cor.result[j-1] * mean(train.target[t.index])
  
  }
  b=proc.time()
  (b-a)[3]
  a = proc.time()
  target.i.ma = abs(matrix(rep(target.i,each = nrow(train.d)),nrow =nrow(train.d) ) - train.d[,-ncol(train.d)])
  for (j in 2:ncol(test.d))
  {
    dis.j.train = target.i.ma[,j]
    t.index = sort(dis.j.train,index.return=TRUE)$ix[1:10]
    target.final = target.final + cor.result[j-1] * mean(train.target[t.index])
  }
  b=proc.time()
  (b-a)[3]
  if (target.final < .5)
  {
    target.result = c(target.result,0)
  } else if (target.final > .5)
  {
    target.result = c(target.result,1)
  } else{
    target.result = c(target.result,rbinom(1,1,.5))
  }
  print(i)
}

# The result for this method is not valid.
ind = train.d[,ncol(train.d)] == 1
center.1 = apply(train.d[ind,],2,mean)
center.1.var = apply(train.d[ind,],2,var)
ind.1.var.0 =center.1.var == 0
common.index = ((1:ncol(train.d))[!ind.1.var.0])[-1]
center.0 = apply(train.d[!ind,],2,mean)
center.0.var = apply(train.d[!ind,],2,var)
target.result = rep(0,nrow(test.d))
for (i in 1:nrow(test.d))
{
  target.i = test.d[i,]
  if (sum(target.i[ind.1.var.0[-ncol(train.d)]] == 0)==sum(ind.1.var.0[-ncol(train.d)]))
  {
    target.result[i] = 1
  } 
  
  
  print(i)
}



# We should split the observations based on 0 or non-0
ind = train.d[,ncol(train.d)] == 1
  #' Step 0. obtain the marginals.
P.0 = mean(train.d[,ncol(train.d)] == 0)
prob.0 = apply(train.d==0,2,mean)
  #' Step 1. calculate the P(x_i=0|result) based on the train data. Thus, the last element is for the target variable
prob.0.given.1 <- prob.0.given.0 <- rep(0,ncol(train.d))
prob.0.given.1 = apply(train.d[ind,]==0,2,mean)
prob.0.given.0 = apply(train.d[!ind,]==0,2,mean)
  #' Step 2. calculate the mean and variance for x if x is not 0
center.1 <- center.1.var <- center.0 <- center.0.var <- rep(NA,ncol(train.d))
for (i in 1:ncol(train.d))
{
  candidate.0 = train.d[!ind,i]
  candidate.1 = train.d[ind,i]
  # for candidate 0
  par(mfrow = c(2,1))
  candidate.0.posi = candidate.0[candidate.0!=0]
  if (length(unique(candidate.0.posi))>1)
  {
    hist(candidate.0.posi,main = i)
  }
  # for candidate 1
  candidate.1.posi = candidate.1[candidate.1!=0]
  if (length(unique(candidate.1.posi))>1)
  {
    hist(candidate.1.posi)
  }
}

for (i in 1:ncol(train.d))
{
  train.0.i = train.0[,i]
  
  
}
center.1 = apply(train.d[ind,],2,mean)
center.1.var = apply(train.d[ind,],2,var)
ind.1.var.0 =center.1.var == 0
common.index = ((1:ncol(train.d))[!ind.1.var.0])[-1]
center.0 = apply(train.d[!ind,],2,mean)
center.0.var = apply(train.d[!ind,],2,var)
target.result = rep(0,nrow(test.d))
for (i in 1:nrow(test.d))
{
  target.i = test.d[i,]
  if (sum(target.i[ind.1.var.0[-ncol(train.d)]] != 0)>0)
  {
    target.result[i] = 0
  } else{
    target.final = 0
    for (j in common.index)
    {
      target.final = target.final + cor.result[j] * (0/(abs(target.i[j] - center.0[j])/center.0.var[j]) + 1/(abs(target.i[j] - center.1[j])/center.1.var[j]))/
        (1/(abs(target.i[j] - center.0[j])/center.0.var[j]) + 1/(abs(target.i[j] - center.1[j])/center.1.var[j]))
      print(target.final)
    }
    target.final = target.final / sum(cor.result[common.index])
    if (target.final < .5)
    {
      target.result[i] = 0
    } else if (target.final > .5)
    {
      target.result[i] =1
    } else{
      target.result[i] = rbinom(1,1,.5)
    }
  }
  
  
  print(i)
}

result.submit = data.frame(ID = test.d[,1],TARGET = target.result)
write.csv(result.submit,file = "submission.csv")