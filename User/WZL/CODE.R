# train.d = read.csv("train.csv")
# test.d = read.csv("test.csv")
# save(train.d,file = "train.Rdata")
# save(test.d,file = "test.Rdata")
# setwd("C:/Users/wangzl/Dropbox/2016_spring/502X/KAGGAL/data/")
library(ks)
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

cor.result = ((abs(cor.result) /sum(abs(cor.result)))[-ncol(train.d)])^2


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
non.0.dist.given.1<- non.0.dist.given.0 <- list()
test.d[test.d[,2] == -999999,2] = NA
for (i in 1:ncol(train.d))
{
  candidate.0 = train.d[!ind,i]
  candidate.1 = train.d[ind,i]
  
  cand.0.non.0 = candidate.0[candidate.0!=0]
  cand.1.non.0 = candidate.1[candidate.1!=0]
  
  if (length(unique(cand.0.non.0))<4)
  {
    type.i = "discrete"
    y.prob = table(cand.0.non.0)/length(cand.0.non.0)
    x.prob = as.double(names(y.prob))
    non.0.dist.given.0[[i]] = list(
      type.i = type.i,
      x.prob = x.prob,
      y.prob = y.prob
    )
    
    type.i = "discrete"
    y.prob = table(cand.1.non.0)/length(cand.0.non.0)
    x.prob = as.double(names(y.prob))
    non.0.dist.given.1[[i]] = list(
      type.i = type.i,
      x.prob = x.prob,
      y.prob = y.prob
    )
  } else {
    type.i = "continuous"
    # aa = kde(cand.0.non.0-min(test.d[,i]))
    # non.0.dist.given.0[[i]] = list(
    #   type.i = type.i,
    #   model = aa
    # )
    cand.0.non.0[cand.0.non.0 == -999999] = NA
    mean.i = mean(cand.0.non.0-min(test.d[,i],na.rm = TRUE),na.rm = TRUE)
    sd.i = sd(cand.0.non.0-min(test.d[,i],na.rm = TRUE),na.rm = TRUE)
    beta.i = sd.i^2/mean.i
    alpha.i = mean.i/beta.i
    singular = 0
    if (sd.i == 0 | is.na(sd.i))
      singular = 1
    non.0.dist.given.0[[i]] = list(
      type.i = type.i,
      mean = mean.i,
      sd = sd.i,
      alpha = alpha.i,
      beta = beta.i,
      singular = singular
    )
    
    cand.1.non.0[cand.1.non.0 == -999999] = NA
    mean.i = mean(cand.1.non.0-min(test.d[,i],na.rm = TRUE),na.rm = TRUE)
    sd.i = sd(cand.1.non.0-min(test.d[,i],na.rm = TRUE),na.rm = TRUE)
    singular = 0
    if (sd.i == 0 | is.na(sd.i))
      singular = 1
    beta.i = sd.i^2/mean.i
    alpha.i = mean.i/beta.i
    non.0.dist.given.1[[i]] = list(
      type.i = type.i,
      mean = mean.i,
      sd = sd.i,
      alpha = alpha.i,
      beta = beta.i,
      singular = singular)
  }
  
  if (length(unique(cand.1.non.0))<30)
  {
    # type.i = "discrete"
    # y.prob = table(cand.1.non.0)
    # x.prob = as.double(names(y.prob))
    # non.0.dist.given.1[[i]] = list(
    #   type.i = type.i,
    #   x.prob = x.prob,
    #   y.prob = y.prob
    # )
  } else {
    type.i = "continuous"
    # aa = kde(cand.1.non.0)
    # non.0.dist.given.1[[i]] = list(
    #   type.i = type.i,
    #   model = aa
    # )
    # cand.1.non.0[cand.1.non.0 == -999999] = NA
    # mean.i = mean(cand.1.non.0-min(test.d[,i]),na.rm = TRUE)
    # sd.i = sd(cand.1.non.0-min(test.d[,i]),na.rm = TRUE)
    # beta.i = sd.i^2/mean.i
    # alpha.i = mean.i/beta.i
    # non.0.dist.given.1[[i]] = list(
    #   type.i = type.i,
    #   mean = mean.i,
    #   sd = sd.i,
    #   alpha = alpha.i,
    #   beta = beta.i)
  }
}

center.1 = apply(train.d[ind,],2,mean)
center.1.var = apply(train.d[ind,],2,var)
ind.1.var.0 =center.1.var == 0
common.index = ((1:ncol(train.d))[!ind.1.var.0])[-1]

# The following step is based on Bayes
target.result = rep(0,nrow(test.d))
target.prob = target.result
i=1
while (i <=nrow(test.d))
{
  target.i = test.d[i,]
  if (sum(target.i[ind.1.var.0[-ncol(train.d)]] != 0)>0)
  {
    target.result[i] = 0
    # print(i)
  } else{
    target.final = 0
    for (j in common.index)
    {
      x.target.j = target.i[j] 
      if (is.na(x.target.j))
        next
      if (x.target.j == 0)
      {
        prob.y.1.x.0 <- (1-P.0) * prob.0.given.1[j] 
        prob.y.0.x.0 <- P.0 * prob.0.given.0[j] 
        prob.y.1.x = prob.y.1.x.0/(prob.y.1.x.0 +prob.y.0.x.0 )
      } else {
        model.type.0.j = non.0.dist.given.0[[j]]$type.i
        model.type.1.j = non.0.dist.given.1[[j]]$type.i
        if (model.type.1.j == "continuous"){
          x.target.j=target.i[j] - min(test.d[,j],na.rm=TRUE)+1e-5
          prob.y.1.x.n0 = (1-P.0) * (1-prob.0.given.1[j]) *  ifelse(non.0.dist.given.1[[j]]$singular == 1,
                                                                    ifelse(x.target.j == non.0.dist.given.1[[j]]$mean,1,0),dgamma(x.target.j,
                                                                                                                                  shape = non.0.dist.given.1[[j]]$alpha,
                                                                                                                                  scale = non.0.dist.given.1[[j]]$beta
                                                                    ))
          prob.y.0.x.n0 = (P.0) * (1-prob.0.given.0[j]) *  dgamma(x.target.j,
                                                                  shape = non.0.dist.given.0[[j]]$alpha,
                                                                  scale = non.0.dist.given.0[[j]]$beta
          )
          prob.y.1.x = prob.y.1.x.n0 /(prob.y.1.x.n0 +prob.y.0.x.n0 )
        } else {
          prob.y.1.x.n0 = (1-P.0) * (1-prob.0.given.1[j]) *  ifelse(sum(non.0.dist.given.1[[j]]$x.prob == x.target.j)>0,
                                                                    non.0.dist.given.1[[j]]$y.prob[which(non.0.dist.given.1[[j]]$x.prob == x.target.j)],
                                                                    0)
          prob.y.0.x.n0 = (P.0) * (1-prob.0.given.0[j]) *  ifelse(sum(non.0.dist.given.0[[j]]$x.prob == x.target.j)>0,
                                                                  non.0.dist.given.0[[j]]$y.prob[which(non.0.dist.given.0[[j]]$x.prob == x.target.j)],
                                                                  0)
          prob.y.1.x = prob.y.1.x.n0 /(prob.y.1.x.n0 +prob.y.0.x.n0 )
        }
      }
      target.final = target.final + abs(cor.result[j]) * prob.y.1.x 
      # print(paste(j,target.final))
    }
    target.final = target.final / sum(abs(cor.result[common.index]))
    # if (target.final < .5)
    # {
    #   target.result[i] = 0
    # } else if (target.final > .5)
    # {
    #   target.result[i] =1
    # } else{
    #   target.result[i] = rbinom(1,1,.5)
    # }
    target.prob[i] = target.final
    target.result[i] = rbinom(1,1,target.final)
  }
  
  i=i+1
  print(i)
}

result.submit = data.frame(ID = as.double(test.d[,1]),TARGET = as.double(target.result))
result.submit = as.double(result.submit)
# write.csv(result.submit,file = "submission.csv")
write.csv(result.submit, "../../submit/0330_wzl.csv", row.names = FALSE, quote = FALSE)


# The following step is based on frequentist
target.result = rep(0,nrow(test.d))
target.prob = target.result
i=1
while (i <=nrow(test.d))
{
  target.i = test.d[i,]
  if (sum(target.i[ind.1.var.0[-ncol(train.d)]] != 0)>0)
  {
    target.result[i] = 0
    # print(i)
  } else{
    target.final = 0
    for (j in common.index)
    {
      x.target.j = target.i[j] 
      if (is.na(x.target.j))
        next
      if (x.target.j == 0)
      {
        prob.y.1.x.0 <-  prob.0.given.1[j] 
        prob.y.0.x.0 <- prob.0.given.0[j] 
        prob.y.1.x = prob.y.1.x.0/(prob.y.1.x.0 +prob.y.0.x.0 )
      } else {
        model.type.0.j = non.0.dist.given.0[[j]]$type.i
        model.type.1.j = non.0.dist.given.1[[j]]$type.i
        if (model.type.1.j == "continuous"){
          x.target.j=target.i[j] - min(test.d[,j],na.rm=TRUE)+1e-5
          prob.y.1.x.n0 =  (1-prob.0.given.1[j]) *  ifelse(non.0.dist.given.1[[j]]$singular == 1,
                                                                    ifelse(x.target.j == non.0.dist.given.1[[j]]$mean,1,0),dgamma(x.target.j,
                                                                                                                                  shape = non.0.dist.given.1[[j]]$alpha,
                                                                                                                                  scale = non.0.dist.given.1[[j]]$beta
                                                                    ))
          prob.y.0.x.n0 = (1-prob.0.given.0[j]) *  dgamma(x.target.j,
                                                                  shape = non.0.dist.given.0[[j]]$alpha,
                                                                  scale = non.0.dist.given.0[[j]]$beta
          )
          prob.y.1.x = prob.y.1.x.n0 /(prob.y.1.x.n0 +prob.y.0.x.n0 )
        } else {
          prob.y.1.x.n0 =  (1-prob.0.given.1[j]) *  ifelse(sum(non.0.dist.given.1[[j]]$x.prob == x.target.j)>0,
                                                                    non.0.dist.given.1[[j]]$y.prob[which(non.0.dist.given.1[[j]]$x.prob == x.target.j)],
                                                                    0)
          prob.y.0.x.n0 = (1-prob.0.given.0[j]) *  ifelse(sum(non.0.dist.given.0[[j]]$x.prob == x.target.j)>0,
                                                                  non.0.dist.given.0[[j]]$y.prob[which(non.0.dist.given.0[[j]]$x.prob == x.target.j)],
                                                                  0)+1e-10
          prob.y.1.x = prob.y.1.x.n0 /(prob.y.1.x.n0 +prob.y.0.x.n0 )
        }
      }
      target.final = target.final + abs(cor.result[j]) * prob.y.1.x 
      # print(paste(j,target.final))#C
    }
    target.final = target.final / sum(abs(cor.result[common.index]))
    
    if (target.final < .5)
    {
      target.result[i] = 0
    } else if (target.final > .5)
    {
      target.result[i] =1
    } else{
      target.result[i] = rbinom(1,1,.5)
    }
    target.prob[i] = target.final
    # target.result[i] = rbinom(1,1,target.final)
  }
  
  i=i+1
  print(i)
}



### Explore the data. 
library(ggplot2)
preliminary = list()
for (i in 1:ncol(train.d))
{
  candidate=train.d[,i]
  data.i = data.frame(x=candidate,y=train.d[,337])
  table.i = NULL
  if (length(unique(candidate))<50)
    table.i = table(candidate)
  
  if (length(unique(candidate))<50){
    hist.i = ggplot(subset(data.i,x!=0)) + geom_histogram(aes(x),bins = length(unique(candidate)))+
      labs(title = paste0(round(sum(candidate!=0)/length(candidate),2)*100, "% non zero."))
  }else{
    breaks = unique(as.double(quantile(candidate[candidate!=0],seq(0.1,0.9,length=30))))
    if (length(breaks) == 1)
    {
      hist.i = table.i 
    }else{
      hist.i = ggplot(subset(data.i,x!=0)) + geom_histogram(aes(x),breaks=breaks)+
        labs(title = paste0(sum(candidate!=0),"(",round(sum(candidate!=0)/length(candidate),2)*100, "%) non zero. There are ",length(unique(candidate))," unque values."))
    }
    
  }
  
  preliminary[[i]]=list(
    name.i = colnames(train.d)[i],
    length.i = length(unique(candidate)),
    min.i = min(candidate),
    max.i = max(candidate),
    table.i = table.i,
    hist.i = hist.i,
    plot.i = ggplot(data.i) + geom_point(aes(x=x,y=y)),
    plot.y = ggplot(data.i) + geom_point(aes(x=y,y=x)) + labs(title = colnames(train.d)[i])
  )  
  print(i)
  
  
}


saveRDS(preliminary,file = "preliminary_train_only.rds")

preliminary.all = list()
for (i in 1:ncol(test.d))
{
  candidate=c(train.d[,i],test.d[,i])
  table.i = NULL
  if (length(unique(candidate))<50)
    table.i = table(candidate)
  if (length(unique(candidate))<50){
    hist.i = ggplot(subset(data.i,x!=0)) + geom_histogram(aes(x),bins = length(unique(candidate)))+
      labs(title = paste0(round(sum(candidate!=0)/length(candidate),2)*100, "% non zero."))
  }else{
    breaks = unique(as.double(quantile(candidate[candidate!=0],seq(0.1,0.9,length=30))))
    if (length(breaks) == 1)
    {
      hist.i = table.i 
    }else{
      hist.i = ggplot(subset(data.i,x!=0)) + geom_histogram(aes(x),breaks=breaks)+
        labs(title = paste0(sum(candidate!=0),"(",round(sum(candidate!=0)/length(candidate),2)*100, "%) non zero. There are ",length(unique(candidate))," unque values."))
    }
  }

  
  data.i = data.frame(x=candidate)
  
  preliminary.all[[i]]=list(
    name.i = colnames(train.d)[i],
    length.i = length(unique(candidate)),
    min.i = min(candidate),
    max.i = max(candidate),
    table.i = table.i,
    hist.i = hist.i
  )  
  print(i)
}
saveRDS(preliminary.all,file = "preliminary_train_test.rds")

preliminary = readRDS("preliminary_train_only.rds")
preliminary.all = readRDS("preliminary_train_test.rds")

result.all = rbind(
  name.var = unlist(lapply(preliminary.all,function(x) x$name.i)),
  length.var =  as.double(unlist(lapply(preliminary.all,function(x) x$length.i))),
  min.var = as.double(unlist(lapply(preliminary.all,function(x) x$min.i))),
  max.var = as.double(unlist(lapply(preliminary.all,function(x) x$max)))
)

result = rbind(
  name.var = unlist(lapply(preliminary,function(x) x$name.i)),
  length.var =  as.double(unlist(lapply(preliminary,function(x) x$length.i))),
  min.var = as.double(unlist(lapply(preliminary,function(x) x$min.i))),
  max.var = as.double(unlist(lapply(preliminary,function(x) x$max)))
)
result = result[,1:336]
plot(as.double(result[2,1:336]),as.double(result.all[2,]))
abline(0,1)


cutoff = 30
ind.discrete.var = as.double(result[2,1:336]) < cutoff
plot(as.double(result[2,ind.discrete.var]),as.double(result.all[2,ind.discrete.var]))
abline(0,1)
result[1,as.double(result[2,ind.discrete.var])<as.double(result.all[2,ind.discrete.var])]

