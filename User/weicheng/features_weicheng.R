library(dplyr)
library(data.table)

######### Feature1 var15 ##########
f_var15_ratio <- function(trn, tst){
  r <- trn %>% select(var15, TARGET) %>% 
    group_by(var15) %>%
    summarise(var15_ratio = sum(TARGET)/length(TARGET))
  trn <- trn %>% select(ID, var15) %>%
    left_join(r) %>%
    select(-var15)
  tst <- tst %>% select(ID, var15) %>%
    left_join(r) %>%
    select(-var15)
  list(trn = trn, tst = tst)
}
# res = f_var15(trn, tst)
# head(res$trn)
# head(res$tst)

######### Feature 2 var38 ##########
f_var38_peak <- function(trn, tst){
  num_peak = as.numeric(names(sort(table(trn$var38), decreasing = TRUE)[1]))
  trn <- trn %>% select(ID, var38) %>%
    mutate(var_38_peak = as.numeric(var38 == num_peak)) %>%
    select(-var38)
  tst <- tst %>% select(ID, var38) %>%
    mutate(var_38_peak = as.numeric(var38 == num_peak)) %>%
    select(-var38)
  list(trn = trn, tst = tst)
}
# res = f_var38_peak(trn, tst)

######### Feature 3 var38 ##########
f_var38_ratio <- function(trn, tst){
  num_peak = as.numeric(names(sort(table(trn$var38), decreasing = TRUE)[1]))
  ## remove the peak and take the log
  tmp = trn %>% select(var38, TARGET) %>% 
    filter(var38 != num_peak) %>%
    mutate(var38 = log(var38)) %>% data.table
  ## bandwidth selection
  # bb=density(tmp[TARGET==0, var38], bw="ucv"); bb$bw
  # bb=density(tmp[TARGET==1, var38], bw="ucv")
  dens0 = density(tmp[TARGET==0, var38], bw=0.078)
  dens1 = density(tmp[TARGET==1, var38], bw=0.114)
  f <- function(x){
    idx0 = findInterval(x, dens0$x)
    idx1 = findInterval(x, dens1$x)
    
    if(idx0 != 0 & idx0 != length(dens0$x)){
      den0 = (dens0$y[idx0] + dens0$y[idx0+1])/2
    } else
      if(idx0 == 0)
        den0 = dens0$y[idx0+1]/2 else
          den0 = dens0$y[idx0]/2
    if(idx1 != 0 & idx1 != length(dens1$x)){
      den1 = (dens1$y[idx1] + dens1$y[idx1+1])/2
    } else
      if(idx1 == 0)
        den1 = dens1$y[idx1+1]/2 else
          den1 = dens1$y[idx1]/2
    den1/den0
  }
  trn <- trn %>% select(ID, var38) %>%
    mutate(var38 = log(var38)) %>%
    mutate(var38_ratio = sapply(var38, f)) %>%
    select(-var38)
  tst <- tst %>% select(ID, var38) %>%
    mutate(var38 = log(var38)) %>%
    mutate(var38_ratio = sapply(var38, f)) %>%
    select(-var38)
  list(trn = trn, tst = tst)
}
# res = f_var38_ratio(trn, tst)

######### Feature 4 var4 ##########
# f_var4 <- function(trn, tst){
#   
# }