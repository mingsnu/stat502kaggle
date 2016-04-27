### depth and minimum child weight visulization
dm = read.csv("../../tuning/old_all_feature_round2/xg_auc1.csv")
depth = unique(dm$Depth)
mcw = unique(dm$Min_child_weight)
auc = matrix(dm$auc_max, nrow = length(depth), ncol = length(mcw), byrow = TRUE)
std = matrix(dm$std, nrow = length(depth), ncol = length(mcw), byrow = TRUE)
image(depth, mcw, auc, main = "auc")
image(depth, mcw, std, main = "std")

### gamma plot
ga = read.csv("tuning/old_all_feature_round2/xg_auc2.csv")
par(mfrow=c(2,1))
plot(auc_max~gamma, data= ga, type="l")
plot(std~gamma, data= ga, type="l")

### subsample(row sample), colsample_bytree
rc = read.csv("tuning/old_all_feature_round2/xg_auc3.csv")
rsample = unique(rc$r_sample)
csample = unique(rc$c_sample)
auc = matrix(rc$auc_max, nrow = length(rsample), ncol = length(csample), byrow = TRUE)
std = matrix(rc$std, nrow = length(rsample), ncol = length(csample), byrow = TRUE)
image(rsample, csample, auc, main = "auc")
image(rsample, csample, std, main = "std")

### scale_positive_weight
spw =  read.csv("tuning/old_all_feature_round2/xg_auc4.csv")
par(mfrow=c(2,1))
plot(auc_max~scale_pos_weight, data = spw, type="l")
plot(std~scale_pos_weight, data = spw, type="l")

### eta
eta =  read.csv("tuning/old_all_feature_round2/xg_auc5.csv")
par(mfrow=c(2,1))
plot(auc_max~eta, data= eta, type="l")
plot(std ~ eta, data= eta, type="l")
