library(readr)
library(dplyr)
library(copula)

setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Fourth Quarter/RCP 10")

NOV1False <- read_csv("./dataFiles/RCP10NOV1False.csv")

NOV1True <- read_csv("./dataFiles/RCP10NOV1True.csv")

NOV3False <- read_csv("./dataFiles/RCP10NOV3False.csv")

NOV3True <- read_csv("./dataFiles/RCP10NOV3True.csv")

NOVFalseCorr <- read_csv("./dataFiles/RCP10NOVFalseCorr.csv")

NOVTrueCorr <- read_csv("./dataFiles/RCP10NOVTrueCorr.csv")

set.seed(123)

total_users = (sum(NOV1False[,3])+sum(NOV1True[,3])) # total number of users in a month

# NOV1False$wt = 0
# 
# NOV1False[,"wt"] = NOV1False[,3]/sum(NOV1False[,3])

# random_bins = sample_n(NOV1False, 
#          size = sum(NOV1False[,3]), 
#          replace = TRUE, 
#          weight = NOV1False$wt)

# start of uniform sampling from bins #
det1_samples_false = NULL
for (i in 1:dim(NOV1False)[1])
{
det1_samples_false = append(det1_samples_false, runif(n=NOV1False$Count[i], min = NOV1False$`Lower bound`[i], max = NOV1False$`Upper bound`[i]))
}

det1_samples_true = NULL
for (i in 1:dim(NOV1True)[1])
{
  det1_samples_true = append(det1_samples_true, runif(n=NOV1True$Count[i], min = NOV1True$`Lower bound`[i], max = NOV1True$`Upper bound`[i]))
}

det3_samples_false = NULL
for (i in 1:dim(NOV3False)[1])
{
  det3_samples_false = append(det3_samples_false, runif(n=NOV3False$Count[i], min = NOV3False$`Lower bound`[i], max = NOV3False$`Upper bound`[i]))
}

det3_samples_true = NULL
for (i in 1:dim(NOV3True)[1])
{
  det3_samples_true = append(det3_samples_true, runif(n=NOV3True$Count[i], min = NOV3True$`Lower bound`[i], max = NOV3True$`Upper bound`[i]))
}

# end of uniform sampling #

det_samples = matrix(ncol = 3,nrow = total_users)
colnames(det_samples) = c("det1", "det3", "targetBehavior")

det_samples[,"det1"] = c(det1_samples_false,det1_samples_true)
det_samples[,"det3"] = c(det3_samples_false,det3_samples_true)
det_samples[,"targetBehavior"] = c(rep(0,sum(NOV1False[,3])), rep(1,sum(NOV1True[,3])))

# start of copulas #

norm_cop_false = normalCopula(param = c(NOVFalseCorr[5,3]), dim=2, dispstr = "un")

norm_cop_true = normalCopula(param = c(NOVTrueCorr[5,3]), dim=2, dispstr = "un")

t_cop_true = tCopula(param = c(NOVTrueCorr[5,3]), dim=2, dispstr = "un")

copula_samples_false = rCopula(n = sum(NOV1False$Count), copula = norm_cop_false)

#copula_samples_true = rCopula(n = sum(NOV1True$Count), copula = norm_cop_true) # doesn't produce enough correlation

copula_samples_true = rCopula(n = sum(NOV1True$Count), copula = t_cop_true) #this turns out to be a better fit

det1_dist_false = quantile(x = det1_samples_false, copula_samples_false[,1]) # pull out values from underlying distribution based on correlation structure produced by copula

det3_dist_false = quantile(x = det3_samples_false, copula_samples_false[,2])

det1_dist_true = quantile(x = det1_samples_true, copula_samples_true[,1])

det3_dist_true = quantile(x = det3_samples_true, copula_samples_true[,2])

det_profile_est = matrix(ncol = 3, nrow = total_users)
colnames(det_profile_est) = c("det1", "det3", "targetBehavior")

det_profile_est[,"det1"] = c(det1_dist_false,det1_dist_true)
det_profile_est[,"det3"] = c(det3_dist_false,det3_dist_true)
det_profile_est[,"targetBehavior"] = c(rep(0,sum(NOV1False[,3])), rep(1,sum(NOV1True[,3])))

cor(det_profile_est[det_profile_est[,"targetBehavior"]==0,1:2], method = c("spearman"))

cor(det_profile_est[det_profile_est[,"targetBehavior"]==1,1:2], method = c("spearman"))


## Some EDA ##

hist(det1_samples_false, breaks = 50)

hist(det1_samples_true, breaks = 10)

hist(det_samples[,"det1"], breaks = 50)

hist(det3_samples_false, breaks = 50)

hist(det3_samples_true, breaks = 10)

hist(det_samples[,"det3"], breaks = 50)


hist(det1_samples_false, col=rgb(1,0,0,0.5), main="Overlapping Histogram of FALSE and TRUE", breaks = 50, freq = F)
hist(det1_samples_true, col=rgb(0,0,1,0.5), add=T, breaks = 50, freq = F)
box()

# end of EDA #

## 75% of the sample size is used for training and rest for test data
smp_size <- floor(0.75 * nrow(det_profile_est))

## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(det_profile_est)), size = smp_size)

train <- det_profile_est[train_ind, ]
test <- det_profile_est[-train_ind, ]

write.csv(train, file = "train.csv")
write.csv(test, file = "test.csv")



