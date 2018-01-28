library(readr)
library(tibble)
library(dplyr)
library(magrittr)
library(stringr)
library(boot)
library(MASS)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(tree)
library(pROC)
library(e1071)
library(DMwR)

set.seed(123)

setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Fourth Quarter/RCP 10/dataFiles")
  
# list.of.dataframes <- list()
# fileNames <- c()
# detNum <- c()
# reqdDetNum <- c(9:11)
# samp = c()
# 
# for(i in seq_along(list.files())){
#   fileNames[i] <- list.files()[i]
#   detNum[i] <- substr(fileNames[i], 9, 10)
#   list.of.dataframes[[i]] <- read.csv(list.files()[i])
#   names(list.of.dataframes[[i]])[1] <- "lower_bound"
#   names(list.of.dataframes[[i]])[2] <- "upper_bound"
#   names(list.of.dataframes[[i]])[3] <- "count"
#   
#   if (length(grep("9|10|11",detNum[i], value=TRUE))!=0)
#   { 
#     list.of.dataframes[[i]] = list.of.dataframes[[i]][list.of.dataframes[[i]]$count!=0,]
#     list.of.dataframes[[i]]$upper_bound = trunc(list.of.dataframes[[i]]$upper_bound)
#     list.of.dataframes[[i]]$lower_bound = NULL
#     names(list.of.dataframes[[i]])[1] = "days"
#   }
#   else if (grepl("False", fileNames[i]))
#   {
#     df_name = paste("det",i, "False",sep = "_")
#   }
# 
# }

integer_detectors <- c("9", "10", "11a", "11b")
other_detectors <- c("1","2a","2b","2c","2d","3","4a","4b","4c","4d","5","6a","6b","6c","6d","7","8a","8b","8c","8d")
all_detectors <- c(other_detectors, integer_detectors)

dataframes.for.true <- list()
dataframes.for.false <- list()
dataframes.for.integer.true <- list()
dataframes.for.integer.false <- list()
correlations.for.true <- list()
correlations.for.false <- list()

for(month in c("NOV")){
  for(detector in all_detectors){
    filename <- str_c("RCP10", month, detector, "True", ".csv")
    dataframes.for.true[[str_c(month, detector)]] <- read_csv(filename)
    filename <- str_c("RCP10", month, detector, "False", ".csv")
    dataframes.for.false[[str_c(month, detector)]] <- read_csv(filename)
  }
  correlations.for.true[[month]] <- read_csv(str_c("RCP10", month, "TrueCorr.csv"))
  correlations.for.false[[month]] <- read_csv(str_c("RCP10", month, "FalseCorr.csv"))
  
}

num_false = sum(dataframes.for.false[[str_c(month, "1")]][["Count"]])
num_true = sum(dataframes.for.true[[str_c(month, "1")]][["Count"]])
total_pop = num_false + num_true

unif.samples.for.false <- list()
unif.samples.for.true <- list()
all.unif.samples <- list()
dist.for.false <- list()
dist.for.true <- list()
dist.all <- list()


for (detector in other_detectors)
{
unif.samples.for.false[[str_c(month, detector)]] = runif(n = sum(dataframes.for.false[[str_c(month, detector)]][["Count"]]),
                                             min = rep(dataframes.for.false[[str_c(month, detector)]][["Lower bound"]], dataframes.for.false[[str_c(month, detector)]][["Count"]]),
                                             max = rep(dataframes.for.false[[str_c(month, detector)]][["Upper bound"]], dataframes.for.false[[str_c(month, detector)]][["Count"]]))
  
unif.samples.for.true[[str_c(month, detector)]] = runif(n = sum(dataframes.for.true[[str_c(month, detector)]][["Count"]]),
                                                    min = rep(dataframes.for.true[[str_c(month, detector)]][["Lower bound"]], dataframes.for.true[[str_c(month, detector)]][["Count"]]),
                                                    max = rep(dataframes.for.true[[str_c(month, detector)]][["Upper bound"]], dataframes.for.true[[str_c(month, detector)]][["Count"]]))

all.unif.samples[[str_c(month, detector)]] = c(unif.samples.for.false[[str_c(month, detector)]], unif.samples.for.true[[str_c(month, detector)]])

}

for (detector in integer_detectors)
{
  dist.for.false[[str_c(month, detector)]] = dataframes.for.false[[str_c(month, detector)]][dataframes.for.false[[str_c(month, detector)]]["Count"]!=0,]
  dist.for.false[[str_c(month, detector)]]["Upper bound"] = trunc(dist.for.false[[str_c(month, detector)]]["Upper bound"])
  dist.for.false[[str_c(month, detector)]]["Lower bound"] = NULL
  
  dist.for.true[[str_c(month, detector)]] = dataframes.for.true[[str_c(month, detector)]][dataframes.for.true[[str_c(month,detector)]]["Count"]!=0,]
  dist.for.true[[str_c(month, detector)]]["Upper bound"] = trunc(dist.for.true[[str_c(month, detector)]]["Upper bound"])
  dist.for.true[[str_c(month, detector)]]["Lower bound"] = NULL
  
  dist.for.false[[str_c(month, detector)]] = rep(dist.for.false[[str_c(month, detector)]][["Upper bound"]], times = dist.for.false[[str_c(month, detector)]][["Count"]])  
  dist.for.true[[str_c(month, detector)]] = rep(dist.for.true[[str_c(month, detector)]][["Upper bound"]], times = dist.for.true[[str_c(month, detector)]][["Count"]])
  
  dist.all[[str_c(month, detector)]] = c(dist.for.false[[str_c(month, detector)]], dist.for.true[[str_c(month, detector)]])
}


all_unif_samples_df <- as.data.frame(all.unif.samples)

all_int_dist_df <- as.data.frame(dist.all)

all_distributions_df <- cbind(all_unif_samples_df, all_int_dist_df)

all_distributions_df$targetBehavior = rep(c(0,1), times=c(num_false,num_true))

# Histograms

xf = hist(all_distributions_df$NOV11b[all_distributions_df$targetBehavior==0])
xf$density = (xf$counts/sum(xf$counts))*100
xt = hist(all_distributions_df$NOV11b[all_distributions_df$targetBehavior==1])
xt$density = (xt$counts/sum(xt$counts))*100
plot(xf, col=rgb(0,0,1,0.5), main="Histogram of FALSE and TRUE", freq = F)
plot(xt, col=rgb(1,0,0,0.5), add=T, freq = F)
box()

# end of Histograms

# cutoff plots

par(mfrow=c(1,1))
for (detector in all_detectors){
plot(all_distributions_df[,str_c("NOV",detector)],all_distributions_df$targetBehavior)
}
# end of cutoff plots



# ## 75% of the sample size is used for training and rest for test data
# smp_size <- floor(0.75 * nrow(all_distributions_df))
# 
# ## set the seed to make your partition reproductible
# train_ind <- sample(seq_len(nrow(all_distributions_df)), size = smp_size)
# 
# train <- all_distributions_df[train_ind, ]
# test <- all_distributions_df[-train_ind, ]


splitIndex <- createDataPartition(all_distributions_df$targetBehavior, p = .70, list = FALSE, times = 1)
trainSplit <- all_distributions_df[ splitIndex,]
testSplit <- all_distributions_df[-splitIndex,]

prop.table(table(trainSplit$targetBehavior))
prop.table(table(testSplit$targetBehavior))

table(trainSplit$targetBehavior)
table(testSplit$targetBehavior)

tree_fit1 = rpart(targetBehavior ~ ., method = "class", data = trainSplit, control = list(maxdepth = 4))

printcp(tree_fit1) # display the results
plotcp(tree_fit1) # visualize cross-validation results
summary(tree_fit1) # detailed summary of splits
par(xpd=NA)
plot(tree_fit1)
text(tree_fit1, use.n=TRUE, all=TRUE)

ctrl <- trainControl(method = "maxdepth = 4")
class_model <- train(as.factor(targetBehavior) ~ ., data = trainSplit, method = "rpart2", trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'targetBehavior']
pred <- predict(object = tree_fit1, newdata = testSplit[,predictors])
#pred <- predict(object = tree_fit1, newdata = testSplit[,predictors])
# evaluate the model's performance

auc <- roc(testSplit$targetBehavior, pred)
print(auc)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)



trainSplit$targetBehavior <- as.factor(trainSplit$targetBehavior)
trainSplit <- SMOTE(targetBehavior ~ ., trainSplit, perc.over = 100, perc.under=200)
trainSplit$targetBehavior <- as.numeric(trainSplit$targetBehavior)
prop.table(table(trainSplit$targetBehavior))




splitIndex <- createDataPartition(all_int_dist_df$targetBehavior, p = .70, list = FALSE, times = 1)
trainSplit <- all_int_dist_df[ splitIndex,]
testSplit <- all_int_dist_df[-splitIndex,]

prop.table(table(trainSplit$targetBehavior))
prop.table(table(testSplit$targetBehavior))

table(trainSplit$targetBehavior)
table(testSplit$targetBehavior)

tree_fit1 = rpart(targetBehavior ~ ., method = "class", data = trainSplit, control = list(maxdepth = 4))

printcp(tree_fit1) # display the results
plotcp(tree_fit1) # visualize cross-validation results
summary(tree_fit1) # detailed summary of splits
par(xpd=NA)
plot(tree_fit1)
text(tree_fit1, use.n=TRUE, all=TRUE)



