library(dplyr)
library(plyr)
library(reshape2)
library(Matrix)
library(readr)

# 
# splineKdeDistn <- function(detHist, bw, xw=.01, zeroSD=.1){
#   detHist <- histDF
#   zeroSD<-.1
#   bw <- .1
#   xw <- .01
#   # mean and st based on hist -- needed for optional adjustment
#   xMean <- sum(detHist$mid*detHist$Count)/sum(detHist$Count)
#   xSD <- max(sqrt(sum(detHist$mid^2*detHist$Count)/sum(detHist$Count) - xMean^2) , zeroSD)
#   
#   # rescale midpoint
#   detHist$mid <- detHist$mid/xSD
#   
#   kd <- density(x=detHist$mid, weights=detHist$Count/sum(detHist$Count), bw=.1)
#   xRange <- range(kd$x)
#   # PDF: smoothing spline estimate of KDE -- FAST
#   dkd.s <- splinefun(kd$x, kd$y)
#   
#   # spline smoothing evaluation points 
#   xPts <- seq(min(kd$x),max(kd$x),xw)
#   
#   # CDF #3: smoothing spline estimate of sums of normal CDFs
#   pkde <- Vectorize(function(x){ sum(detHist$Count*pnorm((x - detHist$mid)/bw))/sum(detHist$Count)}, "x")
#   pkd.s3 <- splinefun(xPts, pkde(xPts), method="monoH.FC")
#   
#   # quantile using smoothing spline based based on CDF using sums of normals -- FAST
#   
#   qkd.s3 <- splinefun(pkd.s3(xPts), xPts, method="monoH.FC")
#   
#   list(xRange=xRange, xPts=xPts, kd=kd, dkd.s=dkd.s, pkd.s3=pkd.s3, qkd.s3=qkd.s3, xMean=xMean, xSD=xSD)
# }



splineKdeDistn <- function(detHist, bw, xw=.01, zeroSD=.1){
  # mean and st based on hist -- needed for optional adjustment
  xMean <- sum(detHist$mid*detHist$Count)/sum(detHist$Count)
  xSD <- max(sqrt(sum(detHist$mid^2*detHist$Count)/sum(detHist$Count) - xMean^2) , zeroSD)

  # rescale midpoint
  detHist$mid <- detHist$mid/xSD

  kd <- density(x=detHist$mid, weights=detHist$Count/sum(detHist$Count), bw=bw)
  xRange <- range(kd$x)
  # PDF: smoothing spline estimate of KDE -- FAST
  dkd.s <- splinefun(kd$x, kd$y)

  # spline smoothing evaluation points
  xPts <- seq(min(kd$x),max(kd$x),xw)

  # CDF #3: smoothing spline estimate of sums of normal CDFs
  pkde <- Vectorize(function(x){ sum(detHist$Count*pnorm((x - detHist$mid)/bw))/sum(detHist$Count)}, "x")
  pkd.s3 <- splinefun(xPts, pkde(xPts), method="monoH.FC")

  # quantile using smoothing spline based based on CDF using sums of normals -- FAST
  qkd.s3 <- splinefun(pkd.s3(xPts), xPts, method="monoH.FC")

  list(xRange=xRange, xPts=xPts, kd=kd, dkd.s=dkd.s, pkd.s3=pkd.s3, qkd.s3=qkd.s3, xMean=xMean, xSD=xSD)
}

#---------------------------------------------------------------------------------------------------------------------
# ------------ Start Code from Ted 3/7/2017 (This is the code from the RCP12_Input_Data.R file)--------------------
# input parameters for generating RCP14 detector data
#---------------------------------------------------------------------------------------------------------------------
#EVal2 <- c("AGG")
WeekNum <- c(5:9)
classVal <- c("NoChange")


MeanStd_Change <- read_csv("MeanStd_Change.csv")
detNames <- MeanStd_Change[,1]
csvFiles <- apply(expand.grid("Week", WeekNum,  "_MeanStd_",classVal,".csv"), 1, paste, collapse="")

MeanStdDF <- data.frame(class=character(0), var=character(0), mean=numeric(0), std=numeric(0))
for(i in 1:length(csvFiles)){
  tmpCsv <- read.csv(csvFiles[i],stringsAsFactors=FALSE)
  #tmpCsv$Detector <- paste("x",detNames[i], sep="")
  names(tmpCsv) <- c("detector", "mean", "std")
  MeanStdDF <- rbind(MeanStdDF,
                     data.frame(week=as.character(WeekNum[i]),class=as.character("NoChange"),
                                tmpCsv, stringsAsFactors=FALSE) )
}
MeanStdChange <- data.frame(week="TrainPeriod",class=as.character("Change"), MeanStd_Change, stringsAsFactors=FALSE) 
colnames(MeanStdChange) <- c("week" ,"class","detector","mean","std") 
MeanStdAll <- rbind(MeanStdChange, MeanStdDF)

#-----------------------------------------------------
# detector histogram / binned data
# NOTE: interval of form: (lower,upper]
#-----------------------------------------------------
detNames <- c("21f", "21h", "22f")
#Original Files
csvFiles <- apply(expand.grid("Week", WeekNum,  "_det0", detNames,"_",classVal,".csv"), 1, paste, collapse="")
gridVarClass <- expand.grid(paste("det",detNames,sep=""), classVal, WeekNum)


histDF <- data.frame(class=logical(0), var=character(0), lower=numeric(0), 
                     upper=numeric(0), mid=numeric(0), n=numeric(0))
histDF <- NULL
for(i in 1:length(csvFiles)){
  tmpCsv <- read.csv(csvFiles[i],stringsAsFactors=FALSE, header = TRUE)
  tmpCsv <- tmpCsv[complete.cases(tmpCsv),]
  tmpCsv <- tmpCsv[,c(1,2,3)]
  names(tmpCsv) <- c("LowerBound", "UpperBound", "Count")
  # if(as.character(gridVarClass[i,1])=="det1a"){
  #   tmpCsv <- filter(tmpCsv, LowerBound==0 | UpperBound==1)
  #   tmpCsv[tmpCsv$LowerBound==0, "UpperBound"] <- 0
  #   tmpCsv[tmpCsv$UpperBound==1, "LowerBound"] <- 1
  # }
  tmpCsv$mid <- (tmpCsv$LowerBound + tmpCsv$UpperBound)/2
  histDF <- rbind(histDF, 
                  data.frame(class=as.character(gridVarClass[i,2]),week=as.character(gridVarClass[i,3]), 
                             detector=as.character(gridVarClass[i,1]), tmpCsv[,c(1:4)], stringsAsFactors=FALSE) )
}

#-------------------------
# KDEs for class=FALSE
# ListMonthKDs = list of list; nested list, where outer list is over months and inner list is over variables
#-------------------------

ListWeekKDs <- vector("list", 1)

detNames <- c("det21f", "det21h", "det22f")
numX <- length(detNames)
#FilteredHist <- filter(histDF,  class=="NoChange" & week==5 & detector==detNames[1])
for(m in 5:5){
  # given month
  kd.week <- vector("list", numX)
  WeekVal <- m
  for(i in 1:numX){
    tmpKD <- splineKdeDistn( filter(histDF, class=="NoChange" & week==m & detector==detNames[i]), bw=.05, xw=.01, zeroSD=.1)
    kd.week[[i]] <- list(x=detNames[i], 
                          df=tmpKD$dkd.s,
                          pf=tmpKD$pkd.s3,
                          qf=tmpKD$qkd.s3,
                          sd=tmpKD$xSD)
  }
  names(kd.week) <- detNames
  ListWeekKDs[[m]] <- kd.week
}


# check Ns by class and var
#histDF %>% group_by(var, class) %>% summarize( nSize=sum(n))
ddply(histDF, .(var,class), summarize, nSize=sum(Count))
#    var     class nSize
# 1  det10a     F  3185
# 2  det10a     T    27
# 3  det11a     F  3166
# 4  det11a     T    19
# 5  det11b     F  3163
# 6  det11b     T    21
# 7   det1a     F  3165
# 8   det1a     T    19
# 9   det2a     F  3175
# 10  det2a     T    20
# 11  det2b     F  3173
# 12  det2b     T    19
# 13  det2c     F    21
# 14  det2c     T    20
# 15  det2d     F  3172
# 16  det2d     T    21
# 17  det3a     F    20
# 18  det3a     T    21
# 19  det4a     F  3174
# 20  det4a     T    20
# 21  det4b     F  3174
# 22  det4b     T    19
# 23  det4c     F  3172
# 24  det4c     T    19
# 25  det4d     F  3173
# 26  det4d     T    20
# 27  det5a     F  3171
# 28  det5a     T    21
# 29  det6a     F  3178
# 30  det6a     T    19
# 31  det6b     F  3173
# 32  det6b     T    19
# 33  det6c     F  3172
# 34  det6c     T    19
# 35  det6d     F  3175
# 36  det6d     T    20
# 37  det7a     F  3166
# 38  det7a     T    19
# 39  det8a     F  3169
# 40  det8a     T    21
# 41  det8b     F  3175
# 42  det8b     T    19
# 43  det8c     F  3174
# 44  det8c     T    21
# 45  det8d     F  3170
# 46  det8d     T    18
# 47  det9a     F  3167
# 48  det9a     T    22

save(file="RCP11_Input_Data_AGG.RData", list = c("MeanStdDF","histDF"))



# max(filter(histDF, var=="x3a")$upper/60)
# max(filter(histDF, var=="x3b")$upper/60)
# max(filter(histDF, var=="x3c")$upper/60)
# # > max(filter(histDF, var=="x3a")$upper/60)
# # [1] 20.12972
# # > max(filter(histDF, var=="x3b")$upper/60)
# # [1] 23.99889
# # > max(filter(histDF, var=="x3c")$upper/60)
# # [1] 21.62361
# 
# max(filter(histDF, var=="x3d")$upper/60)
# max(filter(histDF, var=="x3e")$upper/60)
# max(filter(histDF, var=="x3f")$upper/60)
# # > max(filter(histDF, var=="x3d")$upper/60)
# # [1] 14.16611
# # > max(filter(histDF, var=="x3e")$upper/60)
# # [1] 19.34222
# # > max(filter(histDF, var=="x3f")$upper/60)
# # [1] 18.70444
# 
# max(filter(histDF, var=="x3g")$upper/60)
# # > max(filter(histDF, var=="x3g")$upper/60)
# # [1] 20.59806
# 
# max(filter(histDF, var=="x3h")$upper)
# max(filter(histDF, var=="x3i")$upper)
# # > max(filter(histDF, var=="x3h")$upper)
# # [1] 32
# # > max(filter(histDF, var=="x3i")$upper)
# # [1] 23
# 
# max(filter(histDF, var=="x4")$upper)
# # > max(filter(histDF, var=="x4")$upper)
# # [1] 22
# 
# histDF %>% filter(substr(var,2,2)=="5") %>% group_by(var) %>% summarize( maxUpper=max(upper))
# #     var maxUpper
# # 1   x5a    23313
# # 2   x5b     9020
# # 3   x5c     1143
# # 4   x5d      936