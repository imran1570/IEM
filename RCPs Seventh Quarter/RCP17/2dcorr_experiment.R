myhist <-list(breaks=Week9_det001a_Other$lower, counts=Week9_det001a_Other$counts, xname="Detector")
class(myhist) <- "histogram"
plot(myhist)
lines((myhist$density))


ggplot(myhist, aes(breaks)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df$x), sd = sd(df$x)), 
                lwd = 2, 
                col = 'red')


tmp <- runif(sum(Week5_det001a_NoChange$Count), min = rep(Week5_det001a_NoChange$`Lower bound`, Week5_det001a_NoChange$Count), max = rep(Week5_det001a_NoChange$`Upper bound`, Week5_det001a_NoChange$Count))

hist(tmp, probability = T)
lines(density(tmp))


detector_values <- runif(sum(Week9_det001a_Other$counts), min = rep(Week9_det001a_Other$lower, Week9_det001a_Other$counts), max = rep(Week9_det001a_Other$upper, Week9_det001a_Other$counts))
df <- data.frame(detector_values)
bin_width <- diff(Week9_det001a_Other$lower)[1]
ggplot(df, aes(x=detector_values)) +
  geom_histogram(aes(y=..count..,), fill="blue", alpha = .7)+
  geom_density(colour="brown", size=0.8)+
  labs(title="Detector: Number of Unblocked Webproxy Connections")

#detector_values <- detector_values[detector_values>150000]
df <- data.frame(detector_values)
ggplot(df, aes(x=detector_values)) +
  geom_histogram(aes(x=detector_values, y=..density..))+
  geom_density(size=0.8)+
  labs(title="Detector: Number of Unblocked Webproxy Connections")

p <- ggplot(df, aes(x=detector_values)) +
  geom_histogram(aes(x=detector_values, y=..density..))+
  coord_cartesian(xlim= c(15,30))

# If you want it on the count scale, that is trickier and requires
# knowing (setting) the binwidth and keeping that value in sync in
# two places.
# In this example, the binwidth is 0.2 (set in geom_histogram and also
# used in the aes of geom_density).

ggplot(df, aes(x=tmp)) +
  geom_histogram(fill="blue", colour="black", binwidth=49747) +
  geom_density(aes(y=49747*..count..), colour="Green") +
  opts(title="Detector Histogram")

dati <- data.frame(Week9_det001a_Other)
str(dati)
dati$class <- dati$upper - dati$lower 
str(dati)
dati$lower=NULL
dati$upper=NULL
bins <- diff(Week9_det001a_Other$lower)


vals1 <- runif(sum(Week9_det001a_Other$counts), min = rep(Week9_det001a_Other$lower, Week9_det001a_Other$counts), max = rep(Week9_det001a_Other$upper, Week9_det001a_Other$counts))

gg <- ggplot(data.frame(x=c(vals1)))
gg <- gg + geom_histogram(aes(x=vals1, y=..count..))
gg <- gg + geom_density(aes(x=x, y=..scaled..))
gg <- gg + theme_bw()
gg


ggplot(subset(df, detector_values != arrange(count(df, detector_values), desc(n))[1,]$detector_values), aes(detector_values)) + geom_histogram(aes(y=..count..))
  geom_density(aes(y=..scaled..)) + facet_grid()


gg <- ggplot(data.frame(x=c(detector_values)), aes(x=detector_values))
gg <- gg + geom_histogram(aes(y=..count..))
ggplot_build(gg)$data[[1]] <- ggplot_build(gg)$data[[1]][-c(1),]


ggplot(df, aes(x=detector_values, width = c(diff(Week9_det001a_Other$upper)))) +  geom_histogram( aes(x=detector_values,y = ..count..),
                                position = "identity")


ggplot(Week9_det001a_Other) +  geom_histogram( aes(x= , y = ..density.., weight = Week9_det001a_Other$counts))


avg_det1 <- avg_time_detector_correlated_noleave_train_samples[,1]
nov_det1 <- time_correlated_noleave_train_list_original[["2015-11"]]$`001a`
View(cor(cbind(avg_det1, nov_det1), method = "spearman"))

avg_det2 <- avg_time_detector_correlated_noleave_train_samples[,2]
nov_det2 <- time_correlated_noleave_train_list_original[["2015-11"]]$`002a`
View(cor(cbind(avg_det2, nov_det2), method = "spearman"))

getwd()
tmp <- (cor(time_correlated_noleave_train_list[["001a"]], method = "spearman"))
write_csv(as.data.frame(tmp), "initial_time_correlation.csv")

avg_time_detector_correlated_noleave_train_samples
tmp <- (cor(avg_time_detector_correlated_leave_train_samples, method = "spearman"))
write_csv(as.data.frame(tmp), "det-det_correlation_averaged_samples.csv")

View(avg_det_to_det_noleave_train_corr)
write_csv(as.data.frame(avg_det_to_det_leave_train_corr), "det-det_averaged_correlation_leave.csv")

correlated_train_leave_list
tmp <- (cor(correlated_train_noleave_list[["2015-12"]], method = "spearman"))
write_csv(as.data.frame(tmp), "post_det-det_correlation_dec_noleave.csv")
View(tmp)

avg_time_detector_correlated_leave_train_samples
tmp1 <- (cor(avg_time_detector_correlated_leave_train_samples, method = "spearman"))
View(tmp1)
#View(cor(avg_time_detector_correlated_noleave_train_samples, method = "spearman"))
tmp3 <- (cor(cbind(time_correlated_noleave_train_list_original[["2015-11"]]$`001a`, time_correlated_noleave_train_list_original[["2015-12"]]$`001a`), method = "spearman"))
View(tmp3)


calculated_corr_list <- lapply(correlated_train_noleave_list, function(x){cor(x, method = "spearman")})
avg_calculated_corr <- Reduce("+", calculated_corr_list) / length(calculated_corr_list)
View(avg_calculated_corr)
write_csv(as.data.frame(avg_calculated_corr), "avg_calculated_corr.csv")

tmp_avg <- Reduce("+", correlated_train_noleave_list) / length(correlated_train_noleave_list)
View(tmp_avg)



################## experimentation of cholesky decomp with gaussian samples methdology###################


numWeeks <- 3
numDets <- 3
numUsers <- 3127

det1a_weekly_corr_vect <- c(0.807165741208,0.783706855398,0.638119143739)
det1a_weekly_corr_mat <- vec2symmat(invec = det1a_weekly_corr_vect)
isSymmetric(det1a_weekly_corr_mat)  
is.positive.definite(det1a_weekly_corr_mat)
det1a_weekly_corr_chol_decomp <- chol(det1a_weekly_corr_mat)

det1b_weekly_corr_vect <- c(0.740258954703,0.729887137871,0.619563721676)
det1b_weekly_corr_mat <- vec2symmat(invec = det1b_weekly_corr_vect)
isSymmetric(det1b_weekly_corr_mat)  
is.positive.definite(det1b_weekly_corr_mat)
det1b_weekly_corr_chol_decomp <- chol(det1b_weekly_corr_mat)

det1c_weekly_corr_vect <- c(0.742688369895,0.647294961645,0.58321466648)
det1c_weekly_corr_mat <- vec2symmat(invec = det1c_weekly_corr_vect)
isSymmetric(det1c_weekly_corr_mat)  
is.positive.definite(det1c_weekly_corr_mat)
det1c_weekly_corr_chol_decomp <- chol(det1c_weekly_corr_mat)

det2a_weekly_corr_vect <- c(0.784423499336,0.75241725759,0.624798324534)
det2a_weekly_corr_mat <- vec2symmat(invec = det2a_weekly_corr_vect)
isSymmetric(det2a_weekly_corr_mat)  
is.positive.definite(det2a_weekly_corr_mat)
det2a_weekly_corr_chol_decomp <- chol(det2a_weekly_corr_mat)


library(MASS)

# Sigma <- diag(3)
# tmp=mvrnorm(n=1000,c(0,0),Sigma) # sample 5x2 with mean [0,0]
# colMeans(tmp)
# apply(tmp, 2, sd)

Sigma <- diag(numWeeks)
det1a_weekly_uncorrelated_samples <- mvrnorm(n=numUsers, rep(0, numWeeks), Sigma)
colnames(det1a_weekly_uncorrelated_samples) <- c("week09", "week10", "week11")

#standard_gauss_samples <- matrix(rnorm(nrow(det1a_weekly_uncorrelated_samples)*ncol(det1a_weekly_uncorrelated_samples),mean=0,sd=1), nrow(det1a_weekly_uncorrelated_samples), ncol(det1a_weekly_uncorrelated_samples)) 

det1a_weekly_correlated_samples <- det1a_weekly_uncorrelated_samples %*% det1a_weekly_corr_chol_decomp
colnames(det1a_weekly_correlated_samples) <- c("week09", "week10", "week11")

cor(det1a_weekly_uncorrelated_samples, method = "spearman")
cor(det1a_weekly_correlated_samples, method = "spearman")
det1a_weekly_corr_mat

det1b_weekly_uncorrelated_samples <- mvrnorm(n=numUsers, rep(0, numWeeks), Sigma)
colnames(det1b_weekly_uncorrelated_samples) <- c("week09", "week10", "week11")

det1b_weekly_correlated_samples <- det1b_weekly_uncorrelated_samples %*% det1b_weekly_corr_chol_decomp
colnames(det1b_weekly_correlated_samples) <- c("week09", "week10", "week11")

cor(det1b_weekly_uncorrelated_samples, method = "spearman")
cor(det1b_weekly_correlated_samples, method = "spearman")
det1b_weekly_corr_mat

det1c_weekly_uncorrelated_samples <- mvrnorm(n=numUsers, rep(0, numWeeks), Sigma)
colnames(det1c_weekly_uncorrelated_samples) <- c("week09", "week10", "week11")

det1c_weekly_correlated_samples <- det1c_weekly_uncorrelated_samples %*% det1c_weekly_corr_chol_decomp
colnames(det1c_weekly_correlated_samples) <- c("week09", "week10", "week11")

cor(det1c_weekly_uncorrelated_samples, method = "spearman")
cor(det1c_weekly_correlated_samples, method = "spearman")
det1c_weekly_corr_mat

num_detectors <- 142

week09_det_to_det_corr <- matrix(1, nrow = num_detectors, ncol = num_detectors)
counter <- 0
for (row in 1:nrow(week09_det_to_det_corr)){
  
  for (col in 1:ncol(week09_det_to_det_corr)) {
    if (row == col){
      week09_det_to_det_corr[row, col] <- 1
    } else {
      counter <- counter+1
      week09_det_to_det_corr[row, col] <- Week09_Correlations_LineManagement$`Pearson Correlation`[counter]
    }
  }
}
isSymmetric(week09_det_to_det_corr)  
is.positive.definite(week09_det_to_det_corr)

week10_det_to_det_corr <- matrix(1, nrow = num_detectors, ncol = num_detectors)
counter <- 0
for (row in 1:nrow(week10_det_to_det_corr)){
  
  for (col in 1:ncol(week10_det_to_det_corr)) {
    if (row == col){
      week10_det_to_det_corr[row, col] <- 1
    } else {
      counter <- counter+1
      week10_det_to_det_corr[row, col] <- Week10_Correlations_LineManagement$`Pearson Correlation`[counter]
    }
  }
}
isSymmetric(week10_det_to_det_corr)  
is.positive.definite(week10_det_to_det_corr)


week11_det_to_det_corr <- matrix(1, nrow = num_detectors, ncol = num_detectors)
counter <- 0
for (row in 1:nrow(week11_det_to_det_corr)){
  
  for (col in 1:ncol(week11_det_to_det_corr)) {
    if (row == col){
      week11_det_to_det_corr[row, col] <- 1
    } else {
      counter <- counter+1
      week11_det_to_det_corr[row, col] <- Week11_Correlations_LineManagement$`Pearson Correlation`[counter]
    }
  }
}
isSymmetric(week11_det_to_det_corr)  
is.positive.definite(week11_det_to_det_corr)

week09_detectorly_corr <- week09_det_to_det_corr[1:3, 1:3]
week09_detectorly_corr_chol_decomp <- chol(week09_detectorly_corr)
week10_detectorly_corr <- week10_det_to_det_corr[1:3, 1:3]
week10_detectorly_corr_chol_decomp <- chol(week10_detectorly_corr)
week11_detectorly_corr <- week11_det_to_det_corr[1:3, 1:3]
week11_detectorly_corr_chol_decomp <- chol(week11_detectorly_corr)


week09_detectorly_uncorrelated_samples <- cbind(det1a_weekly_correlated_samples[,c("week09")], det1b_weekly_correlated_samples[,c("week09")], det1c_weekly_correlated_samples[,c("week09")])
colnames(week09_detectorly_uncorrelated_samples) <- c("det001a", "det001b", "det001c")

week09_detectorly_correlated_samples <- week09_detectorly_uncorrelated_samples %*% week09_detectorly_corr_chol_decomp
colnames(week09_detectorly_correlated_samples) <- c("det001a", "det001b", "det001c")

cor(week09_detectorly_uncorrelated_samples, method = "spearman")
cor(week09_detectorly_correlated_samples, method = "spearman")
week09_detectorly_corr

week09_user_profiles_dummy <- week09_detectorly_correlated_samples


week10_detectorly_uncorrelated_samples <- cbind(det1a_weekly_correlated_samples[,c("week10")], det1b_weekly_correlated_samples[,c("week10")], det1c_weekly_correlated_samples[,c("week10")])
colnames(week10_detectorly_uncorrelated_samples) <- c("det001a", "det001b", "det001c")

week10_detectorly_correlated_samples <- week10_detectorly_uncorrelated_samples %*% week10_detectorly_corr_chol_decomp
colnames(week10_detectorly_correlated_samples) <- c("det001a", "det001b", "det001c")

cor(week10_detectorly_uncorrelated_samples, method = "spearman")
cor(week10_detectorly_correlated_samples, method = "spearman")
week10_detectorly_corr

week10_user_profiles_dummy <- week10_detectorly_correlated_samples

week11_detectorly_uncorrelated_samples <- cbind(det1a_weekly_correlated_samples[,c("week11")], det1b_weekly_correlated_samples[,c("week11")], det1c_weekly_correlated_samples[,c("week11")])

week11_detectorly_correlated_samples <- week11_detectorly_uncorrelated_samples %*% week11_detectorly_corr_chol_decomp


cor(week11_detectorly_uncorrelated_samples, method = "spearman")
cor(week11_detectorly_correlated_samples, method = "spearman")
week11_detectorly_corr

week11_user_profiles_dummy <- week11_detectorly_correlated_samples



cor(cbind(week09_user_profiles_dummy[,c("det001a")], week10_user_profiles_dummy[,c("det001a")], week11_user_profiles_dummy[,c("det001a")]), method = "spearman")
det1a_weekly_corr_mat

cor(cbind(week09_user_profiles_dummy[,c("det001b")], week10_user_profiles_dummy[,c("det001b")], week11_user_profiles_dummy[,c("det001b")]), method = "spearman")
det1b_weekly_corr_mat

cor(cbind(week09_user_profiles_dummy[,c("det001c")], week10_user_profiles_dummy[,c("det001c")], week11_user_profiles_dummy[,c("det001c")]), method = "spearman")
det1c_weekly_corr_mat

#Week09_Other <- read_csv("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Seventh Quarter/RCP19/RCP19DataOptSamples/Week9_Other.csv")
Week09_Other <- read_csv("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Seventh Quarter/RCP19/RCP19DataOptSamples/Week10_Other.csv")
Week10_Other <- read_csv("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Seventh Quarter/RCP19/RCP19DataOptSamples/Week11_Other.csv")
Week11_Other <- read_csv("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Seventh Quarter/RCP19/RCP19DataOptSamples/Week12_Other.csv")



week09_uncorrelated_user_profiles <- Week09_Other[,c("det001a", "det001b", "det001c")]
week10_uncorrelated_user_profiles  <- Week10_Other[,c("det001a", "det001b", "det001c")]
week11_uncorrelated_user_profiles  <- Week11_Other[,c("det001a", "det001b", "det001c")]

week09_correlated_user_profiles <- CorrelateData(DetectorData = as.data.frame(week09_uncorrelated_user_profiles), CopulaMatrix = as.data.frame(week09_user_profiles_dummy))
colnames(week09_correlated_user_profiles) <- c("det001a", "det001b", "det001c")
week10_correlated_user_profiles <- CorrelateData(DetectorData = as.data.frame(week10_uncorrelated_user_profiles), CopulaMatrix = as.data.frame(week10_user_profiles_dummy))
colnames(week10_correlated_user_profiles) <- c("det001a", "det001b", "det001c")
week11_correlated_user_profiles <- CorrelateData(DetectorData = as.data.frame(week11_uncorrelated_user_profiles), CopulaMatrix = as.data.frame(week11_user_profiles_dummy))
colnames(week11_correlated_user_profiles) <- c("det001a", "det001b", "det001c")

cor(week09_correlated_user_profiles, method = "spearman")
cor(week09_user_profiles_dummy, method = "spearman")
week09_detectorly_corr

cor(week10_correlated_user_profiles, method = "spearman")
cor(week10_user_profiles_dummy, method = "spearman")
week10_detectorly_corr

cor(week11_correlated_user_profiles, method = "spearman")
cor(week11_user_profiles_dummy, method = "spearman")
week11_detectorly_corr

cor(cbind(week09_correlated_user_profiles[,c("det001a")], week10_correlated_user_profiles[,c("det001a")], week11_correlated_user_profiles[,c("det001a")]), method = "spearman")
det1a_weekly_corr_mat

cor(cbind(week09_correlated_user_profiles[,c("det001b")], week10_correlated_user_profiles[,c("det001b")], week11_correlated_user_profiles[,c("det001b")]), method = "spearman")
det1b_weekly_corr_mat

cor(cbind(week09_correlated_user_profiles[,c("det001c")], week10_correlated_user_profiles[,c("det001c")], week11_correlated_user_profiles[,c("det001c")]), method = "spearman")
det1c_weekly_corr_mat


