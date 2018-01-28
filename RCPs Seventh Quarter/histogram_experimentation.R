library(triangle)


det_hist <- read_csv("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Seventh Quarter/RCP19/Week17_det002c_LineManagement.csv")
View(det_hist)
det_hist$x <- round(det_hist$x)
unbinned_data_vect <- c()
count_zeros <- det_hist$counts[det_hist$lower==0 & det_hist$upper==0]
unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
det_hist_no_zeros <- det_hist[(det_hist$lower!=0 & det_hist$upper!=0) & (det_hist$counts!=0),]
tri_dist_means <- 3*(det_hist_no_zeros$x) - det_hist_no_zeros$lower - det_hist_no_zeros$upper
tri_dist_modes <- (det_hist_no_zeros$lower+det_hist_no_zeros$upper)/2
for (i in 1:nrow(det_hist_no_zeros)){
  print(str_c("lower:",det_hist_no_zeros$lower[i], " upper:", det_hist_no_zeros$upper[i], " mean:", tri_dist_means[i]))
  unbinned_data_vect <- c(unbinned_data_vect, rtriangle(n=det_hist_no_zeros$counts[i], a=det_hist_no_zeros$lower[i], b=det_hist_no_zeros$upper[i], c=det_hist_no_zeros$x[i]))
}

137.4067797/mean(round(unbinned_data_vect))
sd(round(unbinned_data_vect))


unbinned_data_vect <- c(unbinned_data_vect, rep(det_hist_no_zeros$x, det_hist_no_zeros$counts))
mean(unbinned_data_vect)/137.4067797
sd(unbinned_data_vect)/579.1966945





# # simplistic optimisation example
# # I am looking for a given mean and a standard deviation
# # but starting from a plain uniform(0,1) distribution
# # create a function to optimise
# fun <- function(xvec, N=177) {
#   xmin <- xvec[1]
#   xmax <- xvec[2]
#   x <- runif(N, xmin, xmax)
#   xdist <- (mean(x) - 137.4067797)^2 + (sd(x) - 579.1966945)^2
#   xdist
# }
# xr <- optim(c(0,6341), fun)
# 
# # now lets test those results
# X <- runif(177, xr$par[1], xr$par[2])
# mean(X) # approx 0
# sd(X)   # approx 1

mid_points <- (det_hist_no_zeros$lower+det_hist_no_zeros$upper)/2
mid_points <- round(mid_points)
# mean(mid_points)/137.4067797


unbinned_data_vect <- c(unbinned_data_vect, rep(mid_points, det_hist_no_zeros$counts))

mean(unbinned_data_vect)/137.4067797
sd(unbinned_data_vect)/579.1966945


unbinned_data_vect <- c(unbinned_data_vect, rep(det_hist_no_zeros$upper[nrow(det_hist_no_zeros)], det_hist_no_zeros$counts[nrow(det_hist_no_zeros)]))

unbinned_unif_samples <- c()
count_zeros <- det_hist$counts[det_hist$lower==0 & det_hist$upper==0]
unbinned_unif_samples <- c(unbinned_unif_samples, rep(0, count_zeros))
for (i in 1:nrow(det_hist_no_zeros)){
unbinned_unif_samples <- c(unbinned_unif_samples, runif(n=det_hist_no_zeros$counts[i], min = det_hist_no_zeros$lower[i], max = det_hist_no_zeros$upper[i]))
}
unbinned_unif_samples <- round(unbinned_unif_samples)
mean(unbinned_unif_samples)/137.4067797
sd(unbinned_unif_samples)/579.1966945


# unbinned_binom_samples <- c()
# count_zeros <- det_hist$counts[det_hist$lower==0 & det_hist$upper==0]
# unbinned_binom_samples <- c(unbinned_binom_samples, rep(0, count_zeros))
# for (i in 1:nrow(det_hist_no_zeros)){
#   unbinned_binom_samples <- c(unbinned_binom_samples, rbinom(n=det_hist_no_zeros$counts[i], size = , prob = det_hist_no_zeros$p[i]))
# }



###### experiment with another detector#################

Week9_det009a_LineManagement <- read_csv("C:/Users/Mimran/Desktop/Week9_det009a_LineManagement-new.csv")

unbinned_data_vect <- c()
count_zeros <- Week9_det009a_LineManagement$counts[Week9_det009a_LineManagement$lower==0 & Week9_det009a_LineManagement$upper==0]
unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
det_hist_no_zeros <- Week9_det009a_LineManagement[(Week9_det009a_LineManagement$lower!=0 & Week9_det009a_LineManagement$upper!=0) & (Week9_det009a_LineManagement$counts!=0),]
unbinned_data_vect <- c(unbinned_data_vect, rep(det_hist_no_zeros$x, det_hist_no_zeros$counts))

given_mean <- 79085391.0847457
given_std <- 172990182.377219

mean(unbinned_data_vect)/given_mean
sd(unbinned_data_vect)/given_std

mids <- round((Week9_det009a_LineManagement$upper + Week9_det009a_LineManagement$lower)/2)
unbinned_data_vect <- c()
count_zeros <- Week9_det009a_LineManagement$counts[Week9_det009a_LineManagement$lower==0 & Week9_det009a_LineManagement$upper==0]
unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
det_hist_no_zeros <- Week9_det009a_LineManagement[(Week9_det009a_LineManagement$lower!=0 & Week9_det009a_LineManagement$upper!=0) & (Week9_det009a_LineManagement$counts!=0),]
unbinned_data_vect <- c(unbinned_data_vect, rep(mids[-1], det_hist_no_zeros$counts))


# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

rmse(mean(unbinned_data_vect)-given_mean)
mae(mean(unbinned_data_vect)-given_mean)

Week17_det005a_LineManagement <- read_csv("C:/Users/Mimran/Desktop/Week17_det005a_LineManagement.csv")
unbinned_data_vect <- c()
count_zeros <- Week17_det005a_LineManagement$counts[Week17_det005a_LineManagement$lower==0 & Week17_det005a_LineManagement$upper==0]
unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
det_hist_no_zeros <- Week17_det005a_LineManagement[(Week17_det005a_LineManagement$lower!=0 & Week17_det005a_LineManagement$upper!=0) & (Week17_det005a_LineManagement$counts!=0),]
unbinned_data_vect <- c(unbinned_data_vect, rep(det_hist_no_zeros$x, det_hist_no_zeros$counts))

given_mean <- 62.82719945
given_std <- 153.5462967

mean(unbinned_data_vect)/given_mean
sd(unbinned_data_vect)/given_std


mids <- (Week17_det005a_LineManagement$upper + Week17_det005a_LineManagement$lower)/2
unbinned_data_vect <- c()
count_zeros <- Week17_det005a_LineManagement$counts[Week17_det005a_LineManagement$lower==0 & Week17_det005a_LineManagement$upper==0]
unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
det_hist_no_zeros <- Week17_det005a_LineManagement[(Week17_det005a_LineManagement$lower!=0 & Week17_det005a_LineManagement$upper!=0) & (Week17_det005a_LineManagement$counts!=0),]
unbinned_data_vect <- c(unbinned_data_vect, rep(mids[-1], det_hist_no_zeros$counts))


Week9_det009a_LineManagement_mean_sd <- read_csv("C:/Users/Mimran/Desktop/Week9_det009a_LineManagement-mean&sd.csv")
unbinned_data_vect <- c()
count_zeros <- Week9_det009a_LineManagement_mean_sd$counts[Week9_det009a_LineManagement_mean_sd$lower==0 & Week9_det009a_LineManagement_mean_sd$upper==0]
unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
det_hist_no_zeros <- Week9_det009a_LineManagement_mean_sd[(Week9_det009a_LineManagement_mean_sd$lower!=0 & Week9_det009a_LineManagement_mean_sd$upper!=0) & (Week9_det009a_LineManagement_mean_sd$counts!=0),]
unbinned_data_vect <- c(unbinned_data_vect, rep(det_hist_no_zeros$x, det_hist_no_zeros$counts))



####### non parametric estimation of the distribution ############

#### kernel density estimation approach heavily depends upon the uniform samples drawn from the bins
Week17_det005a_LineManagement <- read_csv("C:/Users/Mimran/Desktop/Week17_det005a_LineManagement.csv")
unbinned_unif_samples <- c()
det_hist_no_zeros <- Week17_det005a_LineManagement[(Week17_det005a_LineManagement$counts!=0),]
for (i in 1:nrow(det_hist_no_zeros)){
  unbinned_unif_samples <- c(unbinned_unif_samples, runif(n=det_hist_no_zeros$counts[i], min = det_hist_no_zeros$lower[i], max = det_hist_no_zeros$upper[i]))
}
given_mean <- 62.82719945
given_std <- 153.5462967
mean(unbinned_unif_samples)/given_mean
sd(unbinned_unif_samples)/given_std

kernel_fit <- density(unbinned_unif_samples)
N <- kernel_fit$n*2
x.new <- rnorm(N, sample(unbinned_unif_samples, size = 177, replace = F), kernel_fit$bw)
x.new = x.new[x.new>=0]
mean(x.new[1:177])/given_mean
sd(x.new[1:177])/given_std

##### following is somewhat stable but multiple iterations are required to come closer the midpoints to meet the mean and std #########
Week17_det005a_LineManagement <- read_csv("C:/Users/Mimran/Desktop/Week17_det005a_LineManagement.csv")
unbinned_triang_samples <- c()
count_zeros <- Week17_det005a_LineManagement$counts[Week17_det005a_LineManagement$lower==0 & Week17_det005a_LineManagement$upper==0]
unbinned_triang_samples <- c(unbinned_triang_samples, rep(0, count_zeros))
det_hist_no_zeros <- Week17_det005a_LineManagement[(Week17_det005a_LineManagement$lower!=0 & Week17_det005a_LineManagement$upper!=0) & (Week17_det005a_LineManagement$counts!=0),]
for (i in 1:nrow(det_hist_no_zeros)){
  unbinned_triang_samples <- c(unbinned_triang_samples, rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
}
mean(unbinned_triang_samples)/given_mean
sd(unbinned_triang_samples)/given_std

###### following is unstable because it produces a lot of variation ###########
unbinned_gauss_samples <- c()
count_zeros <- Week17_det005a_LineManagement$counts[Week17_det005a_LineManagement$lower==0 & Week17_det005a_LineManagement$upper==0]
unbinned_gauss_samples <- c(unbinned_gauss_samples, rep(0, count_zeros))
det_hist_no_zeros <- Week17_det005a_LineManagement[(Week17_det005a_LineManagement$lower!=0 & Week17_det005a_LineManagement$upper!=0) & (Week17_det005a_LineManagement$counts!=0),]
for (i in 1:nrow(det_hist_no_zeros)){
  unbinned_gauss_samples <- c(unbinned_gauss_samples, rnorm(n=det_hist_no_zeros$counts[i], mean = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2, sd = det_hist_no_zeros$upper[i]-det_hist_no_zeros$lower[i]))
}
mean(unbinned_gauss_samples)/given_mean
sd(unbinned_gauss_samples)/given_std


Week9_det009a_LineManagement <- read_csv("C:/Users/Mimran/Desktop/Week9_det009a_LineManagement.csv")
unbinned_data_vect <- c()
count_zeros <- Week9_det009a_LineManagement$counts[Week9_det009a_LineManagement$lower==0 & Week9_det009a_LineManagement$upper==0]
unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
det_hist_no_zeros <- Week9_det009a_LineManagement[(Week9_det009a_LineManagement$lower!=0 & Week9_det009a_LineManagement$upper!=0) & (Week9_det009a_LineManagement$counts!=0),]
for (i in 1:nrow(det_hist_no_zeros)){
unbinned_data_vect <- c(unbinned_data_vect, rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = det_hist_no_zeros$x[i]))
}
unbinned_data_vect <- round(unbinned_data_vect)
given_mean <- 79085391.0847457
given_std <- 172990182.377219

mean(unbinned_data_vect)/given_mean
sd(unbinned_data_vect)/given_std


