all_detectors <- c("001a","002a","002b","002c","002d","003a","004a","004b","004c","004d","005a","006a","006b","006c","006d","007a","008a","008b","008c","008d","009a","010a","011a","011b")
given_months <- c("2015-11", "2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06")
total_num_detectors <- 24
num_given_months <- 8
leave_train_avg_pop_size <- 15
noleave_train_avg_pop_size <- 3151

time_corr_mat <- suppressMessages(read_csv(str_c(getwd(), "/RCP17data/RCP17_time_spearman_correlations/Time_Correlations_Spearman.csv"), skip = 1, col_names = F))
time_corr_mat[,1] <- NULL #drop first column
#loop to transform time correlations into matrix format, detector by detector
list_of_time_corr_mats <- list()
counter <- 0
for (det in all_detectors){
  counter <- counter+1
  list_of_time_corr_mats[[det]] <- vec2symmat(invec = as.numeric(time_corr_mat[counter,]))
}

#loop for reading in the correlation files of leave and no leave group and making positive definite correlation matrices of them  
det_to_det_train_corr_leave_list <- list()
det_to_det_train_corr_noleave_list <- list()
for (month in corr_months){
  corr_vect <- suppressMessages(read_csv(str_c(getwd(), "/RCP17data/RCP17_detector_spearman_correlations/", month, "_Correlations_Leave.csv")))$`Spearman Correlation` %>% as.numeric
  det_to_det_corr <- vec2symmat(invec = corr_vect)
  det_to_det_corr <- round(det_to_det_corr, 10)
  det_to_det_corr[is.na(det_to_det_corr)] <- 0
  #print(isSymmetric(det_to_det_corr))
  #print(is.positive.definite(det_to_det_corr))
  if (is.positive.definite(det_to_det_corr) == FALSE){
    det_to_det_corr <- nearPD(det_to_det_corr) %>% .$mat %>% as.matrix
    diag(det_to_det_corr) <- 1
  }
  print(is.positive.definite(det_to_det_corr))
  det_to_det_train_corr_leave_list[[month]] <- det_to_det_corr
  
  
  corr_vect <- suppressMessages(read_csv(str_c(getwd(), "/RCP17data/RCP17_detector_spearman_correlations/", month, "_Correlations_NoLeave.csv")))$`Spearman Correlation` %>% as.numeric
  det_to_det_corr <- vec2symmat(invec = corr_vect)
  det_to_det_corr <- round(det_to_det_corr, 10)
  det_to_det_corr[is.na(det_to_det_corr)] <- 0
  #print(isSymmetric(det_to_det_corr))
  #print(is.positive.definite(det_to_det_corr))
  if (is.positive.definite(det_to_det_corr) == FALSE){
    det_to_det_corr <- nearPD(det_to_det_corr) %>% .$mat %>% as.matrix
    diag(det_to_det_corr) <- 1
  }
  print(is.positive.definite(det_to_det_corr))
  det_to_det_train_corr_noleave_list[[month]] <- det_to_det_corr
  
} # end of correlations reading loop


dummy_2dcorrelated_data_generator <- function(all_detectors, months, leave_avg_pop_size, noleave_avg_pop_size, list_of_time_corr_mats, det_to_det_corr_leave_list, det_to_det_corr_noleave_list){

# correlate dummy detectors across time using cholesky decomposition of time correlations
list_of_uncorrelated_gaussian_samples_leave <- list()
list_of_uncorrelated_gaussian_samples_noleave <- list()
list_of_time_correlated_gaussian_samples_leave <- list()
list_of_time_correlated_gaussian_samples_noleave <- list()
counter <- 0
Sigma <- diag(length(months))
for(det in all_detectors){
  counter <- counter+1
  list_of_uncorrelated_gaussian_samples_leave[[det]] <- mvrnorm(n=leave_avg_pop_size, mu = rep(0, length(months)), Sigma = Sigma)
  list_of_uncorrelated_gaussian_samples_noleave[[det]] <- mvrnorm(n=noleave_avg_pop_size, mu = rep(0, length(months)), Sigma = Sigma)
  list_of_time_correlated_gaussian_samples_leave[[det]] <- list_of_uncorrelated_gaussian_samples_leave[[det]] %*% chol(list_of_time_corr_mats[[det]])
  list_of_time_correlated_gaussian_samples_noleave[[det]] <- list_of_uncorrelated_gaussian_samples_noleave[[det]] %*% chol(list_of_time_corr_mats[[det]])
}

# correlate data across dummy detectors using cholesky decomposition of detector correlations
list_of_time_and_det_correlated_gaussian_samples_leave <- list()
list_of_time_and_det_correlated_gaussian_samples_noleave <- list()
counter <- 0
for (month in months){
  counter <- counter+1
  time_correlated_gaussian_samples_by_month_leave <- lapply(list_of_time_correlated_gaussian_samples_leave, function(df,month){df[,month]}, month = counter) %>% bind_cols # tranforming back to original format that is, each matrix is monthly data for all detectors
  time_correlated_gaussian_samples_by_month_noleave <- lapply(list_of_time_correlated_gaussian_samples_noleave, function(df,month){df[,month]}, month = counter) %>% bind_cols # tranforming back to original format that is, each matrix is monthly data for all detectors
  list_of_time_and_det_correlated_gaussian_samples_leave[[month]] <- as.matrix(time_correlated_gaussian_samples_by_month_leave) %*% as.matrix(chol(det_to_det_corr_leave_list[[month]]))
  list_of_time_and_det_correlated_gaussian_samples_noleave[[month]] <- as.matrix(time_correlated_gaussian_samples_by_month_noleave) %*% as.matrix(chol(det_to_det_corr_noleave_list[[month]]))
}
  return(list(list_of_time_and_det_correlated_gaussian_samples_leave, list_of_time_and_det_correlated_gaussian_samples_noleave))
}#end of function

#this part requires actual iid detector samples for each month and copies the rank correlation from dummy data to actual data
correlated_train_leave_list <- list()
correlated_train_noleave_list <- list()
correlated_train_list <- list()
for (month in given_months){
  
  correlated_train_leave_list[[month]] <- CorrelateData(DetectorData = as.data.frame(list_of_all_detector_samples_leave_train[[month]]), CopulaMatrix = as.data.frame(list_of_time_and_det_correlated_gaussian_samples_leave[[month]]))
  #correlated_train_leave_list[[month]]["month"] <- rep(month, nrow(correlated_train_leave_list[[month]])) #adding month column
  #correlated_train_leave_list[[month]]["label"] <- rep(1, nrow(correlated_train_leave_list[[month]]))
  correlated_train_noleave_list[[month]] <- CorrelateData(DetectorData = as.data.frame(list_of_all_detector_samples_noleave_train[[month]]), CopulaMatrix = as.data.frame(list_of_time_and_det_correlated_gaussian_samples_noleave[[month]]))
  #correlated_train_noleave_list[[month]]["month"] <- rep(month, nrow(correlated_train_noleave_list[[month]])) #adding month column
  #correlated_train_noleave_list[[month]]["label"] <- rep(0, nrow(correlated_train_noleave_list[[month]]))
  correlated_train_list[[month]] <- bind_rows(correlated_train_noleave_list[[month]], correlated_train_leave_list[[month]])
  correlated_train_list[[month]]["uid"] <- 1:nrow(correlated_train_list[[month]])
}







# code to test the det-det and time correlations

calc_noLeave_det_corr <- (cor(correlated_train_noleave_list[["2015-11"]], method = "spearman")) #compare detector to detector correlations
actual_noLeave_det_corr <- (det_to_det_train_corr_noleave_list[["2015-11"]])
diff_noLeave_det_corr <- abs(actual_noLeave_det_corr - calc_noLeave_det_corr)
write_csv(as.data.frame(calc_noLeave_det_corr), "calc_noLeave_det_corr.csv")
write_csv(as.data.frame(actual_noLeave_det_corr), "actual_noLeave_det_corr.csv")
write_csv(as.data.frame(diff_noLeave_det_corr), "diff_noLeave_det_corr.csv")

calc_Leave_det_corr <- (cor(correlated_train_leave_list[["2015-11"]], method = "spearman"))
actual_Leave_det_corr <- (det_to_det_train_corr_leave_list[["2015-11"]])
diff_Leave_det_corr <- abs(actual_Leave_det_corr - calc_Leave_det_corr)
write_csv(as.data.frame(calc_Leave_det_corr), "calc_Leave_det_corr.csv")
write_csv(as.data.frame(actual_Leave_det_corr), "actual_Leave_det_corr.csv")
write_csv(as.data.frame(diff_Leave_det_corr), "diff_Leave_det_corr.csv")

upper_calc_noLeave_det_corr <- calc_noLeave_det_corr[upper.tri(calc_noLeave_det_corr)]
upper_actual_noLeave_det_corr <- actual_noLeave_det_corr[upper.tri(actual_noLeave_det_corr)]
plot(upper_calc_noLeave_det_corr, upper_actual_noLeave_det_corr)

upper_calc_Leave_det_corr <- calc_Leave_det_corr[upper.tri(calc_Leave_det_corr)]
upper_actual_Leave_det_corr <- actual_Leave_det_corr[upper.tri(actual_Leave_det_corr)]
plot(upper_calc_Leave_det_corr, upper_actual_Leave_det_corr)

cortest(cor(correlated_train_noleave_list[["2015-11"]], method = "spearman"),as.matrix(det_to_det_train_corr_noleave_list[["2015-11"]]),n1=3515, n2=3515)
cortest(cor(correlated_train_leave_list[["2015-11"]], method = "spearman"),as.matrix(det_to_det_train_corr_leave_list[["2015-11"]]),n1=15, n2=15)

# transform data to test time to time correlations
leave_train_detector_monthly_dists_list <- list()
dist_list <- list()
counter <- 0
for (det in all_detectors){
  counter <- counter+1
  for (month in given_months){
    dist_list[[month]] <- correlated_train_leave_list[[month]][,counter]
  }
  leave_train_detector_monthly_dists_list[[det]] <- bind_cols(dist_list)
}

noleave_train_detector_monthly_dists_list <- list()
dist_list <- list()
counter <- 0
for (det in all_detectors){
  counter <- counter+1
  for (month in given_months){
    dist_list[[month]] <- correlated_train_noleave_list[[month]][,counter]
  }
  noleave_train_detector_monthly_dists_list[[det]] <- bind_cols(dist_list)
}


calc_time_corr_leave <- (cor(leave_train_detector_monthly_dists_list[["001a"]], method = "spearman")) 
calc_time_corr_noleave <- (cor(noleave_train_detector_monthly_dists_list[["001a"]], method = "spearman")) 
actual_time_corr <- (list_of_time_corr_mats[["001a"]])
diff_noLeave_time_corr <- abs(actual_time_corr - calc_time_corr_noleave)
diff_Leave_time_corr <- abs(actual_time_corr - calc_time_corr_leave)
write_csv(as.data.frame(calc_time_corr_leave), "calc_time_corr_leave.csv")
write_csv(as.data.frame(calc_time_corr_noleave), "calc_time_corr_noleave.csv")
write_csv(as.data.frame(actual_time_corr), "actual_time_corr.csv")
write_csv(as.data.frame(diff_noLeave_time_corr), "diff_noLeave_time_corr.csv")
write_csv(as.data.frame(diff_Leave_time_corr), "diff_Leave_time_corr.csv")

upper_calc_time_corr_noleave <- calc_time_corr_noleave[upper.tri(calc_time_corr_noleave)]
upper_actual_time_corr <- actual_time_corr[upper.tri(actual_time_corr)]
plot(upper_calc_time_corr_noleave, upper_actual_time_corr)

upper_calc_time_corr_leave <- calc_time_corr_leave[upper.tri(calc_time_corr_leave)]
upper_actual_time_corr <- actual_time_corr[upper.tri(actual_time_corr)]
plot(upper_calc_time_corr_leave, upper_actual_time_corr)

cortest(cor(noleave_train_detector_monthly_dists_list[["001a"]], method = "spearman"),as.matrix(list_of_time_corr_mats[["001a"]]),n1=3515, n2=3515)
cortest(cor(leave_train_detector_monthly_dists_list[["001a"]], method = "spearman"),as.matrix(list_of_time_corr_mats[["001a"]]),n1=15, n2=15)



### code to check test period data

calc_noLeave_det_corr <- (cor(correlated_test_noleave_list[["2016-07"]], method = "spearman")) #compare detector to detector correlations
actual_noLeave_det_corr <- (det_to_det_test_corr_noleave_list[["2016-04"]])
diff_noLeave_det_corr <- abs(actual_noLeave_det_corr - calc_noLeave_det_corr)
write_csv(as.data.frame(calc_noLeave_det_corr), "calc_noLeave_det_corr.csv")
write_csv(as.data.frame(actual_noLeave_det_corr), "actual_noLeave_det_corr.csv")
write_csv(as.data.frame(diff_noLeave_det_corr), "diff_noLeave_det_corr.csv")

calc_Leave_det_corr <- (cor(correlated_test_leave_list[["2016-07"]], method = "spearman"))
actual_Leave_det_corr <- (det_to_det_test_corr_leave_list[["2016-04"]])
diff_Leave_det_corr <- abs(actual_Leave_det_corr - calc_Leave_det_corr)
write_csv(as.data.frame(calc_Leave_det_corr), "calc_Leave_det_corr.csv")
write_csv(as.data.frame(actual_Leave_det_corr), "actual_Leave_det_corr.csv")
write_csv(as.data.frame(diff_Leave_det_corr), "diff_Leave_det_corr.csv")

upper_calc_noLeave_det_corr <- calc_noLeave_det_corr[upper.tri(calc_noLeave_det_corr)]
upper_actual_noLeave_det_corr <- actual_noLeave_det_corr[upper.tri(actual_noLeave_det_corr)]
plot(upper_calc_noLeave_det_corr, upper_actual_noLeave_det_corr)

upper_calc_Leave_det_corr <- calc_Leave_det_corr[upper.tri(calc_Leave_det_corr)]
upper_actual_Leave_det_corr <- actual_Leave_det_corr[upper.tri(actual_Leave_det_corr)]
plot(upper_calc_Leave_det_corr, upper_actual_Leave_det_corr)

cortest(cor(correlated_test_noleave_list[["2016-07"]], method = "spearman"),as.matrix(det_to_det_test_corr_noleave_list[["2015-11"]]),n1=3515, n2=3515)
cortest(cor(correlated_test_leave_list[["2015-11"]], method = "spearman"),as.matrix(det_to_det_test_corr_leave_list[["2015-11"]]),n1=15, n2=15)

# transform data to test time to time correlations
leave_test_detector_monthly_dists_list <- list()
dist_list <- list()
counter <- 0
for (det in all_detectors){
  counter <- counter+1
  for (month in test_months){
    dist_list[[month]] <- correlated_test_leave_list[[month]][,counter]
  }
  leave_test_detector_monthly_dists_list[[det]] <- bind_cols(dist_list)
}

noleave_test_detector_monthly_dists_list <- list()
dist_list <- list()
counter <- 0
for (det in all_detectors){
  counter <- counter+1
  for (month in test_months){
    dist_list[[month]] <- correlated_test_noleave_list[[month]][,counter]
  }
  noleave_test_detector_monthly_dists_list[[det]] <- bind_cols(dist_list)
}


calc_time_corr_leave <- (cor(leave_test_detector_monthly_dists_list[["001a"]], method = "spearman")) 
calc_time_corr_noleave <- (cor(noleave_test_detector_monthly_dists_list[["001a"]], method = "spearman")) 
actual_time_corr <- (list_of_time_corr_mats[["001a"]])
diff_noLeave_time_corr <- abs(actual_time_corr - calc_time_corr_noleave)
diff_Leave_time_corr <- abs(actual_time_corr - calc_time_corr_leave)
write_csv(as.data.frame(calc_time_corr_leave), "calc_time_corr_leave.csv")
write_csv(as.data.frame(calc_time_corr_noleave), "calc_time_corr_noleave.csv")
write_csv(as.data.frame(actual_time_corr), "actual_time_corr.csv")
write_csv(as.data.frame(diff_noLeave_time_corr), "diff_noLeave_time_corr.csv")
write_csv(as.data.frame(diff_Leave_time_corr), "diff_Leave_time_corr.csv")

upper_calc_time_corr_noleave <- calc_time_corr_noleave[upper.tri(calc_time_corr_noleave)]
upper_actual_time_corr <- actual_time_corr[upper.tri(actual_time_corr)]
plot(upper_calc_time_corr_noleave, upper_actual_time_corr)

upper_calc_time_corr_leave <- calc_time_corr_leave[upper.tri(calc_time_corr_leave)]
upper_actual_time_corr <- actual_time_corr[upper.tri(actual_time_corr)]
plot(upper_calc_time_corr_leave, upper_actual_time_corr)

cortest(cor(noleave_test_detector_monthly_dists_list[["001a"]], method = "spearman"),as.matrix(list_of_time_corr_mats[["001a"]]),n1=3515, n2=3515)
cortest(cor(leave_test_detector_monthly_dists_list[["001a"]], method = "spearman"),as.matrix(list_of_time_corr_mats[["001a"]]),n1=15, n2=15)


