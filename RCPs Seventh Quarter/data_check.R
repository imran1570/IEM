library(readr)
library(stringr)
library(triangle)
options(scipen = 100)

RCP_num <- 18

setwd(str_c("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Seventh Quarter/RCP",RCP_num,"/RCP",RCP_num,"data"))

corr_files <- list.files(pattern = "Correlations")

hist_files <- list.files(pattern = "[_]det\\d{3}[a-z][_]")

hist_files <- c(hist_files, list.files(pattern = "^det\\d{3}[a-z][_]"))

hist_list <- list()
num_user_df <- data.frame(matrix(NA, ncol = 2))
counter <- 0

for (file in hist_files){
  
  counter <- counter+1
  
  hist_list[[file]] <- suppressMessages(read_csv(file))
  
  num_user_df[counter,1] <- file
  num_user_df[counter,2] <- sum(hist_list[[file]]["counts"]) #check counts of observations in each time unit (month/week)
  
}

calculated_means_df <- data.frame(matrix(NA, ncol = 2))
lower_calculated_means_df <- data.frame(matrix(NA, ncol = 2))
upper_calculated_means_df <- data.frame(matrix(NA, ncol = 2))
counter <- 0
for (file in hist_files){
  counter <- counter+1
  mid_point_vect <- (hist_list[[file]][["lower"]]+hist_list[[file]][["upper"]])/2
  prob_dense_vect <- hist_list[[file]][["counts"]] * mid_point_vect
  calculated_means_df[counter,1] <- file
  calculated_means_df[counter,2] <- sum(prob_dense_vect)/sum(hist_list[[file]][["counts"]])
  
  prob_dense_vect <- hist_list[[file]][["counts"]] * hist_list[[file]][["lower"]]
  lower_calculated_means_df[counter,1] <- file
  lower_calculated_means_df[counter,2] <- sum(prob_dense_vect)/sum(hist_list[[file]][["counts"]])
  
  prob_dense_vect <- hist_list[[file]][["counts"]] * hist_list[[file]][["upper"]]
  upper_calculated_means_df[counter,1] <- file
  upper_calculated_means_df[counter,2] <- sum(prob_dense_vect)/sum(hist_list[[file]][["counts"]])
  
}

mean_std_files <- list.files(pattern = "MeanStd")
mean_std_list <- list()
for (file in mean_std_files){
  
  mean_std_list[[file]] <- suppressMessages(read_csv(file)) #remove first row which is not used in analysis
  
}

# head(mean_std_files)
# head(hist_files)

################## for RCP 19 ##################################
period_start <- 9
period_end <- 51

a_class <- "LineManagement"
b_class <- "Other"

lm_lower_calculated_means_df <- (lower_calculated_means_df[grepl(b_class, lower_calculated_means_df$X1),])
lm_upper_calculated_means_df <- (upper_calculated_means_df[grepl(b_class, upper_calculated_means_df$X1),])

mean_check_df <- data.frame(matrix(NA, ncol = 2))
counter <- 0
for (i in period_start:period_end){
  counter <- counter+1
  mean_check_df[counter,1] <- i
  m <- mean_std_list[[str_c("Week",i,"_MeanStd_",b_class,".csv")]][["Mean"]]
  lower <- lm_lower_calculated_means_df[grepl(str_c('Week',i), lm_lower_calculated_means_df$X1),]
  upper <- lm_upper_calculated_means_df[grepl(str_c('Week',i), lm_upper_calculated_means_df$X1),]
  mean_check_df[counter,2] <- sum(!ifelse(lower$X2<=m & m<=upper$X2, TRUE, FALSE))
}




#################### for RCP17 ###############################################

mean_std_files <- list.files(pattern = "MeanStd")
mean_std_list <- list()
for (file in mean_std_files){
  
  mean_std_list[[file]] <- suppressMessages(read_csv(file)[-1,]) #remove first row which is not used in analysis
  
}

period_vect <- c("2015-11", "2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06")

a_class <- "Leave"
b_class <- "NoLeave"

lm_lower_calculated_means_df <- (lower_calculated_means_df[grepl(a_class, lower_calculated_means_df$X1),])
lm_upper_calculated_means_df <- (upper_calculated_means_df[grepl(a_class, upper_calculated_means_df$X1),])

mean_check_df <- data.frame(matrix(NA, ncol = 2))
counter <- 0
for (i in period_vect){
  counter <- counter+1
  mean_check_df[counter,1] <- i
  m <- mean_std_list[[str_c(i,"_MeanStd_",a_class,".csv")]][["Mean"]]
  lower <- lm_lower_calculated_means_df[grepl(str_c('Y-M',i), lm_lower_calculated_means_df$X1),]
  upper <- lm_upper_calculated_means_df[grepl(str_c('Y-M',i), lm_upper_calculated_means_df$X1),]
  mean_check_df[counter,2] <- sum(!ifelse(lower$X2<=m & m<=upper$X2, TRUE, FALSE))
}


#### For RCP18 #####


period_start <- 5
period_end <- 33

a_class <- "Change"
b_class <- "NoChange"

lm_lower_calculated_means_df <- (lower_calculated_means_df[grepl(a_class, lower_calculated_means_df$X1),])
lm_upper_calculated_means_df <- (upper_calculated_means_df[grepl(a_class, upper_calculated_means_df$X1),])

mean_check_df <- data.frame(matrix(NA, ncol = 2))
counter <- 0
for (i in period_start:period_end){
  counter <- counter+1
  mean_check_df[counter,1] <- i
  m_df <- mean_std_list[[str_c("Week",i,"_MeanStd_",a_class,".csv")]]
  lower <- lm_lower_calculated_means_df[grepl(str_c('Week',i), lm_lower_calculated_means_df$X1),]
  matching_dets <- gsub(".*\\_(.*)\\_.*", "\\1", lower$X1)
  upper <- lm_upper_calculated_means_df[grepl(str_c('Week',i), lm_upper_calculated_means_df$X1),]
  m <- m_df$Mean[m_df$Detector %in% matching_dets]
  mean_check_df[counter,2] <- sum(!ifelse(lower$X2<=m & m<=upper$X2, TRUE, FALSE))
}


# View(lm_lower_calculated_means_df[grepl(str_c('Week',10), lm_lower_calculated_means_df$X1),])
# View(mean_std_list[[str_c("Week",10,"_MeanStd_LineManagement.csv")]])
# View(as.data.frame(names(mean_std_list)))
