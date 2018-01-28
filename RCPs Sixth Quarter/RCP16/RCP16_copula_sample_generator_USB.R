#==============================================================================
#title           :	RCP16_copula_sample_generator.R
#description     :	Script to calculate the Spearman correlation matrix from the given data and generate correlated samples from the Gaussian and T copula
#author          :	Muhammad Imran
#date            :	03 OCT 2017
#notes           :	This is the final version to date
#R_version       :	3.4.1 (64-bit)
#==============================================================================


#setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Sixth Quarter/RCP16/RCP16_Repo/") #this line is for the sake of R studio



#loading libraries and setting seed
#library(MASS)
library(copula)
#library(BMS)
library(readr)
library(readxl)
library(stringr)
library(tidyverse)
#library(GenOrd) #for discrete dists
#library(wavethresh) #for vector rotation

source("RCP16_helper_functions.R")

#set.seed(123)



num_orgs = 1 # initialize with default of 1

input_directory_name = "./input_dir"

output_directory_name = "./output_dir_usb"

start_org <- 1 #default = 1

end_org <- 1 # default = 1

number_of_given_obs = 10

number_of_estimation_obs = 10*3

t_cop_dof <- 5

########################### code block for importing data and getting it into the right format #####################################

### data importation section ###
full_features <- read_csv(str_c(output_directory_name,"/Full_Features_with_Group_USB.csv"))


day_num_to_filter = 21

features <-  full_features %>% filter(day != day_num_to_filter)

features_dists_df = subset(features, select=-c(day, Search_Threat, Exfil_Threat, Windows_User)) # removing day and staff id column

features_dists_df$Email_Sent <- as.integer(features_dists_df$Email_Sent)

features_dists_df$Email_Recv <- as.integer(features_dists_df$Email_Recv)

features_dists_df$USB_Transfers <- as.integer(features_dists_df$USB_Transfers)

features_dists_df$Searches <- as.integer(features_dists_df$Searches)

features_dists_df$Websites <- as.integer(features_dists_df$Websites)

features_dists_df$Accesses <- as.integer(features_dists_df$Accesses)

features_dists_df$Unique_Accesses <- as.integer(features_dists_df$Unique_Accesses)

staff_ids <- unique(features$staff_id)

non_window_user_ids <- features %>% filter(Windows_User == 0) %>% .$staff_id %>% unique

window_user_ids <- features %>% filter(Windows_User == 1) %>% .$staff_id %>% unique

usb_feature_names <- c("USB_Transfers", "Avg_USB_Transfer", "Total_USB_Transfer")

# continuous_detectors_df = as.data.frame(features_dists_df[, sapply(features_dists_df, class) != "integer"])
# 
# continuous_detectors_names = colnames(continuous_detectors_df)
# 
# discrete_detectors_df = as.data.frame(features_dists_df[, sapply(features_dists_df, class) == "integer"])
# 
# discrete_detectors_names = colnames(discrete_detectors_df)

# corr_df = cor(features_dists_df, method = "spearman")

# continuous_corr_df = cor(continuous_detectors_df, method = "spearman")
# 
# discrete_corr_df = cor(discrete_detectors_df, method = "spearman")

########################### end of code block for importing data and getting it into the right format #####################################


for (num_org in start_org:end_org){
  
norm_cop_samples_list <- list()
  
t_cop_samples_list <- list()
  
  
  for (sid in staff_ids){

  user_df <- filter(features_dists_df, staff_id == sid) 
  
  user_corr_df <- cor(subset(user_df, select=-c(staff_id, group_id)), method = "spearman")
  
  user_corr_df[is.na(user_corr_df)] <- 0

  norm_cop_obj = normalCopula(param = P2p(user_corr_df), dispstr = "un", dim = dim(user_corr_df)[1]) # create gaussian copula oject

  t_cop_obj = tCopula(param = P2p(user_corr_df), dispstr = "un", dim = dim(user_corr_df)[1], df = t_cop_dof) # create t copula object
  
  group_id <- user_df$group_id
  
  staff_id <- user_df$staff_id

  norm_cop_samples_list[[sid]] = data.frame(group_id, staff_id, rCopula(copula = norm_cop_obj , n = number_of_estimation_obs)) #sample from copula object

  t_cop_samples_list[[sid]] = data.frame(group_id, staff_id, rCopula(copula = t_cop_obj , n = number_of_estimation_obs)) #sample from copula object

# norm_correlated_dist_df = data.frame(matrix(data = 0, nrow = number_of_estimation_obs, ncol = dim(continuous_corr_df)[1]))
# 
# t_correlated_dist_df = data.frame(matrix(data = 0, nrow = number_of_estimation_obs, ncol = dim(continuous_corr_df)[1]))

  
  }

  
norm_cop_samples <- bind_rows(norm_cop_samples_list)

colnames(norm_cop_samples) <- colnames(features_dists_df)

for (non_sid in non_window_user_ids){
  
  indeces <- (norm_cop_samples$staff_id == non_sid)
  
  non_win_usr_grp_id <- norm_cop_samples %>% filter(staff_id == non_sid) %>% .$group_id %>% first
  
  win_usrs_in_grp_df <- norm_cop_samples %>% filter(staff_id %in% window_user_ids & group_id == non_win_usr_grp_id)
  
  norm_cop_samples[indeces, usb_feature_names] <- filter(win_usrs_in_grp_df, staff_id == sample(x = win_usrs_in_grp_df$staff_id, size = 1)) %>% dplyr::select(usb_feature_names)
  
}



t_cop_samples <- bind_rows(t_cop_samples_list)

colnames(t_cop_samples) <- colnames(features_dists_df)

for (non_sid in non_window_user_ids){
  
  indeces <- (t_cop_samples$staff_id == non_sid)
  
  non_win_usr_grp_id <- t_cop_samples %>% filter(staff_id == non_sid) %>% .$group_id %>% first
  
  win_usrs_in_grp_df <- t_cop_samples %>% filter(staff_id %in% window_user_ids & group_id == non_win_usr_grp_id)
  
  t_cop_samples[indeces, usb_feature_names] <- filter(win_usrs_in_grp_df, staff_id == sample(x = win_usrs_in_grp_df$staff_id, size = 1)) %>% dplyr::select(usb_feature_names)
  
}


write_csv(as.data.frame(norm_cop_samples), str_c(output_directory_name,"/norm_cop_samples_df_org_",num_org,".csv"))

write_csv(as.data.frame(t_cop_samples), str_c(output_directory_name,"/t_cop_samples_df_org_",num_org,".csv"))

}
