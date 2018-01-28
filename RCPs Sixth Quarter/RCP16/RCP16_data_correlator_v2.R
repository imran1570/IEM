#==============================================================================
#title           :	RCP16_data_correlator_v2.R
#description     :	Script to make the data correlated, obtained from RCP16_distribution_sampler.R and using the copula samples generated from RCP16_copula_sample_generator.R This script can use two type of distribution samples i.e. Gaussian or Triangular
#author          :	Muhammad Imran
#date            :	10 OCT 2017
#notes           :	This script uses the Mohanad's methodology (based on Mathworks paper) to introduce spearman correlation into data. 
#R_version       :	3.4.1 (64-bit)
#==============================================================================


#setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Sixth Quarter/RCP16/RCP16_Repo/") #this line is for the sake of R studio


#loading libraries and setting seed
library(MASS)
library(copula)
#library(BMS)
library(readr)
library(readxl)
library(stringr)
library(tidyverse)
#library(GenOrd) #for discrete dists
#library(wavethresh) #for vector rotation

#set.seed(123)

source("RCP16_helper_functions.R")


# num_orgs = 1
# 
# copula_type = "norm" #default
# 
# dist_sample_type = "gaussian" #default

input_directory_name = "./input_dir"
output_directory_name = "./output_dir"

start_org <- 1 #default = 1

end_org <- 1 #default = 1

number_of_given_obs = 10

number_of_estimation_obs = 10*4




full_features <- read_excel(str_c(input_directory_name,"/Full Features.xlsx"))



day_num_to_filter = 21

features <-  full_features %>% filter(day != day_num_to_filter)

features_dists_df = subset(features, select=-c(staff_id, day, Search_Threat, Exfil_Threat, Windows_User)) # removing day and staff id column

features_dists_df$Email_Sent <- as.integer(features_dists_df$Email_Sent)

features_dists_df$Email_Recv <- as.integer(features_dists_df$Email_Recv)

features_dists_df$USB_Transfers <- as.integer(features_dists_df$USB_Transfers)

features_dists_df$Searches <- as.integer(features_dists_df$Searches)

features_dists_df$Websites <- as.integer(features_dists_df$Websites)

features_dists_df$Accesses <- as.integer(features_dists_df$Accesses)

features_dists_df$Unique_Accesses <- as.integer(features_dists_df$Unique_Accesses)

staff_ids <- unique(features$staff_id)

#corr_df = cor(features_dists_df, method = "spearman")
# 
# continuous_corr_df = cor(continuous_detectors_df, method = "spearman")
# 
# discrete_corr_df = cor(discrete_detectors_df, method = "spearman")

### data importation section ###

for (num_org in start_org:end_org){
  
  gaussian_sampled_all_dist_df = read_csv(str_c(output_directory_name,"/gaussian_all_sampled_dist_df_org_",num_org,".csv"))
  
  norm_cop_sample_df = read_csv(str_c(output_directory_name,"/norm_cop_samples_df_org_",num_org,".csv"))
  
  triangular_sampled_all_dist_df = read_csv(str_c(output_directory_name,"/triangular_all_sampled_dist_df_org_",num_org,".csv"))
  
  t_cop_sample_df = read_csv(str_c(output_directory_name,"/t_cop_samples_df_org_",num_org,".csv"))

############

  # for the combination for normal copula, t copula and gaussian, triangular dist samples
  gauss_norm_correlated_data_df_list <- list()
  gauss_t_correlated_data_df_list <- list()
  tri_norm_correlated_data_df_list <- list()
  tri_t_correlated_data_df_list <- list()
  
 for (sid in staff_ids){
   
   
   gauss_user_df <- subset(filter(gaussian_sampled_all_dist_df, staff_id == sid), select=-c(staff_id, day))
   
   tri_user_df <- subset(filter(triangular_sampled_all_dist_df, staff_id == sid), select=-c(staff_id, day))
   
   # user_corr_df <- cor(user_df, method = "spearman")
   # 
   # user_corr_df[is.na(user_corr_df)] <- 0
   
   norm_user_cop_sample_df <- subset(filter(norm_cop_sample_df, staff_id == sid), select = -c(staff_id))
   
   t_user_cop_sample_df <- subset(filter(t_cop_sample_df, staff_id == sid), select = -c(staff_id))
   
   
   
   
   gauss_norm_correlated_data_df_list[[sid]] = CorrelateData(CorrelationMatrix = as.data.frame(norm_user_cop_sample_df), DetectorData = as.data.frame(gauss_user_df), CopulaMatrix = as.data.frame(norm_user_cop_sample_df))
   
   gauss_t_correlated_data_df_list[[sid]] = CorrelateData(CorrelationMatrix = as.data.frame(t_user_cop_sample_df), DetectorData = as.data.frame(gauss_user_df), CopulaMatrix = as.data.frame(t_user_cop_sample_df))
   
   tri_norm_correlated_data_df_list[[sid]] = CorrelateData(CorrelationMatrix = as.data.frame(norm_user_cop_sample_df), DetectorData = as.data.frame(tri_user_df), CopulaMatrix = as.data.frame(norm_user_cop_sample_df))
   
   tri_t_correlated_data_df_list[[sid]] = CorrelateData(CorrelationMatrix = as.data.frame(t_user_cop_sample_df), DetectorData = as.data.frame(tri_user_df), CopulaMatrix = as.data.frame(t_user_cop_sample_df))
   
  
 }

  gauss_norm_correlated_data_df <- bind_rows(gauss_norm_correlated_data_df_list)
  gauss_t_correlated_data_df <- bind_rows(gauss_t_correlated_data_df_list)
  tri_norm_correlated_data_df <- bind_rows(tri_norm_correlated_data_df_list)
  tri_t_correlated_data_df <- bind_rows(tri_t_correlated_data_df_list)
  
  
  
  gauss_norm_correlated_data_df <- data.frame(gaussian_sampled_all_dist_df$staff_id, gaussian_sampled_all_dist_df$day, gauss_norm_correlated_data_df)
  colnames(gauss_norm_correlated_data_df) <- colnames(gaussian_sampled_all_dist_df) 
  
  
  gauss_t_correlated_data_df <- data.frame(gaussian_sampled_all_dist_df$staff_id, gaussian_sampled_all_dist_df$day, gauss_t_correlated_data_df)
  colnames(gauss_t_correlated_data_df) <- colnames(gaussian_sampled_all_dist_df) 
  
  tri_norm_correlated_data_df <- data.frame(gaussian_sampled_all_dist_df$staff_id, gaussian_sampled_all_dist_df$day, tri_norm_correlated_data_df)
  colnames(tri_norm_correlated_data_df) <- colnames(gaussian_sampled_all_dist_df) 
  
  tri_t_correlated_data_df <- data.frame(gaussian_sampled_all_dist_df$staff_id, gaussian_sampled_all_dist_df$day, tri_t_correlated_data_df)
  colnames(tri_t_correlated_data_df) <- colnames(gaussian_sampled_all_dist_df) 
  
  
  #discrete_detectors_names <- colnames(as.data.frame(features_dists_df[, sapply(features_dists_df, class) == "integer"]))
  
  #continuous_detectors_names <- colnames(as.data.frame(features_dists_df[, sapply(features_dists_df, class) != "integer"]))
  
  
  # ##### data fixing#############
  email_sent_indeces <- gauss_norm_correlated_data_df$Email_Sent == 0
  gauss_norm_correlated_data_df$Avg_Attach_Sent[email_sent_indeces] <- 0
  gauss_norm_correlated_data_df$Total_Attach_Sent[email_sent_indeces] <- 0
  gauss_norm_correlated_data_df$Avg_Ext_Attach_Sent[email_sent_indeces] <- 0
  gauss_norm_correlated_data_df$Total_Ext_Attach_Sent[email_sent_indeces] <- 0
  gauss_norm_correlated_data_df$Percent_with_attach[email_sent_indeces] <- 0
  gauss_norm_correlated_data_df$Percent_Ext_Sent[email_sent_indeces] <- 0

  email_recv_indeces <- gauss_norm_correlated_data_df$Email_Recv == 0
  gauss_norm_correlated_data_df$Avg_Attach_Recv[email_recv_indeces] <- 0
  gauss_norm_correlated_data_df$Total_Attach_Recv[email_recv_indeces] <- 0
  gauss_norm_correlated_data_df$Avg_Ext_Attach_Recv[email_recv_indeces] <- 0
  gauss_norm_correlated_data_df$Total_Ext_Attach_Recv[email_recv_indeces] <- 0
  gauss_norm_correlated_data_df$Percent_Attach_Recv[email_recv_indeces] <- 0
  gauss_norm_correlated_data_df$Percent_Ext_Attach_Rec[email_recv_indeces] <- 0

  usb_trans_indeces <- gauss_norm_correlated_data_df$USB_Transfers == 0
  gauss_norm_correlated_data_df$Avg_USB_Transfer[usb_trans_indeces] <- 0
  gauss_norm_correlated_data_df$Total_USB_Transfer[usb_trans_indeces] <- 0

  websites_indices <- gauss_norm_correlated_data_df$Websites == 0
  gauss_norm_correlated_data_df$Avg_Website_size[websites_indices] <- 0
  gauss_norm_correlated_data_df$Total_Website_Size[websites_indices] <- 0
  gauss_norm_correlated_data_df$Avg_Website_Ext[websites_indices] <- 0
  gauss_norm_correlated_data_df$Total_Website_Ext[websites_indices] <- 0
  gauss_norm_correlated_data_df$Percent_Ext_Website[websites_indices] <- 0
  
  
  email_sent_indeces <- gauss_t_correlated_data_df$Email_Sent == 0
  gauss_t_correlated_data_df$Avg_Attach_Sent[email_sent_indeces] <- 0
  gauss_t_correlated_data_df$Total_Attach_Sent[email_sent_indeces] <- 0
  gauss_t_correlated_data_df$Avg_Ext_Attach_Sent[email_sent_indeces] <- 0
  gauss_t_correlated_data_df$Total_Ext_Attach_Sent[email_sent_indeces] <- 0
  gauss_t_correlated_data_df$Percent_with_attach[email_sent_indeces] <- 0
  gauss_t_correlated_data_df$Percent_Ext_Sent[email_sent_indeces] <- 0
  
  email_recv_indeces <- gauss_t_correlated_data_df$Email_Recv == 0
  gauss_t_correlated_data_df$Avg_Attach_Recv[email_recv_indeces] <- 0
  gauss_t_correlated_data_df$Total_Attach_Recv[email_recv_indeces] <- 0
  gauss_t_correlated_data_df$Avg_Ext_Attach_Recv[email_recv_indeces] <- 0
  gauss_t_correlated_data_df$Total_Ext_Attach_Recv[email_recv_indeces] <- 0
  gauss_t_correlated_data_df$Percent_Attach_Recv[email_recv_indeces] <- 0
  gauss_t_correlated_data_df$Percent_Ext_Attach_Rec[email_recv_indeces] <- 0
  
  usb_trans_indeces <- gauss_t_correlated_data_df$USB_Transfers == 0
  gauss_t_correlated_data_df$Avg_USB_Transfer[usb_trans_indeces] <- 0
  gauss_t_correlated_data_df$Total_USB_Transfer[usb_trans_indeces] <- 0
  
  websites_indices <- gauss_t_correlated_data_df$Websites == 0
  gauss_t_correlated_data_df$Avg_Website_size[websites_indices] <- 0
  gauss_t_correlated_data_df$Total_Website_Size[websites_indices] <- 0
  gauss_t_correlated_data_df$Avg_Website_Ext[websites_indices] <- 0
  gauss_t_correlated_data_df$Total_Website_Ext[websites_indices] <- 0
  gauss_t_correlated_data_df$Percent_Ext_Website[websites_indices] <- 0
  
  
  email_sent_indeces <- tri_norm_correlated_data_df$Email_Sent == 0
  tri_norm_correlated_data_df$Avg_Attach_Sent[email_sent_indeces] <- 0
  tri_norm_correlated_data_df$Total_Attach_Sent[email_sent_indeces] <- 0
  tri_norm_correlated_data_df$Avg_Ext_Attach_Sent[email_sent_indeces] <- 0
  tri_norm_correlated_data_df$Total_Ext_Attach_Sent[email_sent_indeces] <- 0
  tri_norm_correlated_data_df$Percent_with_attach[email_sent_indeces] <- 0
  tri_norm_correlated_data_df$Percent_Ext_Sent[email_sent_indeces] <- 0
  
  email_recv_indeces <- tri_norm_correlated_data_df$Email_Recv == 0
  tri_norm_correlated_data_df$Avg_Attach_Recv[email_recv_indeces] <- 0
  tri_norm_correlated_data_df$Total_Attach_Recv[email_recv_indeces] <- 0
  tri_norm_correlated_data_df$Avg_Ext_Attach_Recv[email_recv_indeces] <- 0
  tri_norm_correlated_data_df$Total_Ext_Attach_Recv[email_recv_indeces] <- 0
  tri_norm_correlated_data_df$Percent_Attach_Recv[email_recv_indeces] <- 0
  tri_norm_correlated_data_df$Percent_Ext_Attach_Rec[email_recv_indeces] <- 0
  
  usb_trans_indeces <- tri_norm_correlated_data_df$USB_Transfers == 0
  tri_norm_correlated_data_df$Avg_USB_Transfer[usb_trans_indeces] <- 0
  tri_norm_correlated_data_df$Total_USB_Transfer[usb_trans_indeces] <- 0
  
  websites_indices <- tri_norm_correlated_data_df$Websites == 0
  tri_norm_correlated_data_df$Avg_Website_size[websites_indices] <- 0
  tri_norm_correlated_data_df$Total_Website_Size[websites_indices] <- 0
  tri_norm_correlated_data_df$Avg_Website_Ext[websites_indices] <- 0
  tri_norm_correlated_data_df$Total_Website_Ext[websites_indices] <- 0
  tri_norm_correlated_data_df$Percent_Ext_Website[websites_indices] <- 0
  
  
  
  email_sent_indeces <- tri_t_correlated_data_df$Email_Sent == 0
  tri_t_correlated_data_df$Avg_Attach_Sent[email_sent_indeces] <- 0
  tri_t_correlated_data_df$Total_Attach_Sent[email_sent_indeces] <- 0
  tri_t_correlated_data_df$Avg_Ext_Attach_Sent[email_sent_indeces] <- 0
  tri_t_correlated_data_df$Total_Ext_Attach_Sent[email_sent_indeces] <- 0
  tri_t_correlated_data_df$Percent_with_attach[email_sent_indeces] <- 0
  tri_t_correlated_data_df$Percent_Ext_Sent[email_sent_indeces] <- 0
  
  email_recv_indeces <- tri_t_correlated_data_df$Email_Recv == 0
  tri_t_correlated_data_df$Avg_Attach_Recv[email_recv_indeces] <- 0
  tri_t_correlated_data_df$Total_Attach_Recv[email_recv_indeces] <- 0
  tri_t_correlated_data_df$Avg_Ext_Attach_Recv[email_recv_indeces] <- 0
  tri_t_correlated_data_df$Total_Ext_Attach_Recv[email_recv_indeces] <- 0
  tri_t_correlated_data_df$Percent_Attach_Recv[email_recv_indeces] <- 0
  tri_t_correlated_data_df$Percent_Ext_Attach_Rec[email_recv_indeces] <- 0
  
  usb_trans_indeces <- tri_t_correlated_data_df$USB_Transfers == 0
  tri_t_correlated_data_df$Avg_USB_Transfer[usb_trans_indeces] <- 0
  tri_t_correlated_data_df$Total_USB_Transfer[usb_trans_indeces] <- 0
  
  websites_indices <- tri_t_correlated_data_df$Websites == 0
  tri_t_correlated_data_df$Avg_Website_size[websites_indices] <- 0
  tri_t_correlated_data_df$Total_Website_Size[websites_indices] <- 0
  tri_t_correlated_data_df$Avg_Website_Ext[websites_indices] <- 0
  tri_t_correlated_data_df$Total_Website_Ext[websites_indices] <- 0
  tri_t_correlated_data_df$Percent_Ext_Website[websites_indices] <- 0
  
  
  write_csv(gauss_norm_correlated_data_df, str_c(output_directory_name,"/gauss_norm_six_week_data_df_org_",num_org,".csv"))
  write_csv(gauss_t_correlated_data_df, str_c(output_directory_name,"/gauss_t_six_week_data_df_org_",num_org,".csv"))
  write_csv(tri_norm_correlated_data_df, str_c(output_directory_name,"/tri_norm_six_week_data_df_org_",num_org,".csv"))
  write_csv(tri_t_correlated_data_df, str_c(output_directory_name,"/tri_t_six_week_data_df_org_",num_org,".csv"))

}

