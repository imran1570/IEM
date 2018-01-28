#==============================================================================
#title           :	RCP16_distribution_sampler_usb_v2.R
#description     :	Script to generate samples from the detector distributions of the given features (10 days) for the new 30 days. This is at the user level of the given organization.
#author          :	Muhammad Imran
#date            :	19 OCT 2017
#notes           :	This approach is for utilization of this component towards final model
#R_version       :	3.4.1 (64-bit)
#==============================================================================


#setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Sixth Quarter/RCP16/RCP16_Repo/") #this line is for the sake of R studio



#loading libraries and setting seed
library(MASS)
#library(copula)
#library(BMS)
library(readr)
library(readxl)
library(stringr)
library(tidyverse)
library(triangle)
#library(truncnorm)
#library(GenOrd) #for discrete dists
#library(wavethresh) #for vector rotation

source("RCP16_helper_functions.R")

#set.seed(123)


num_of_over_samples <- 1000

#vector_of_kernel_bandwidth_alogrithms = c("nrd0", "nrd", "ucv", "bcv", "SJ")

#num_orgs = 1 # initialize with default of 1

input_directory_name = "./input_dir"

output_directory_name = "./output_dir_usb"

start_org <- 1

end_org <- 1

#bandwidth_selection_alogrithm = "nrd0" #initializing with default

number_of_given_days = 10

number_of_estimation_days = 10*3

future_day_start <- 21

future_day_end <- 50

########################### code block for importing data and getting it into the right format #####################################

### data importation section ###

full_features <- read_csv(str_c(input_directory_name,"/Full_Features_with_Group_ID.csv"))
day_num_to_filter = 21
features <-  full_features %>% filter(day != day_num_to_filter)

staff_ids <- unique(features$staff_id)

non_window_user_ids <- features %>% filter(Windows_User == 0) %>% .$staff_id %>% unique

window_user_ids <- features %>% filter(Windows_User == 1) %>% .$staff_id %>% unique

usb_feature_names <- c("USB_Transfers", "Avg_USB_Transfer", "Total_USB_Transfer")

exfil_user_ids <- features %>% filter(Exfil_Threat == 1) %>% .$staff_id %>% unique


# code block for introducing non-window user USB activity in given dataset

features_with_usb <- features

for (non_sid in non_window_user_ids){
  
  indeces <- (features$staff_id == non_sid)
  
  non_win_usr_grp_id <- features %>% filter(staff_id == non_sid) %>% .$group_id %>% first
  
  win_usrs_in_grp_df <- features %>% filter(staff_id %in% window_user_ids & group_id == non_win_usr_grp_id)
  
  if (non_sid %in% exfil_user_ids & sum(win_usrs_in_grp_df$Exfil_Threat) > 0){ #to check if the group also have windows-exfil users){
    
    features_with_usb[indeces,usb_feature_names] <- filter(win_usrs_in_grp_df, staff_id == sample(x = win_usrs_in_grp_df$staff_id[win_usrs_in_grp_df$staff_id %in% exfil_user_ids], size = 1)) %>% dplyr::select(usb_feature_names)
    
  } else {
    
    features_with_usb[indeces,usb_feature_names] <- filter(win_usrs_in_grp_df, staff_id == sample(x = win_usrs_in_grp_df$staff_id[!win_usrs_in_grp_df$staff_id %in% exfil_user_ids], size = 1)) %>% dplyr::select(usb_feature_names)
    
  }
}
write_csv(features_with_usb, str_c(output_directory_name,"/Full_Features_with_Group_USB.csv"))



features_dists_df = subset(features_with_usb, select=-c(day, Search_Threat, Exfil_Threat, Windows_User)) # removing day and staff id column

features_dists_df$Email_Sent <- as.integer(features_dists_df$Email_Sent)

features_dists_df$Email_Recv <- as.integer(features_dists_df$Email_Recv)

features_dists_df$USB_Transfers <- as.integer(features_dists_df$USB_Transfers)

features_dists_df$Searches <- as.integer(features_dists_df$Searches)

features_dists_df$Websites <- as.integer(features_dists_df$Websites)

features_dists_df$Accesses <- as.integer(features_dists_df$Accesses)

features_dists_df$Unique_Accesses <- as.integer(features_dists_df$Unique_Accesses)

continuous_detectors_df = as.data.frame(features_dists_df[, sapply(features_dists_df, class) != "integer"])

continuous_detectors_names = colnames(continuous_detectors_df[-1:-2]) # removing staff_id and group_id column names

discrete_detectors_df = as.data.frame(features_dists_df[, sapply(features_dists_df, class) == "integer"])

discrete_detectors_names = colnames(discrete_detectors_df)

discrete_detectors_df$staff_id = features_with_usb$staff_id

discrete_detectors_df$group_id <- features_with_usb$group_id


# corr_df = cor(features_dists_df, method = "spearman")
# 
# continuous_corr_df = cor(continuous_detectors_df, method = "spearman")
# 
# discrete_corr_df = cor(discrete_detectors_df, method = "spearman")
########################### end of code block for importing data and getting it into the right format #####################################



for (num_org in start_org:end_org){
  
  ################# code block for continuous distribution sampling using Gaussian kernel ##########################
  
  
  sampled_continuous_dist_gaussian_df_list <- list()
  
  for (sid in staff_ids){
    
    samples_df <- data.frame(matrix(0, nrow = number_of_estimation_days, ncol = 3+length(continuous_detectors_names))) # +3 is for appending group_id, student_id and day
    
    counter = 0
    
    #samples_df[,1] <- rep(sid, number_of_estimation_days)
    
    user_df <- filter(continuous_detectors_df, staff_id == sid)
    
    for (det in continuous_detectors_names){
      
      counter <-  counter + 1
      
      vect <- as.vector(user_df[,c(det)])
      
      sample_vect <- rnorm(n=num_of_over_samples, mean = mean(vect), sd = sd(vect))
      
      sample_vect <- sample_vect[sample_vect>=0]
      
      samples_df[,counter] <- sample(sample_vect, size = number_of_estimation_days, replace = F)
      
      #samples_df[,counter] <- sample_vect
      
    }
    
    samples_df$X20 <- rep(sid, number_of_estimation_days)
    
    user_group_id <- features_with_usb %>% filter(staff_id == sid) %>% .$group_id %>% first
    
    samples_df$X21 <- rep(user_group_id, number_of_estimation_days)
    
    samples_df$X22 <- future_day_start:future_day_end 
    
    colnames(samples_df) = c(continuous_detectors_names, "staff_id", "group_id", "day") 
    
    sampled_continuous_dist_gaussian_df_list[[sid]] <- as.data.frame(samples_df)
    
  }
  
  sampled_continuous_dist_gaussian_df <- bind_rows(sampled_continuous_dist_gaussian_df_list)
  
  
  
  ################# end of code block for continuous distribution sampling with Gaussian Kernel ##########################
  
  ################# code block for continuous distribution sampling using Traingular kernel ##########################
  
  
  sampled_continuous_dist_triangular_df_list <- list()
  
  for (sid in staff_ids){
    
    samples_df <- data.frame(matrix(0, nrow = number_of_estimation_days, ncol = 3+length(continuous_detectors_names))) # +3 is for appending group_id, student_id and day
    
    counter = 0
    
    #samples_df[,1] <- rep(sid, number_of_estimation_days)
    
    user_df <- filter(continuous_detectors_df, staff_id == sid)
    
    for (det in continuous_detectors_names){
      
      counter <-  counter + 1
      
      vect <- as.vector(user_df[,c(det)])
      
      sample_vect <- rtriangle(n=number_of_estimation_days, a = min(vect), b = max(vect), c=Mode(vect))
      
      #sample_vect <- sample_vect[sample_vect>=0]
      
      samples_df[,counter] <- sample_vect
      
    }
    
    samples_df$X20 <- rep(sid, number_of_estimation_days)
    
    user_group_id <- features_with_usb %>% filter(staff_id == sid) %>% .$group_id %>% first
    
    samples_df$X21 <- rep(user_group_id, number_of_estimation_days)
    
    samples_df$X22 <- future_day_start:future_day_end
    
    colnames(samples_df) = c(continuous_detectors_names, "staff_id", "group_id", "day")  
    
    sampled_continuous_dist_triangular_df_list[[sid]] <- as.data.frame(samples_df)
    
  }
  
  sampled_continuous_dist_triangular_df <- bind_rows(sampled_continuous_dist_triangular_df_list)
  
  
  ################# end of code block for continuous distribution sampling with Triangular Kernel ##########################
  
  
  
  
  ######### sampling of discrete detectors ##################
  
  
  sampled_discrete_dist_df_list <- list()
  
  for (sid in staff_ids){
    
    samples_df <- data.frame(matrix(0, nrow = number_of_estimation_days, ncol = 3+length(discrete_detectors_names))) # +3 is for appending group_id, student_id and day
    
    counter = 0
    
    #samples_df[,1] <- rep(sid, number_of_estimation_days)
    
    user_df <- filter(discrete_detectors_df, staff_id == sid)
    
    for (det in discrete_detectors_names){
      
      counter = counter + 1
      
      probs <- as.data.frame(table(user_df[,c(det)])/length(user_df[,c(det)]))
      
      probs_vect = probs$Freq[match(user_df[,c(det)], probs$Var1)]
      
      samples_df[,counter] = sample(user_df[,c(det)], number_of_estimation_days, replace=T, prob = probs_vect)
      
    }
    
    samples_df$X8 <- rep(sid, number_of_estimation_days)
    
    user_group_id <- features_with_usb %>% filter(staff_id == sid) %>% .$group_id %>% first
    
    samples_df$X9 <- rep(user_group_id, number_of_estimation_days)
    
    samples_df$X10 <- future_day_start:future_day_end
    
    colnames(samples_df) = c(discrete_detectors_names, "staff_id", "group_id", "day") 
    
    sampled_discrete_dist_df_list[[sid]] <- as.data.frame(samples_df)
    
  }
  
  
  sampled_discrete_dist_df <- bind_rows(sampled_discrete_dist_df_list)
  
  
  ############ end of discrete detector sampling ###############################
  
  
  
  
  
  ############# code block for replacing non-windows usb activity with windows usb activity from the same group ############################
  sampled_all_dist_df <- as.data.frame(cbind(subset(sampled_continuous_dist_gaussian_df, select=-c(staff_id, day)), sampled_discrete_dist_df))
  
  sampled_all_dist_df <- sampled_all_dist_df[colnames(subset(features, select=-c(Search_Threat, Exfil_Threat, Windows_User)))] # reordering columns
  
  # for (non_sid in non_window_user_ids){
  #   
  #   indeces <- (sampled_all_dist_df$staff_id == non_sid)
  #   
  #   non_win_usr_grp_id <- features %>% filter(staff_id == non_sid) %>% .$group_id %>% first
  #   
  #   win_usrs_in_grp_df <- features %>% filter(staff_id %in% window_user_ids & group_id == non_win_usr_grp_id)
  #   
  #   if (non_sid %in% exfil_user_ids & sum(win_usrs_in_grp_df$Exfil_Threat) > 0){ #to check if the group also have windows-exfil users
  #   
  #     sampled_all_dist_df[indeces,usb_feature_names] <- filter(win_usrs_in_grp_df, staff_id == sample(x = win_usrs_in_grp_df$staff_id[win_usrs_in_grp_df$staff_id %in% exfil_user_ids], size = 1)) %>% dplyr::select(usb_feature_names)
  #   
  #   } else {
  #     
  #     sampled_all_dist_df[indeces,usb_feature_names] <- filter(win_usrs_in_grp_df, staff_id == sample(x = win_usrs_in_grp_df$staff_id[!win_usrs_in_grp_df$staff_id %in% exfil_user_ids], size = 1)) %>% dplyr::select(usb_feature_names)
  #     
  #   }
  #   
  # }
  
  # ##### data fixing#############
  # email_sent_indeces <- sampled_all_dist_df$Email_Sent == 0
  # sampled_all_dist_df$Avg_Attach_Sent[email_sent_indeces] <- 0
  # sampled_all_dist_df$Total_Attach_Sent[email_sent_indeces] <- 0
  # sampled_all_dist_df$Avg_Ext_Attach_Sent[email_sent_indeces] <- 0
  # sampled_all_dist_df$Total_Ext_Attach_Sent[email_sent_indeces] <- 0
  # sampled_all_dist_df$Percent_with_attach[email_sent_indeces] <- 0
  # sampled_all_dist_df$Percent_Ext_Sent[email_sent_indeces] <- 0
  # 
  # email_recv_indeces <- sampled_all_dist_df$Email_Recv == 0
  # sampled_all_dist_df$Avg_Attach_Recv[email_recv_indeces] <- 0
  # sampled_all_dist_df$Total_Attach_Recv[email_recv_indeces] <- 0
  # sampled_all_dist_df$Avg_Ext_Attach_Recv[email_recv_indeces] <- 0
  # sampled_all_dist_df$Total_Ext_Attach_Recv[email_recv_indeces] <- 0
  # sampled_all_dist_df$Percent_Attach_Recv[email_recv_indeces] <- 0
  # sampled_all_dist_df$Percent_Ext_Attach_Rec[email_recv_indeces] <- 0
  # 
  # usb_trans_indeces <- sampled_all_dist_df$USB_Transfers == 0
  # sampled_all_dist_df$Avg_USB_Transfer[usb_trans_indeces] <- 0
  # sampled_all_dist_df$Total_USB_Transfer[usb_trans_indeces] <- 0
  # 
  # websites_indices <- sampled_all_dist_df$Websites == 0
  # sampled_all_dist_df$Avg_Website_size[websites_indices] <- 0
  # sampled_all_dist_df$Total_Website_Size[websites_indices] <- 0
  # sampled_all_dist_df$Avg_Website_Ext[websites_indices] <- 0
  # sampled_all_dist_df$Total_Website_Ext[websites_indices] <- 0
  # sampled_all_dist_df$Percent_Ext_Website[websites_indices] <- 0
   
  write_csv(sampled_all_dist_df, str_c(output_directory_name,"/gaussian_all_sampled_dist_df_org_",num_org,".csv"))
  
  
  
  
  
  sampled_all_dist_df <- as.data.frame(cbind(subset(sampled_continuous_dist_triangular_df, select=-c(staff_id, day)), sampled_discrete_dist_df))
  
  sampled_all_dist_df <- sampled_all_dist_df[colnames(subset(features, select=-c(Search_Threat, Exfil_Threat, Windows_User)))] # reordering columns
  
  # for (non_sid in non_window_user_ids){
  # 
  #   indeces <- (sampled_all_dist_df$staff_id == non_sid)
  # 
  #   non_win_usr_grp_id <- features %>% filter(staff_id == non_sid) %>% .$group_id %>% first
  # 
  #   win_usrs_in_grp_df <- features %>% filter(staff_id %in% window_user_ids & group_id == non_win_usr_grp_id)
  # 
  #   if (non_sid %in% exfil_user_ids & sum(win_usrs_in_grp_df$Exfil_Threat) > 0){ #to check if the group also have windows-exfil users){
  # 
  #     sampled_all_dist_df[indeces,usb_feature_names] <- filter(win_usrs_in_grp_df, staff_id == sample(x = win_usrs_in_grp_df$staff_id[win_usrs_in_grp_df$staff_id %in% exfil_user_ids], size = 1)) %>% dplyr::select(usb_feature_names)
  # 
  #   } else {
  # 
  #     sampled_all_dist_df[indeces,usb_feature_names] <- filter(win_usrs_in_grp_df, staff_id == sample(x = win_usrs_in_grp_df$staff_id[!win_usrs_in_grp_df$staff_id %in% exfil_user_ids], size = 1)) %>% dplyr::select(usb_feature_names)
  # 
  #   }
  # }
  
  
  # ##### data fixing#############
  # email_sent_indeces <- sampled_all_dist_df$Email_Sent == 0
  # sampled_all_dist_df$Avg_Attach_Sent[email_sent_indeces] <- 0
  # sampled_all_dist_df$Total_Attach_Sent[email_sent_indeces] <- 0
  # sampled_all_dist_df$Avg_Ext_Attach_Sent[email_sent_indeces] <- 0
  # sampled_all_dist_df$Total_Ext_Attach_Sent[email_sent_indeces] <- 0
  # sampled_all_dist_df$Percent_with_attach[email_sent_indeces] <- 0
  # sampled_all_dist_df$Percent_Ext_Sent[email_sent_indeces] <- 0
  # 
  # email_recv_indeces <- sampled_all_dist_df$Email_Recv == 0
  # sampled_all_dist_df$Avg_Attach_Recv[email_recv_indeces] <- 0
  # sampled_all_dist_df$Total_Attach_Recv[email_recv_indeces] <- 0
  # sampled_all_dist_df$Avg_Ext_Attach_Recv[email_recv_indeces] <- 0
  # sampled_all_dist_df$Total_Ext_Attach_Recv[email_recv_indeces] <- 0
  # sampled_all_dist_df$Percent_Attach_Recv[email_recv_indeces] <- 0
  # sampled_all_dist_df$Percent_Ext_Attach_Rec[email_recv_indeces] <- 0
  # 
  # usb_trans_indeces <- sampled_all_dist_df$USB_Transfers == 0
  # sampled_all_dist_df$Avg_USB_Transfer[usb_trans_indeces] <- 0
  # sampled_all_dist_df$Total_USB_Transfer[usb_trans_indeces] <- 0
  # 
  # websites_indices <- sampled_all_dist_df$Websites == 0
  # sampled_all_dist_df$Avg_Website_size[websites_indices] <- 0
  # sampled_all_dist_df$Total_Website_Size[websites_indices] <- 0
  # sampled_all_dist_df$Avg_Website_Ext[websites_indices] <- 0
  # sampled_all_dist_df$Total_Website_Ext[websites_indices] <- 0
  # sampled_all_dist_df$Percent_Ext_Website[websites_indices] <- 0
  # 
  
  write_csv(sampled_all_dist_df, str_c(output_directory_name,"/triangular_all_sampled_dist_df_org_",num_org,".csv"))
  
}

