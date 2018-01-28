#==============================================================================
#title           :	RCP16_distribution_sampler.R
#description     :	Script to generate samples from the detector distributions of the given features (10 days) for the new 30 days. This is at the user level of the given organization.
#author          :	Muhammad Imran
#date            :	25 Sep 2017
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
#library(GenOrd) #for discrete dists
#library(wavethresh) #for vector rotation

source("RCP16_helper_functions.R")

#set.seed(123)


num_of_over_samples <- 1000

#vector_of_kernel_bandwidth_alogrithms = c("nrd0", "nrd", "ucv", "bcv", "SJ")

#num_orgs = 1 # initialize with default of 1

input_directory_name = "./input_dir"

output_directory_name = "./output_dir"

start_org <- 1 #default = 1

end_org <- 1 #default = 1

#bandwidth_selection_alogrithm = "nrd0" #initializing with default

number_of_given_days <- 10

number_of_estimation_days <- 10*4

future_day_start <- 21

future_day_end <- 60

########################### code block for importing data and getting it into the right format #####################################

### data importation section ###

full_features <- read_excel(str_c(input_directory_name,"/Full Features.xlsx"))



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

continuous_detectors_df = as.data.frame(features_dists_df[, sapply(features_dists_df, class) != "integer"])

continuous_detectors_names = colnames(continuous_detectors_df[-1])

discrete_detectors_df = as.data.frame(features_dists_df[, sapply(features_dists_df, class) == "integer"])

discrete_detectors_names = colnames(discrete_detectors_df)

discrete_detectors_df$staff_id = features$staff_id

staff_ids <- unique(features$staff_id)

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
  
  samples_df <- data.frame(matrix(0, nrow = number_of_estimation_days, ncol = 2+length(continuous_detectors_names)))
  
  counter = 0
  
  #samples_df[,1] <- rep(sid, number_of_estimation_days)
  
  user_df <- filter(continuous_detectors_df, staff_id == sid)

  for (det in continuous_detectors_names){
  
  counter <-  counter + 1
  
  vect <- as.vector(user_df[,c(det)])
  
  sample_vect <- rnorm(n=num_of_over_samples, mean = mean(vect), sd = sd(vect))
  
  sample_vect <- sample_vect[sample_vect>=0]
  
  samples_df[,counter] <- sample(sample_vect, size = number_of_estimation_days, replace = F)
  
  }
  
  samples_df$X20 <- rep(sid, number_of_estimation_days)
  
  samples_df$X21 <- future_day_start:future_day_end
  
  colnames(samples_df) = c(continuous_detectors_names, "staff_id", "day") 
  
  sampled_continuous_dist_gaussian_df_list[[sid]] <- as.data.frame(samples_df)
  
}

sampled_continuous_dist_gaussian_df <- bind_rows(sampled_continuous_dist_gaussian_df_list)



################# end of code block for continuous distribution sampling with Gaussian Kernel ##########################

################# code block for continuous distribution sampling using Traingular kernel ##########################


sampled_continuous_dist_triangular_df_list <- list()

for (sid in staff_ids){
  
  samples_df <- data.frame(matrix(0, nrow = number_of_estimation_days, ncol = 2+length(continuous_detectors_names)))
  
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
  
  samples_df$X21 <- future_day_start:future_day_end
  
  colnames(samples_df) = c(continuous_detectors_names, "staff_id", "day") 
  
  sampled_continuous_dist_triangular_df_list[[sid]] <- as.data.frame(samples_df)
  
}

sampled_continuous_dist_triangular_df <- bind_rows(sampled_continuous_dist_triangular_df_list)


################# end of code block for continuous distribution sampling with Triangular Kernel ##########################




######### sampling of discrete detectors ##################


sampled_discrete_dist_df_list <- list()

for (sid in staff_ids){
  
  samples_df <- data.frame(matrix(0, nrow = number_of_estimation_days, ncol = 2+length(discrete_detectors_names)))
  
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
  
  samples_df$X9 <- future_day_start:future_day_end
  
  colnames(samples_df) = c(discrete_detectors_names, "staff_id", "day")
  
  sampled_discrete_dist_df_list[[sid]] <- as.data.frame(samples_df)
  
}


sampled_discrete_dist_df <- bind_rows(sampled_discrete_dist_df_list)


############ end of discrete detector sampling ###############################






sampled_all_dist_df <- as.data.frame(cbind(subset(sampled_continuous_dist_gaussian_df, select=-c(staff_id, day)), sampled_discrete_dist_df))

sampled_all_dist_df <- sampled_all_dist_df[colnames(subset(features, select=-c(Search_Threat, Exfil_Threat, Windows_User)))] # reordering columns

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

write_csv(sampled_all_dist_df, str_c(output_directory_name,"/triangular_all_sampled_dist_df_org_",num_org,".csv"))



}

