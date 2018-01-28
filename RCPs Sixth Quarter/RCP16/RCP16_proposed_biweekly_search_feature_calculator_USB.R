#==============================================================================
#title           :	RCP16_proposed_biweekly_search_feature_calculator_USB.R
#description     :	This code calculates the proposed features on the correlated dataset generated from previous steps.
#author          :	Muhammad Imran
#date            :	11 OCT 2017
#notes           :	This code requires data from RCP16_data_correlator.R 
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
#library(GenOrd) #for discrete dists
library(wavethresh) #for vector rotation

source("RCP16_helper_functions.R")


input_directory_name = "./input_dir"
output_directory_name = "./output_dir_usb"

start_org <- 1

end_org <- 1

number_of_given_obs = 39430

number_of_estimation_obs = 39430*2

########################### code block for importing data and getting it into the right format #####################################

### data importation section ###

full_features <- read_csv(str_c(output_directory_name,"/Full_Features_with_Group_USB.csv"))



day_num_to_filter = 21

features <-  full_features %>% filter(day != day_num_to_filter)

staff_ids = unique(features$staff_id)

week_days <- c(41:50,11:40) # wrap around days sequence

required_week_day_num <- c(11:20,31:40) # the day numbers for which we have to calculate biweekly/bibiweekly features

look_back_start <- 1

look_back_end <- 10

# ########################### end of code block for importing data and getting it into the right format #####################################


for (num_org in start_org:end_org){
  
  gauss_norm_six_week_data_df = read_csv(str_c(output_directory_name,"/gauss_norm_six_week_data_df_org_",num_org,".csv"))
  gauss_t_six_week_data_df = read_csv(str_c(output_directory_name,"/gauss_t_six_week_data_df_org_",num_org,".csv"))
  tri_norm_six_week_data_df = read_csv(str_c(output_directory_name,"/tri_norm_six_week_data_df_org_",num_org,".csv"))
  tri_t_six_week_data_df = read_csv(str_c(output_directory_name,"/tri_t_six_week_data_df_org_",num_org,".csv"))
  
  
  gauss_norm_full_data_df <- as.data.frame(rbind(subset(features, select = -c(group_id, Search_Threat, Exfil_Threat, Windows_User)), gauss_norm_six_week_data_df))
  gauss_t_full_data_df <- as.data.frame(rbind(subset(features, select = -c(group_id, Search_Threat, Exfil_Threat, Windows_User)), gauss_t_six_week_data_df))
  tri_norm_full_data_df <- as.data.frame(rbind(subset(features, select = -c(group_id, Search_Threat, Exfil_Threat, Windows_User)), tri_norm_six_week_data_df))
  tri_t_full_data_df <- as.data.frame(rbind(subset(features, select = -c(group_id, Search_Threat, Exfil_Threat, Windows_User)), tri_t_six_week_data_df))
  
  
  
  gauss_norm_max_access_in_a_day_per_week_count <-  c()
  gauss_norm_max_search_in_a_day_per_week_count <- c()
  gauss_norm_max_website_in_a_day_per_week_count <- c()
  
  
  gauss_t_max_access_in_a_day_per_week_count <-  c()
  gauss_t_max_search_in_a_day_per_week_count <- c()
  gauss_t_max_website_in_a_day_per_week_count <- c()
  
  tri_norm_max_access_in_a_day_per_week_count <-  c()
  tri_norm_max_search_in_a_day_per_week_count <- c()
  tri_norm_max_website_in_a_day_per_week_count <- c()
  
  tri_t_max_access_in_a_day_per_week_count <-  c()
  tri_t_max_search_in_a_day_per_week_count <- c()
  tri_t_max_website_in_a_day_per_week_count <- c()
  
  
  # loop for proposed_weekly_search_features
  for (sid in staff_ids){
    
    #window_days <- guyrot(week_day_num, 6)
    
    for (day_num in required_week_day_num){ 
      
      #window_days <- guyrot(window_days, -1)
      
      #indeces = (gauss_norm_full_data_df$day == day_num & gauss_norm_full_data_df$staff_id == sid)
      gauss_norm_max_access_in_a_day_per_week_count <- c(gauss_norm_max_access_in_a_day_per_week_count, gauss_norm_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Accesses) %>% max)
      gauss_norm_max_search_in_a_day_per_week_count <- c(gauss_norm_max_search_in_a_day_per_week_count, gauss_norm_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Searches) %>% max)
      gauss_norm_max_website_in_a_day_per_week_count <- c(gauss_norm_max_website_in_a_day_per_week_count, gauss_norm_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Websites) %>% max)
      
      # print(str_c("sid: ", sid))
      # ##print(str_c("prop_feat: ", proposed_weekly_search_feature_names[ind]))
      # print(str_c("day_num: ", day_num))
      # # #print(str_c("g_feat: ", given_search_feature_names[ind]))
      # print(str_c("window_days looked back: ", week_days[(day_num-1):(day_num-5)]))
      # print(str_c("max: ", gauss_norm_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-1):(day_num-5)]) %>% select(Email_Recv) %>% max))
      
      
      #indeces = (gauss_t_full_data_df$day == day_num & gauss_t_full_data_df$staff_id == sid)
      gauss_t_max_access_in_a_day_per_week_count <- c(gauss_t_max_access_in_a_day_per_week_count, gauss_t_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Accesses) %>% max)
      gauss_t_max_search_in_a_day_per_week_count <- c(gauss_t_max_search_in_a_day_per_week_count, gauss_t_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Searches) %>% max)
      gauss_t_max_website_in_a_day_per_week_count <- c(gauss_t_max_website_in_a_day_per_week_count, gauss_t_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Websites) %>% max)
      
      #indeces = (tri_norm_full_data_df$day == day_num & tri_norm_full_data_df$staff_id == sid)
      tri_norm_max_access_in_a_day_per_week_count <- c(tri_norm_max_access_in_a_day_per_week_count, tri_norm_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Accesses) %>% max)
      tri_norm_max_search_in_a_day_per_week_count <- c(tri_norm_max_search_in_a_day_per_week_count, tri_norm_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Searches) %>% max)
      tri_norm_max_website_in_a_day_per_week_count <- c(tri_norm_max_website_in_a_day_per_week_count, tri_norm_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Websites) %>% max)
      
      
      #indeces = (tri_t_full_data_df$day == day_num & tri_t_full_data_df$staff_id == sid)
      tri_t_max_access_in_a_day_per_week_count <- c(tri_t_max_access_in_a_day_per_week_count, tri_t_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Accesses) %>% max)
      tri_t_max_search_in_a_day_per_week_count <- c(tri_t_max_search_in_a_day_per_week_count, tri_t_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Searches) %>% max)
      tri_t_max_website_in_a_day_per_week_count <- c(tri_t_max_website_in_a_day_per_week_count, tri_t_full_data_df %>% filter(staff_id == sid & day %in% week_days[(day_num-look_back_start):(day_num-look_back_end)]) %>% select(Websites) %>% max)
      
    }
    
  }
  
  
  gauss_norm_proposed_biweekly_search_features_df <- as.data.frame(cbind(gauss_norm_max_access_in_a_day_per_week_count, gauss_norm_max_search_in_a_day_per_week_count, gauss_norm_max_website_in_a_day_per_week_count))
  gauss_t_proposed_biweekly_search_features_df <- as.data.frame(cbind(gauss_t_max_access_in_a_day_per_week_count, gauss_t_max_search_in_a_day_per_week_count, gauss_t_max_website_in_a_day_per_week_count))
  tri_norm_proposed_biweekly_search_features_df <- as.data.frame(cbind(tri_norm_max_access_in_a_day_per_week_count, tri_norm_max_search_in_a_day_per_week_count, tri_norm_max_website_in_a_day_per_week_count))
  tri_t_proposed_biweekly_search_features_df <- as.data.frame(cbind(tri_t_max_access_in_a_day_per_week_count, tri_t_max_search_in_a_day_per_week_count, tri_t_max_website_in_a_day_per_week_count))
  
  colnames(gauss_norm_proposed_biweekly_search_features_df) <- c("max_access_in_a_day_per_biweek_count", "max_search_in_a_day_per_biweek_count", "max_website_in_a_day_per_biweek_count")
  colnames(gauss_t_proposed_biweekly_search_features_df) <- c("max_access_in_a_day_per_biweek_count", "max_search_in_a_day_per_biweek_count", "max_website_in_a_day_per_biweek_count")
  colnames(tri_norm_proposed_biweekly_search_features_df) <- c("max_access_in_a_day_per_biweek_count", "max_search_in_a_day_per_biweek_count", "max_website_in_a_day_per_biweek_count")
  colnames(tri_t_proposed_biweekly_search_features_df) <- c("max_access_in_a_day_per_biweek_count", "max_search_in_a_day_per_biweek_count", "max_website_in_a_day_per_biweek_count")
  
  write_csv(gauss_norm_proposed_biweekly_search_features_df, str_c(output_directory_name,"/gauss_norm_proposed_biweekly_search_features_org_",num_org,".csv"))
  write_csv(gauss_t_proposed_biweekly_search_features_df, str_c(output_directory_name,"/gauss_t_proposed_biweekly_search_features_org_",num_org,".csv"))
  write_csv(tri_norm_proposed_biweekly_search_features_df, str_c(output_directory_name,"/tri_norm_proposed_biweekly_search_features_org_",num_org,".csv"))
  write_csv(tri_t_proposed_biweekly_search_features_df, str_c(output_directory_name,"/tri_t_proposed_biweekly_search_features_org_",num_org,".csv"))
  
}