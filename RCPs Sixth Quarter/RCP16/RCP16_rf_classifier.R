#==============================================================================
#title           :	RCP16_rf_classifier.R
#description     :	This code adds class labels to the data and then applies Naive Bayes and Random Forest classifiers to the data
#author          :	Muhammad Imran
#date            :	11 OCT 2017
#notes           :	This code requires data from RCP16_proposed_weekly_exfil_feature_calculator.R, RCP16_proposed_biweekly_search_feature_calculator.R,
#                   RCP16_proposed_biweekly_exfil_feature_calculator.R, RCP16_proposed_weekly_search_feature_calculator.R modules 
#R_version       :	3.4.1 (64-bit)
#==============================================================================


#loading libraries and setting seed
library(MASS)
library(copula)
#library(BMS)
library(readr)
library(readxl)
library(stringr)
library(tidyverse)
library(caret)
library(randomForest)
library(MLmetrics)
library(e1071)
#library(GenOrd) #for discrete dists
#library(wavethresh) #for vector rotation

#set.seed(123)
#setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Sixth Quarter/RCP16/RCP16_Repo/") #this line is for the sake of R studio

source("RCP16_helper_functions.R")


input_directory_name = "./input_dir"

output_directory_name = "./output_dir"

start_org <- 1

end_org <- 1

given_days = 31:40

estimated_days = 51:60

number_of_given_obs = 39430

number_of_estimation_obs = 39430*2


full_features <- read_excel(str_c(input_directory_name,"/Full Features.xlsx"))



day_num_to_filter = 21

features <-  full_features %>% filter(day != day_num_to_filter)

features$Search_Threat <- as.factor(features$Search_Threat)

features$Exfil_Threat <- as.factor(features$Exfil_Threat)

features$Windows_User <- as.factor(features$Windows_User)

features$Email_Sent <- as.integer(features$Email_Sent)

features$Email_Recv <- as.integer(features$Email_Recv)

features$USB_Transfers <- as.integer(features$USB_Transfers)

features$Searches <- as.integer(features$Searches)

features$Websites <- as.integer(features$Websites)

features$Accesses <- as.integer(features$Accesses)

features$Unique_Accesses <- as.integer(features$Unique_Accesses)




rf_search_precisions <- c()

rf_exfil_precisions <- c()

rf_search_recalls <- c()

rf_exfil_recalls <- c()

rf_search_fprs <- c()

rf_exfil_fprs <- c()




for (num_org in start_org:end_org){
  
  
  ############### code block for data importation and getting it into right format #####################################
  
  gauss_norm_six_week_data_df = read_csv(str_c(output_directory_name,"/gauss_norm_six_week_data_df_org_",num_org,".csv"))
  gauss_t_six_week_data_df = read_csv(str_c(output_directory_name,"/gauss_t_six_week_data_df_org_",num_org,".csv"))
  tri_norm_six_week_data_df = read_csv(str_c(output_directory_name,"/tri_norm_six_week_data_df_org_",num_org,".csv"))
  tri_t_six_week_data_df = read_csv(str_c(output_directory_name,"/tri_t_six_week_data_df_org_",num_org,".csv"))
  
  gauss_norm_proposed_weekly_exfil_features_df <- read_csv(str_c(output_directory_name,"/gauss_norm_proposed_weekly_exfil_features_org_",num_org,".csv"))
  gauss_t_proposed_weekly_exfil_features_df <- read_csv(str_c(output_directory_name,"/gauss_t_proposed_weekly_exfil_features_org_",num_org,".csv"))
  tri_norm_proposed_weekly_exfil_features_df <- read_csv(str_c(output_directory_name,"/tri_norm_proposed_weekly_exfil_features_org_",num_org,".csv"))
  tri_t_proposed_weekly_exfil_features_df <- read_csv(str_c(output_directory_name,"/tri_t_proposed_weekly_exfil_features_org_",num_org,".csv"))
  
  gauss_norm_proposed_biweekly_exfil_features_df <- read_csv(str_c(output_directory_name,"/gauss_norm_proposed_biweekly_exfil_features_org_",num_org,".csv"))
  gauss_t_proposed_biweekly_exfil_features_df <- read_csv(str_c(output_directory_name,"/gauss_t_proposed_biweekly_exfil_features_org_",num_org,".csv"))
  tri_norm_proposed_biweekly_exfil_features_df <- read_csv(str_c(output_directory_name,"/tri_norm_proposed_biweekly_exfil_features_org_",num_org,".csv"))
  tri_t_proposed_biweekly_exfil_features_df <- read_csv(str_c(output_directory_name,"/tri_t_proposed_biweekly_exfil_features_org_",num_org,".csv"))
  
  gauss_norm_proposed_weekly_search_features_df <- read_csv(str_c(output_directory_name,"/gauss_norm_proposed_weekly_search_features_org_",num_org,".csv"))
  gauss_t_proposed_weekly_search_features_df <- read_csv(str_c(output_directory_name,"/gauss_t_proposed_weekly_search_features_org_",num_org,".csv"))
  tri_norm_proposed_weekly_search_features_df <- read_csv(str_c(output_directory_name,"/tri_norm_proposed_weekly_search_features_org_",num_org,".csv"))
  tri_t_proposed_weekly_search_features_df <- read_csv(str_c(output_directory_name,"/tri_t_proposed_weekly_search_features_org_",num_org,".csv"))
  
  gauss_norm_proposed_biweekly_search_features_df <- read_csv(str_c(output_directory_name,"/gauss_norm_proposed_biweekly_search_features_org_",num_org,".csv"))
  gauss_t_proposed_biweekly_search_features_df <- read_csv(str_c(output_directory_name,"/gauss_t_proposed_biweekly_search_features_org_",num_org,".csv"))
  tri_norm_proposed_biweekly_search_features_df <- read_csv(str_c(output_directory_name,"/tri_norm_proposed_biweekly_search_features_org_",num_org,".csv"))
  tri_t_proposed_biweekly_search_features_df <- read_csv(str_c(output_directory_name,"/tri_t_proposed_biweekly_search_features_org_",num_org,".csv"))
  
  
  gauss_norm_full_data_df <- as.data.frame(rbind(filter(gauss_norm_six_week_data_df, day %in% given_days), filter(gauss_norm_six_week_data_df, day %in% estimated_days)))
  gauss_norm_full_data_df <- gauss_norm_full_data_df[order(gauss_norm_full_data_df[,c("staff_id")], gauss_norm_full_data_df[,c("day")]),]
  
  gauss_t_full_data_df <- as.data.frame(rbind(filter(gauss_t_six_week_data_df, day %in% given_days), filter(gauss_t_six_week_data_df, day %in% estimated_days)))
  gauss_t_full_data_df <- gauss_t_full_data_df[order(gauss_t_full_data_df[,c("staff_id")], gauss_t_full_data_df[,c("day")]),]
  
  tri_norm_full_data_df <- as.data.frame(rbind(filter(tri_norm_six_week_data_df, day %in% given_days), filter(tri_norm_six_week_data_df, day %in% estimated_days)))
  tri_norm_full_data_df <- tri_norm_full_data_df[order(tri_norm_full_data_df[,c("staff_id")], tri_norm_full_data_df[,c("day")]),]
  
  tri_t_full_data_df <- as.data.frame(rbind(filter(tri_t_six_week_data_df, day %in% given_days), filter(tri_t_six_week_data_df, day %in% estimated_days)))
  tri_t_full_data_df <- tri_t_full_data_df[order(tri_t_full_data_df[,c("staff_id")], tri_t_full_data_df[,c("day")]),]
  
  
  gauss_norm_full_data_with_prop_feat_df <- as.data.frame(cbind(gauss_norm_full_data_df,
                                                                gauss_norm_proposed_weekly_search_features_df,
                                                                gauss_norm_proposed_weekly_exfil_features_df,
                                                                gauss_norm_proposed_biweekly_search_features_df,
                                                                gauss_norm_proposed_biweekly_exfil_features_df))
  
  gauss_t_full_data_with_prop_feat_df <- as.data.frame(cbind(gauss_t_full_data_df,
                                                             gauss_t_proposed_weekly_search_features_df,
                                                             gauss_t_proposed_weekly_exfil_features_df,
                                                             gauss_t_proposed_biweekly_search_features_df,
                                                             gauss_t_proposed_biweekly_exfil_features_df))
  
  tri_norm_full_data_with_prop_feat_df <- as.data.frame(cbind(tri_norm_full_data_df,
                                                              tri_norm_proposed_weekly_search_features_df,
                                                              tri_norm_proposed_weekly_exfil_features_df,
                                                              tri_norm_proposed_biweekly_search_features_df,
                                                              tri_norm_proposed_biweekly_exfil_features_df))
  
  tri_t_full_data_with_prop_feat_df <- as.data.frame(cbind(tri_t_full_data_df,
                                                           tri_t_proposed_weekly_search_features_df,
                                                           tri_t_proposed_weekly_exfil_features_df,
                                                           tri_t_proposed_biweekly_search_features_df,
                                                           tri_t_proposed_biweekly_exfil_features_df))
  
  
  gauss_norm_given_data_with_prop_feat_df <- filter(gauss_norm_full_data_with_prop_feat_df, day %in% given_days)
  gauss_norm_given_data_with_prop_feat_df$Search_Threat <- features$Search_Threat
  gauss_norm_given_data_with_prop_feat_df$Exfil_Threat <- features$Exfil_Threat
  gauss_norm_estimated_data_with_prop_feat_df <- filter(gauss_norm_full_data_with_prop_feat_df, day %in% estimated_days)
  
  gauss_t_given_data_with_prop_feat_df <- filter(gauss_t_full_data_with_prop_feat_df, day %in% given_days)
  gauss_t_given_data_with_prop_feat_df$Search_Threat <- features$Search_Threat
  gauss_t_given_data_with_prop_feat_df$Exfil_Threat <- features$Exfil_Threat
  gauss_t_estimated_data_with_prop_feat_df <- filter(gauss_t_full_data_with_prop_feat_df, day %in% estimated_days)
  
  tri_norm_given_data_with_prop_feat_df <- filter(tri_norm_full_data_with_prop_feat_df, day %in% given_days)
  tri_norm_given_data_with_prop_feat_df$Search_Threat <- features$Search_Threat
  tri_norm_given_data_with_prop_feat_df$Exfil_Threat <- features$Exfil_Threat
  tri_norm_estimated_data_with_prop_feat_df <- filter(tri_norm_full_data_with_prop_feat_df, day %in% estimated_days)
  
  tri_t_given_data_with_prop_feat_df <- filter(tri_t_full_data_with_prop_feat_df, day %in% given_days)
  tri_t_given_data_with_prop_feat_df$Search_Threat <- features$Search_Threat
  tri_t_given_data_with_prop_feat_df$Exfil_Threat <- features$Exfil_Threat
  tri_t_estimated_data_with_prop_feat_df <- filter(tri_t_full_data_with_prop_feat_df, day %in% estimated_days)
  
  ################################################################
  
  
  
  ########## code block for random forest search threat model ##############
  
  control <- trainControl(method="cv", number=10)
  x <- 26
  mtry <- floor(sqrt(x))
  tunegrid <- expand.grid(.mtry=mtry)
  
  gauss_norm_rf_search_model <- train(Search_Threat ~ .,
                                      data=subset(gauss_norm_given_data_with_prop_feat_df, select = -c(staff_id, day, Exfil_Threat)), method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
  
  gauss_t_rf_search_model <- train(Search_Threat ~ .,
                                   data=subset(gauss_t_given_data_with_prop_feat_df, select = -c(staff_id, day, Exfil_Threat)), method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)

  tri_norm_rf_search_model <- train(Search_Threat ~ .,
                                    data=subset(tri_norm_given_data_with_prop_feat_df, select = -c(staff_id, day, Exfil_Threat)), method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)

  tri_t_rf_search_model <- train(Search_Threat ~ .,
                                 data=subset(tri_t_given_data_with_prop_feat_df, select = -c(staff_id, day, Exfil_Threat)), method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)

  
  # now test the model
  
  # gauss norm model against 4 configurations
  predictions <- predict(gauss_norm_rf_search_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_search_precisions <- c(rf_search_precisions, search_precision)
  rf_search_recalls <- c(rf_search_recalls, search_recall)
  rf_search_fprs <- c(rf_search_fprs, search_fpr)
  
  predictions <- predict(gauss_norm_rf_search_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_search_precisions <- c(rf_search_precisions, search_precision)
  rf_search_recalls <- c(rf_search_recalls, search_recall)
  rf_search_fprs <- c(rf_search_fprs, search_fpr)

  predictions <- predict(gauss_norm_rf_search_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_search_precisions <- c(rf_search_precisions, search_precision)
  rf_search_recalls <- c(rf_search_recalls, search_recall)
  rf_search_fprs <- c(rf_search_fprs, search_fpr)

  predictions <- predict(gauss_norm_rf_search_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_search_precisions <- c(rf_search_precisions, search_precision)
  rf_search_recalls <- c(rf_search_recalls, search_recall)
  rf_search_fprs <- c(rf_search_fprs, search_fpr)

  # # gauss t model against 4 configurations
  # predictions <- predict(gauss_t_rf_search_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # predictions <- predict(gauss_t_rf_search_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # predictions <- predict(gauss_t_rf_search_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # predictions <- predict(gauss_t_rf_search_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # # tri norm model against 4 configurations
  # predictions <- predict(tri_norm_rf_search_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # predictions <- predict(tri_norm_rf_search_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # predictions <- predict(tri_norm_rf_search_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # predictions <- predict(tri_norm_rf_search_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # # tri t model against 4 configurations
  # predictions <- predict(tri_t_rf_search_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # predictions <- predict(tri_t_rf_search_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # predictions <- predict(tri_t_rf_search_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  # predictions <- predict(tri_t_rf_search_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
  # search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_search_precisions <- c(rf_search_precisions, search_precision)
  # rf_search_recalls <- c(rf_search_recalls, search_recall)
  # rf_search_fprs <- c(rf_search_fprs, search_fpr)
  # 
  
  
  ################################################################
  
  ########## code block for random forest exfil threat model ##############
  
  control <- trainControl(method="cv", number=10)
  x <- 26
  mtry <- floor(sqrt(x))
  tunegrid <- expand.grid(.mtry=mtry)
  
  gauss_norm_rf_exfil_model <- train(Exfil_Threat ~ .,
                                     data=subset(gauss_norm_given_data_with_prop_feat_df, select = -c(staff_id, day, Search_Threat)), method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
  
  gauss_t_rf_exfil_model <- train(Exfil_Threat ~ .,
                                  data=subset(gauss_t_given_data_with_prop_feat_df, select = -c(staff_id, day, Search_Threat)), method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)

  tri_norm_rf_exfil_model <- train(Exfil_Threat ~ .,
                                   data=subset(tri_norm_given_data_with_prop_feat_df, select = -c(staff_id, day, Search_Threat)), method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)

  tri_t_rf_exfil_model <- train(Exfil_Threat ~ .,
                                data=subset(tri_t_given_data_with_prop_feat_df, select = -c(staff_id, day, Search_Threat)), method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)

  
  # now test the model
  
  # gauss norm model against 4 configurations
  predictions <- predict(gauss_norm_rf_exfil_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)

  predictions <- predict(gauss_norm_rf_exfil_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)

  predictions <- predict(gauss_norm_rf_exfil_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)

  predictions <- predict(gauss_norm_rf_exfil_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)

  # # gauss t model against 4 configurations
  # predictions <- predict(gauss_t_rf_exfil_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # predictions <- predict(gauss_t_rf_exfil_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # predictions <- predict(gauss_t_rf_exfil_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # predictions <- predict(gauss_t_rf_exfil_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # # tri norm model against 4 configurations
  # predictions <- predict(tri_norm_rf_exfil_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # predictions <- predict(tri_norm_rf_exfil_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # predictions <- predict(tri_norm_rf_exfil_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # predictions <- predict(tri_norm_rf_exfil_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # # tri t model against 4 configurations
  # predictions <- predict(tri_t_rf_exfil_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # predictions <- predict(tri_t_rf_exfil_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # predictions <- predict(tri_t_rf_exfil_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
  # 
  # predictions <- predict(tri_t_rf_exfil_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df, select = -c(staff_id, day)))
  # cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
  # exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
  # exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
  # exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  # rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
  # rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
  # rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)  
  
  ################################################################  
  
  
  
  gauss_norm_six_week_data <- str_c(output_directory_name,"/gauss_norm_six_week_data_df_org_",num_org+1,".csv")
  gauss_norm_proposed_weekly_exfil_features <- str_c(output_directory_name,"/gauss_norm_proposed_weekly_exfil_features_org_",num_org+1,".csv")
  gauss_norm_proposed_biweekly_exfil_features <- str_c(output_directory_name,"/gauss_norm_proposed_biweekly_exfil_features_org_",num_org+1,".csv")
  gauss_norm_proposed_weekly_search_features <- str_c(output_directory_name,"/gauss_norm_proposed_weekly_search_features_org_",num_org+1,".csv")
  gauss_norm_proposed_biweekly_search_features <- str_c(output_directory_name,"/gauss_norm_proposed_biweekly_search_features_org_",num_org+1,".csv")
  
  if (file.exists(gauss_norm_six_week_data) &
      file.exists(gauss_norm_proposed_weekly_exfil_features) &
      file.exists(gauss_norm_proposed_biweekly_exfil_features) &
      file.exists(gauss_norm_proposed_weekly_search_features) &
      file.exists(gauss_norm_proposed_biweekly_search_features)){
    
    
    gauss_norm_six_week_data_df2 = read_csv(str_c(output_directory_name,"/gauss_norm_six_week_data_df_org_",num_org+1,".csv"))
    gauss_t_six_week_data_df2 = read_csv(str_c(output_directory_name,"/gauss_t_six_week_data_df_org_",num_org+1,".csv"))
    tri_norm_six_week_data_df2 = read_csv(str_c(output_directory_name,"/tri_norm_six_week_data_df_org_",num_org+1,".csv"))
    tri_t_six_week_data_df2 = read_csv(str_c(output_directory_name,"/tri_t_six_week_data_df_org_",num_org+1,".csv"))
    
    
    gauss_norm_proposed_weekly_exfil_features_df2 <- read_csv(str_c(output_directory_name,"/gauss_norm_proposed_weekly_exfil_features_org_",num_org+1,".csv"))
    gauss_t_proposed_weekly_exfil_features_df2 <- read_csv(str_c(output_directory_name,"/gauss_t_proposed_weekly_exfil_features_org_",num_org+1,".csv"))
    tri_norm_proposed_weekly_exfil_features_df2 <- read_csv(str_c(output_directory_name,"/tri_norm_proposed_weekly_exfil_features_org_",num_org+1,".csv"))
    tri_t_proposed_weekly_exfil_features_df2 <- read_csv(str_c(output_directory_name,"/tri_t_proposed_weekly_exfil_features_org_",num_org+1,".csv"))
    
    gauss_norm_proposed_biweekly_exfil_features_df2 <- read_csv(str_c(output_directory_name,"/gauss_norm_proposed_biweekly_exfil_features_org_",num_org+1,".csv"))
    gauss_t_proposed_biweekly_exfil_features_df2 <- read_csv(str_c(output_directory_name,"/gauss_t_proposed_biweekly_exfil_features_org_",num_org+1,".csv"))
    tri_norm_proposed_biweekly_exfil_features_df2 <- read_csv(str_c(output_directory_name,"/tri_norm_proposed_biweekly_exfil_features_org_",num_org+1,".csv"))
    tri_t_proposed_biweekly_exfil_features_df2 <- read_csv(str_c(output_directory_name,"/tri_t_proposed_biweekly_exfil_features_org_",num_org+1,".csv"))
    
    gauss_norm_proposed_weekly_search_features_df2 <- read_csv(str_c(output_directory_name,"/gauss_norm_proposed_weekly_search_features_org_",num_org+1,".csv"))
    gauss_t_proposed_weekly_search_features_df2 <- read_csv(str_c(output_directory_name,"/gauss_t_proposed_weekly_search_features_org_",num_org+1,".csv"))
    tri_norm_proposed_weekly_search_features_df2 <- read_csv(str_c(output_directory_name,"/tri_norm_proposed_weekly_search_features_org_",num_org+1,".csv"))
    tri_t_proposed_weekly_search_features_df2 <- read_csv(str_c(output_directory_name,"/tri_t_proposed_weekly_search_features_org_",num_org+1,".csv"))
    
    
    gauss_norm_proposed_biweekly_search_features_df2 <- read_csv(str_c(output_directory_name,"/gauss_norm_proposed_biweekly_search_features_org_",num_org+1,".csv"))
    gauss_t_proposed_biweekly_search_features_df2 <- read_csv(str_c(output_directory_name,"/gauss_t_proposed_biweekly_search_features_org_",num_org+1,".csv"))
    tri_norm_proposed_biweekly_search_features_df2 <- read_csv(str_c(output_directory_name,"/tri_norm_proposed_biweekly_search_features_org_",num_org+1,".csv"))
    tri_t_proposed_biweekly_search_features_df2 <- read_csv(str_c(output_directory_name,"/tri_t_proposed_biweekly_search_features_org_",num_org+1,".csv"))
    
    
    
    gauss_norm_full_data_df2 <- as.data.frame(rbind(filter(gauss_norm_six_week_data_df2, day %in% given_days), filter(gauss_norm_six_week_data_df2, day %in% estimated_days)))
    gauss_norm_full_data_df2 <- gauss_norm_full_data_df2[order(gauss_norm_full_data_df2[,c("staff_id")], gauss_norm_full_data_df2[,c("day")]),]
    
    gauss_t_full_data_df2 <- as.data.frame(rbind(filter(gauss_t_six_week_data_df2, day %in% given_days), filter(gauss_t_six_week_data_df2, day %in% estimated_days)))
    gauss_t_full_data_df2 <- gauss_t_full_data_df[order(gauss_t_full_data_df2[,c("staff_id")], gauss_t_full_data_df2[,c("day")]),]
    
    tri_norm_full_data_df2 <- as.data.frame(rbind(filter(tri_norm_six_week_data_df2, day %in% given_days), filter(tri_norm_six_week_data_df2, day %in% estimated_days)))
    tri_norm_full_data_df2 <- tri_norm_full_data_df2[order(tri_norm_full_data_df2[,c("staff_id")], tri_norm_full_data_df2[,c("day")]),]
    
    tri_t_full_data_df2 <- as.data.frame(rbind(filter(tri_t_six_week_data_df2, day %in% given_days), filter(tri_t_six_week_data_df2, day %in% estimated_days)))
    tri_t_full_data_df2 <- tri_t_full_data_df2[order(tri_t_full_data_df2[,c("staff_id")], tri_t_full_data_df2[,c("day")]),]
    
    gauss_norm_full_data_with_prop_feat_df2 <- as.data.frame(cbind(gauss_norm_full_data_df2,
                                                                   gauss_norm_proposed_weekly_search_features_df2,
                                                                   gauss_norm_proposed_weekly_exfil_features_df2,
                                                                   gauss_norm_proposed_biweekly_search_features_df2,
                                                                   gauss_norm_proposed_biweekly_exfil_features_df2))
    
    gauss_t_full_data_with_prop_feat_df2 <- as.data.frame(cbind(gauss_t_full_data_df2,
                                                                gauss_t_proposed_weekly_search_features_df2,
                                                                gauss_t_proposed_weekly_exfil_features_df2,
                                                                gauss_t_proposed_biweekly_search_features_df2,
                                                                gauss_t_proposed_biweekly_exfil_features_df2))
    
    tri_norm_full_data_with_prop_feat_df2 <- as.data.frame(cbind(tri_norm_full_data_df2,
                                                                 tri_norm_proposed_weekly_search_features_df2,
                                                                 tri_norm_proposed_weekly_exfil_features_df2,
                                                                 tri_norm_proposed_biweekly_search_features_df2,
                                                                 tri_norm_proposed_biweekly_exfil_features_df2))
    
    tri_t_full_data_with_prop_feat_df2 <- as.data.frame(cbind(tri_t_full_data_df2,
                                                              tri_t_proposed_weekly_search_features_df2,
                                                              tri_t_proposed_weekly_exfil_features_df2,
                                                              tri_t_proposed_biweekly_search_features_df2,
                                                              tri_t_proposed_biweekly_exfil_features_df2))
    
    
    gauss_norm_given_data_with_prop_feat_df2 <- filter(gauss_norm_full_data_with_prop_feat_df2, day %in% given_days)
    gauss_norm_given_data_with_prop_feat_df2$Search_Threat <- features$Search_Threat
    gauss_norm_given_data_with_prop_feat_df2$Exfil_Threat <- features$Exfil_Threat
    gauss_norm_estimated_data_with_prop_feat_df2 <- filter(gauss_norm_full_data_with_prop_feat_df2, day %in% estimated_days)
    
    gauss_t_given_data_with_prop_feat_df2 <- filter(gauss_t_full_data_with_prop_feat_df2, day %in% given_days)
    gauss_t_given_data_with_prop_feat_df2$Search_Threat <- features$Search_Threat
    gauss_t_given_data_with_prop_feat_df2$Exfil_Threat <- features$Exfil_Threat
    gauss_t_estimated_data_with_prop_feat_df2 <- filter(gauss_t_full_data_with_prop_feat_df2, day %in% estimated_days)
    
    tri_norm_given_data_with_prop_feat_df2 <- filter(tri_norm_full_data_with_prop_feat_df2, day %in% given_days)
    tri_norm_given_data_with_prop_feat_df2$Search_Threat <- features$Search_Threat
    tri_norm_given_data_with_prop_feat_df2$Exfil_Threat <- features$Exfil_Threat
    tri_norm_estimated_data_with_prop_feat_df2 <- filter(tri_norm_full_data_with_prop_feat_df2, day %in% estimated_days)
    
    tri_t_given_data_with_prop_feat_df2 <- filter(tri_t_full_data_with_prop_feat_df2, day %in% given_days)
    tri_t_given_data_with_prop_feat_df2$Search_Threat <- features$Search_Threat
    tri_t_given_data_with_prop_feat_df2$Exfil_Threat <- features$Exfil_Threat
    tri_t_estimated_data_with_prop_feat_df2 <- filter(tri_t_full_data_with_prop_feat_df2, day %in% estimated_days)
    
    
    
    
    predictions <- predict(gauss_norm_rf_search_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(gauss_norm_rf_search_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(gauss_norm_rf_search_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(gauss_norm_rf_search_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    # gauss t model against 4 configurations
    predictions <- predict(gauss_t_rf_search_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(gauss_t_rf_search_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(gauss_t_rf_search_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(gauss_t_rf_search_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    # tri norm model against 4 configurations
    predictions <- predict(tri_norm_rf_search_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(tri_norm_rf_search_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(tri_norm_rf_search_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(tri_norm_rf_search_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    # tri t model against 4 configurations
    predictions <- predict(tri_t_rf_search_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(tri_t_rf_search_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(tri_t_rf_search_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    predictions <- predict(tri_t_rf_search_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Search_Threat, mode = "prec_recall", positive = "1")
    search_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    search_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    search_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_search_precisions <- c(rf_search_precisions, search_precision)
    rf_search_recalls <- c(rf_search_recalls, search_recall)
    rf_search_fprs <- c(rf_search_fprs, search_fpr)
    
    
    
    
    
    
    
    
    
    
    
    # gauss norm model against 4 configurations
    predictions <- predict(gauss_norm_rf_exfil_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(gauss_norm_rf_exfil_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(gauss_norm_rf_exfil_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(gauss_norm_rf_exfil_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    # gauss t model against 4 configurations
    predictions <- predict(gauss_t_rf_exfil_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(gauss_t_rf_exfil_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(gauss_t_rf_exfil_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(gauss_t_rf_exfil_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    # tri norm model against 4 configurations
    predictions <- predict(tri_norm_rf_exfil_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(tri_norm_rf_exfil_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(tri_norm_rf_exfil_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(tri_norm_rf_exfil_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    # tri t model against 4 configurations
    predictions <- predict(tri_t_rf_exfil_model, newdata = subset(gauss_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(tri_t_rf_exfil_model, newdata = subset(gauss_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = gauss_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(tri_t_rf_exfil_model, newdata = subset(tri_norm_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_norm_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)
    
    predictions <- predict(tri_t_rf_exfil_model, newdata = subset(tri_t_estimated_data_with_prop_feat_df2, select = -c(staff_id, day)))
    cfm_obj <- confusionMatrix(data = predictions, reference = tri_t_given_data_with_prop_feat_df$Exfil_Threat, mode = "prec_recall", positive = "1")
    exfil_precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
    exfil_recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
    exfil_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
    rf_exfil_precisions <- c(rf_exfil_precisions, exfil_precision)
    rf_exfil_recalls <- c(rf_exfil_recalls, exfil_recall)
    rf_exfil_fprs <- c(rf_exfil_fprs, exfil_fpr)  
    
  
    
    
  } else {
  
    print("No files found for another organization")
    break
  
    }
  
  
}



answers_df <- as.data.frame(cbind(rf_search_precisions, rf_search_recalls, rf_search_fprs, rf_exfil_precisions, rf_exfil_recalls, rf_exfil_fprs))

#write_csv(answers_df, str_c(output_directory_name,"/answers_1-12.csv"))

write_csv(answers_df, "./answers_7-12.csv")














