# This RCP17 population synthesis only induces detector to detector correlation
library(stringr)
library(readr)
library(dplyr)
library(MASS) #for mvrnorm
library(Matrix) # isSymmetric
library(matrixcalc) # is.positive.definite
library(copula)
library(caret)
library(randomForest)
library(MLmetrics)
library(e1071)
library(triangle)
#devtools::install_github("joshuaulrich/quantmod")#required to install DMwR
library(DMwR)#SMOTE
library(ROSE)#ROSE
library (ROCR)
options(scipen = 100)
source("helper_functions.R")

all_detectors <- c("001a","002a","002b","002c","002d","003a","004a","004b","004c","004d","005a","006a","006b","006c","006d","007a","008a","008b","008c","008d","009a","010a","011a","011b")
continuous_detectors <-c("001a","002a","002b","002c","002d","003a","004a","004b","004c","004d","005a","006a","006b","006c","006d","007a","008a","008b","008c","008d")
discrete_detectors <- c("009a","010a","011a","011b")
given_months <- c("2015-11", "2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06")
num_given_month <- length(given_months)
test_months <- c("2016-07", "2016-08", "2016-09")
num_test_month <- length(test_months)
reqd_column_names <- c("uid", "month", "label", "det001a","det002a","det002b","det002c","det002d","det003a","det004a","det004b","det004c","det004d","det005a","det006a","det006b","det006c","det006d","det007a","det008a","det008b","det008c","det008d","det009a","det010a","det011a","det011b")
total_num_detectors <- length(continuous_detectors)+length(discrete_detectors)

raw_corr_file_names <- list.files(path = str_c(getwd(), "/RCP17data/RCP17_detector_spearman_correlations")) #get the correlation file names in the data folder
corr_months <- str_split(raw_corr_file_names, "_") %>% sapply(function (x) x[1]) %>% unique #extracting month names from file names

assumption_one_months <- c("2016-06", "2016-01", "2016-02") #july august september are similar to june january february
assumption_two_months <- c("2016-04", "2016-05", "2016-06") #future three months are similar to past three months
assumption_three_months <- c("2016-03", "2016-04", "2016-05") #labor stats posted by freedman on basecamp

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




#### code block for sampling train period data ####

continuous_hist_list_leave <- list()
continuous_hist_list_noleave <- list()
discrete_hist_list_leave <- list()
discrete_hist_list_noleave <- list()

start_org <- 1
end_org <- 1

#loop for org
for (org_num in start_org:end_org){
  
  list_of_all_detector_samples_leave_train <- list()
  list_of_all_detector_samples_noleave_train <- list()
  
  for(month in given_months){
    
    # cont_det_counter <- 0
    # disc_det_counter <- 0
    cont_det_leave_vect_list <- list() #temporary storage list of continuous detector samples vectors for leave group
    cont_det_noleave_vect_list <- list() #temporary storage list of continuous detector samples vectors for no leave group
    disc_det_leave_vect_list <- list() #temporary storage list of discrete detector samples vectors for leave group
    disc_det_noleave_vect_list <- list() #temporary storage list of discrete detector samples vectors for no leave group
    
    for(det in continuous_detectors){
      
      file_name_leave <- str_c(month, "_det", det, "_Leave.csv")
      file_name_noleave <- str_c(month, "_det", det, "_NoLeave.csv")
      continuous_hist_list_leave[[file_name_leave]] <- suppressMessages(read_csv(str_c(getwd(), "/RCP17data/", file_name_leave))) #load histograms as into list of dataframes
      continuous_hist_list_noleave[[file_name_noleave]] <- suppressMessages(read_csv(str_c(getwd(), "/RCP17data/", file_name_noleave))) #load histograms as into list of dataframes
      continuous_hist_leave <- continuous_hist_list_leave[[file_name_leave]] #extract histogram from list at a time for usage
      continuous_hist_noleave <- continuous_hist_list_noleave[[file_name_noleave]]
      #for leave group
      unbinned_data_vect <- c()
      count_zeros <- continuous_hist_leave$counts[continuous_hist_leave$lower==0 & continuous_hist_leave$upper==0] #count the zeros in the zero bin
      unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
      det_hist_no_zeros <- continuous_hist_leave[(continuous_hist_leave$lower!=0 | continuous_hist_leave$upper!=0) & (continuous_hist_leave$counts!=0),] #filter out the bins with no data points
      if (nrow(det_hist_no_zeros)>0){
        #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros # filter out the count of zeros from the bins which contain zero data points in them
        for (i in 1:nrow(det_hist_no_zeros)){
          unbinned_data_vect <- c(unbinned_data_vect,
                                  rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
        }
      }#end of if statement
      cont_det_leave_vect_list[[det]] <- unbinned_data_vect
      
      #for no leave group
      unbinned_data_vect <- c()
      count_zeros <- continuous_hist_noleave$counts[continuous_hist_noleave$lower==0 & continuous_hist_noleave$upper==0]
      unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
      det_hist_no_zeros <- continuous_hist_noleave[(continuous_hist_noleave$lower!=0 | continuous_hist_noleave$upper!=0) & (continuous_hist_noleave$counts!=0),]
      if (nrow(det_hist_no_zeros)>0){
        #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
        for (i in 1:nrow(det_hist_no_zeros)){
          unbinned_data_vect <- c(unbinned_data_vect,
                                  rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
        }
      } #end of If statement
      cont_det_noleave_vect_list[[det]] <- unbinned_data_vect
      
    } #end of continuous detectors loop
    cont_detector_samples_leave_df <- bind_cols(cont_det_leave_vect_list)
    colnames(cont_detector_samples_leave_df) <- continuous_detectors
    cont_detector_samples_noleave_df <- bind_cols(cont_det_noleave_vect_list)  
    colnames(cont_detector_samples_noleave_df) <- continuous_detectors
    
    for(det in discrete_detectors){
      
      file_name_leave <- str_c(month, "_det", det, "_Leave.csv")
      file_name_noleave <- str_c(month, "_det", det, "_NoLeave.csv")
      discrete_hist_list_leave[[file_name_leave]] <- suppressMessages(read_csv(str_c(getwd(), "/RCP17data/", file_name_leave)))
      discrete_hist_list_noleave[[file_name_noleave]] <- suppressMessages(read_csv(str_c(getwd(), "/RCP17data/", file_name_noleave)))
      discrete_hist_leave <- discrete_hist_list_leave[[file_name_leave]]
      discrete_hist_noleave <- discrete_hist_list_noleave[[file_name_noleave]]
      #for leave group
      unbinned_data_vect <- c()
      count_zeros <- discrete_hist_leave$counts[discrete_hist_leave$lower==0 & discrete_hist_leave$upper==0]
      unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
      det_hist_no_zeros <- discrete_hist_leave[(discrete_hist_leave$lower!=0 | discrete_hist_leave$upper!=0) & (discrete_hist_leave$counts!=0),]
      if (nrow(det_hist_no_zeros)>0){
        #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
        for (i in 1:nrow(det_hist_no_zeros)){
          unbinned_data_vect <- c(unbinned_data_vect, 
                                  rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
        }
      }#end of If statement
      disc_det_leave_vect_list[[det]] <- round(unbinned_data_vect)
      #for no leave group
      unbinned_data_vect <- c()
      count_zeros <- discrete_hist_noleave$counts[discrete_hist_noleave$lower==0 & discrete_hist_noleave$upper==0]
      unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
      det_hist_no_zeros <- discrete_hist_noleave[(discrete_hist_noleave$lower!=0 | discrete_hist_noleave$upper!=0) & (discrete_hist_noleave$counts!=0),]
      if (nrow(det_hist_no_zeros)>0){
        #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
        for (i in 1:nrow(det_hist_no_zeros)){
          unbinned_data_vect <- c(unbinned_data_vect, rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
        }
      } #end of If statement
      disc_det_noleave_vect_list[[det]] <- round(unbinned_data_vect)
      
    } # end of discrete detectors loop
    disc_detector_samples_leave_df <- bind_cols(disc_det_leave_vect_list)
    colnames(disc_detector_samples_leave_df) <- discrete_detectors
    disc_detector_samples_noleave_df <- bind_cols(disc_det_noleave_vect_list)
    colnames(disc_detector_samples_noleave_df) <- discrete_detectors
    
    
    all_detector_samples_leave <- bind_cols(list(cont_detector_samples_leave_df, disc_detector_samples_leave_df))
    all_detector_samples_leave <- all_detector_samples_leave[order(colnames(all_detector_samples_leave))]
    all_detector_samples_noleave <- bind_cols(list(cont_detector_samples_noleave_df, disc_detector_samples_noleave_df))
    all_detector_samples_noleave <- all_detector_samples_noleave[order(colnames(all_detector_samples_noleave))]
    
    list_of_all_detector_samples_leave_train[[month]] <- all_detector_samples_leave
    list_of_all_detector_samples_noleave_train[[month]] <- all_detector_samples_noleave
    
  } #end of months loop
  
  
  
  ### following code block is to generate test period data ###
  
  
  test_months_assumption <- c(1,2,3)
  assumption_token <- sample(test_months_assumption, 1)
  
  list_of_all_detector_samples_leave_test <- list()
  list_of_all_detector_samples_noleave_test <- list()
  test_month_counter <- 0
  
  for (month in test_months){
    
    test_month_counter <- test_month_counter+1
    
    if (assumption_token == 1){
      
      cont_det_leave_vect_list <- list() #temporary storage list of continuous detector samples vectors for leave group
      cont_det_noleave_vect_list <- list() #temporary storage list of continuous detector samples vectors for no leave group
      disc_det_leave_vect_list <- list() #temporary storage list of discrete detector samples vectors for leave group
      disc_det_noleave_vect_list <- list() #temporary storage list of discrete detector samples vectors for no leave group
      #for continuous detectors
      for(det in continuous_detectors){
        file_name_leave <- str_c(assumption_one_months[test_month_counter], "_det", det, "_Leave.csv")
        file_name_noleave <- str_c(assumption_one_months[test_month_counter], "_det", det, "_NoLeave.csv")
        continuous_hist_leave <- continuous_hist_list_leave[[file_name_leave]] #extract histogram from list at a time for usage
        continuous_hist_noleave <- continuous_hist_list_noleave[[file_name_noleave]]
        #for leave group
        unbinned_data_vect <- c()
        count_zeros <- continuous_hist_leave$counts[continuous_hist_leave$lower==0 & continuous_hist_leave$upper==0] #count the zeros in the zero bin
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- continuous_hist_leave[(continuous_hist_leave$lower!=0 & continuous_hist_leave$upper!=0) & (continuous_hist_leave$counts!=0),] #filter out the bins with no data points
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros # filter out the count of zeros from the bins which contain zero data points in them
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect,
                                    rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        }#end of if statement
        cont_det_leave_vect_list[[det]] <- unbinned_data_vect
        
        #for noleave group
        unbinned_data_vect <- c()
        count_zeros <- continuous_hist_noleave$counts[continuous_hist_noleave$lower==0 & continuous_hist_noleave$upper==0]
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- continuous_hist_noleave[(continuous_hist_noleave$lower!=0 & continuous_hist_noleave$upper!=0) & (continuous_hist_noleave$counts!=0),]
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect,
                                    rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        } #end of If statement
        cont_det_noleave_vect_list[[det]] <- unbinned_data_vect
      } #end of continuous detectors loop
      cont_detector_samples_leave_df <- bind_cols(cont_det_leave_vect_list)
      colnames(cont_detector_samples_leave_df) <- continuous_detectors
      cont_detector_samples_noleave_df <- bind_cols(cont_det_noleave_vect_list)  
      colnames(cont_detector_samples_noleave_df) <- continuous_detectors
      #for discrete detectors
      for(det in discrete_detectors){
        
        file_name_leave <- str_c(assumption_one_months[test_month_counter], "_det", det, "_Leave.csv")
        file_name_noleave <- str_c(assumption_one_months[test_month_counter], "_det", det, "_NoLeave.csv")
        discrete_hist_leave <- discrete_hist_list_leave[[file_name_leave]]
        discrete_hist_noleave <- discrete_hist_list_noleave[[file_name_noleave]]
        #for leave group
        unbinned_data_vect <- c()
        count_zeros <- discrete_hist_leave$counts[discrete_hist_leave$lower==0 & discrete_hist_leave$upper==0]
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- discrete_hist_leave[(discrete_hist_leave$lower!=0 & discrete_hist_leave$upper!=0) & (discrete_hist_leave$counts!=0),]
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect, 
                                    rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        }#end of If statement
        disc_det_leave_vect_list[[det]] <- round(unbinned_data_vect)
        #for no leave group
        unbinned_data_vect <- c()
        count_zeros <- discrete_hist_noleave$counts[discrete_hist_noleave$lower==0 & discrete_hist_noleave$upper==0]
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- discrete_hist_noleave[(discrete_hist_noleave$lower!=0 & discrete_hist_noleave$upper!=0) & (discrete_hist_noleave$counts!=0),]
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect, rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        } #end of If statement
        disc_det_noleave_vect_list[[det]] <- round(unbinned_data_vect)
        
      } # end of discrete detectors loop
      disc_detector_samples_leave_df <- bind_cols(disc_det_leave_vect_list)
      colnames(disc_detector_samples_leave_df) <- discrete_detectors
      disc_detector_samples_noleave_df <- bind_cols(disc_det_noleave_vect_list)
      colnames(disc_detector_samples_noleave_df) <- discrete_detectors
      
      
      all_detector_samples_leave <- bind_cols(list(cont_detector_samples_leave_df, disc_detector_samples_leave_df))
      all_detector_samples_leave <- all_detector_samples_leave[order(colnames(all_detector_samples_leave))]
      all_detector_samples_noleave <- bind_cols(list(cont_detector_samples_noleave_df, disc_detector_samples_noleave_df))
      all_detector_samples_noleave <- all_detector_samples_noleave[order(colnames(all_detector_samples_noleave))]
      
      list_of_all_detector_samples_leave_test[[month]] <- all_detector_samples_leave
      list_of_all_detector_samples_noleave_test[[month]] <- all_detector_samples_noleave
      
    } else if (assumption_token == 2){
      
      cont_det_leave_vect_list <- list() #temporary storage list of continuous detector samples vectors for leave group
      cont_det_noleave_vect_list <- list() #temporary storage list of continuous detector samples vectors for no leave group
      disc_det_leave_vect_list <- list() #temporary storage list of discrete detector samples vectors for leave group
      disc_det_noleave_vect_list <- list() #temporary storage list of discrete detector samples vectors for no leave group
      #for continuous detectors
      for(det in continuous_detectors){
        file_name_leave <- str_c(assumption_two_months[test_month_counter], "_det", det, "_Leave.csv")
        file_name_noleave <- str_c(assumption_two_months[test_month_counter], "_det", det, "_NoLeave.csv")
        continuous_hist_leave <- continuous_hist_list_leave[[file_name_leave]] #extract histogram from list at a time for usage
        continuous_hist_noleave <- continuous_hist_list_noleave[[file_name_noleave]]
        #for leave group
        unbinned_data_vect <- c()
        count_zeros <- continuous_hist_leave$counts[continuous_hist_leave$lower==0 & continuous_hist_leave$upper==0] #count the zeros in the zero bin
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- continuous_hist_leave[(continuous_hist_leave$lower!=0 & continuous_hist_leave$upper!=0) & (continuous_hist_leave$counts!=0),] #filter out the bins with no data points
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros # filter out the count of zeros from the bins which contain zero data points in them
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect,
                                    rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        }#end of if statement
        cont_det_leave_vect_list[[det]] <- unbinned_data_vect
        
        #for noleave group
        unbinned_data_vect <- c()
        count_zeros <- continuous_hist_noleave$counts[continuous_hist_noleave$lower==0 & continuous_hist_noleave$upper==0]
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- continuous_hist_noleave[(continuous_hist_noleave$lower!=0 & continuous_hist_noleave$upper!=0) & (continuous_hist_noleave$counts!=0),]
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect,
                                    rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        } #end of If statement
        cont_det_noleave_vect_list[[det]] <- unbinned_data_vect
      } #end of continuous detectors loop
      cont_detector_samples_leave_df <- bind_cols(cont_det_leave_vect_list)
      colnames(cont_detector_samples_leave_df) <- continuous_detectors
      cont_detector_samples_noleave_df <- bind_cols(cont_det_noleave_vect_list)  
      colnames(cont_detector_samples_noleave_df) <- continuous_detectors
      #for discrete detectors
      for(det in discrete_detectors){
        
        file_name_leave <- str_c(assumption_two_months[test_month_counter], "_det", det, "_Leave.csv")
        file_name_noleave <- str_c(assumption_two_months[test_month_counter], "_det", det, "_NoLeave.csv")
        discrete_hist_leave <- discrete_hist_list_leave[[file_name_leave]]
        discrete_hist_noleave <- discrete_hist_list_noleave[[file_name_noleave]]
        #for leave group
        unbinned_data_vect <- c()
        count_zeros <- discrete_hist_leave$counts[discrete_hist_leave$lower==0 & discrete_hist_leave$upper==0]
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- discrete_hist_leave[(discrete_hist_leave$lower!=0 & discrete_hist_leave$upper!=0) & (discrete_hist_leave$counts!=0),]
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect, 
                                    rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        }#end of If statement
        disc_det_leave_vect_list[[det]] <- round(unbinned_data_vect)
        #for no leave group
        unbinned_data_vect <- c()
        count_zeros <- discrete_hist_noleave$counts[discrete_hist_noleave$lower==0 & discrete_hist_noleave$upper==0]
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- discrete_hist_noleave[(discrete_hist_noleave$lower!=0 & discrete_hist_noleave$upper!=0) & (discrete_hist_noleave$counts!=0),]
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect, rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        } #end of If statement
        disc_det_noleave_vect_list[[det]] <- round(unbinned_data_vect)
        
      } # end of discrete detectors loop
      disc_detector_samples_leave_df <- bind_cols(disc_det_leave_vect_list)
      colnames(disc_detector_samples_leave_df) <- discrete_detectors
      disc_detector_samples_noleave_df <- bind_cols(disc_det_noleave_vect_list)
      colnames(disc_detector_samples_noleave_df) <- discrete_detectors
      
      
      all_detector_samples_leave <- bind_cols(list(cont_detector_samples_leave_df, disc_detector_samples_leave_df))
      all_detector_samples_leave <- all_detector_samples_leave[order(colnames(all_detector_samples_leave))]
      all_detector_samples_noleave <- bind_cols(list(cont_detector_samples_noleave_df, disc_detector_samples_noleave_df))
      all_detector_samples_noleave <- all_detector_samples_noleave[order(colnames(all_detector_samples_noleave))]
      
      list_of_all_detector_samples_leave_test[[month]] <- all_detector_samples_leave
      list_of_all_detector_samples_noleave_test[[month]] <- all_detector_samples_noleave
      
    } # end of else of the assumption token
    
    else {
      
      cont_det_leave_vect_list <- list() #temporary storage list of continuous detector samples vectors for leave group
      cont_det_noleave_vect_list <- list() #temporary storage list of continuous detector samples vectors for no leave group
      disc_det_leave_vect_list <- list() #temporary storage list of discrete detector samples vectors for leave group
      disc_det_noleave_vect_list <- list() #temporary storage list of discrete detector samples vectors for no leave group
      #for continuous detectors
      for(det in continuous_detectors){
        file_name_leave <- str_c(assumption_three_months[test_month_counter], "_det", det, "_Leave.csv")
        file_name_noleave <- str_c(assumption_three_months[test_month_counter], "_det", det, "_NoLeave.csv")
        continuous_hist_leave <- continuous_hist_list_leave[[file_name_leave]] #extract histogram from list at a time for usage
        continuous_hist_noleave <- continuous_hist_list_noleave[[file_name_noleave]]
        #for leave group
        unbinned_data_vect <- c()
        count_zeros <- continuous_hist_leave$counts[continuous_hist_leave$lower==0 & continuous_hist_leave$upper==0] #count the zeros in the zero bin
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- continuous_hist_leave[(continuous_hist_leave$lower!=0 & continuous_hist_leave$upper!=0) & (continuous_hist_leave$counts!=0),] #filter out the bins with no data points
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros # filter out the count of zeros from the bins which contain zero data points in them
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect,
                                    rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        }#end of if statement
        cont_det_leave_vect_list[[det]] <- unbinned_data_vect
        
        #for noleave group
        unbinned_data_vect <- c()
        count_zeros <- continuous_hist_noleave$counts[continuous_hist_noleave$lower==0 & continuous_hist_noleave$upper==0]
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- continuous_hist_noleave[(continuous_hist_noleave$lower!=0 & continuous_hist_noleave$upper!=0) & (continuous_hist_noleave$counts!=0),]
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect,
                                    rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        } #end of If statement
        cont_det_noleave_vect_list[[det]] <- unbinned_data_vect
      } #end of continuous detectors loop
      cont_detector_samples_leave_df <- bind_cols(cont_det_leave_vect_list)
      colnames(cont_detector_samples_leave_df) <- continuous_detectors
      cont_detector_samples_noleave_df <- bind_cols(cont_det_noleave_vect_list)  
      colnames(cont_detector_samples_noleave_df) <- continuous_detectors
      #for discrete detectors
      for(det in discrete_detectors){
        
        file_name_leave <- str_c(assumption_three_months[test_month_counter], "_det", det, "_Leave.csv")
        file_name_noleave <- str_c(assumption_three_months[test_month_counter], "_det", det, "_NoLeave.csv")
        discrete_hist_leave <- discrete_hist_list_leave[[file_name_leave]]
        discrete_hist_noleave <- discrete_hist_list_noleave[[file_name_noleave]]
        #for leave group
        unbinned_data_vect <- c()
        count_zeros <- discrete_hist_leave$counts[discrete_hist_leave$lower==0 & discrete_hist_leave$upper==0]
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- discrete_hist_leave[(discrete_hist_leave$lower!=0 & discrete_hist_leave$upper!=0) & (discrete_hist_leave$counts!=0),]
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect, 
                                    rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        }#end of If statement
        disc_det_leave_vect_list[[det]] <- round(unbinned_data_vect)
        #for no leave group
        unbinned_data_vect <- c()
        count_zeros <- discrete_hist_noleave$counts[discrete_hist_noleave$lower==0 & discrete_hist_noleave$upper==0]
        unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
        det_hist_no_zeros <- discrete_hist_noleave[(discrete_hist_noleave$lower!=0 & discrete_hist_noleave$upper!=0) & (discrete_hist_noleave$counts!=0),]
        if (nrow(det_hist_no_zeros)>0){
          #det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
          for (i in 1:nrow(det_hist_no_zeros)){
            unbinned_data_vect <- c(unbinned_data_vect, rtriangle(n=det_hist_no_zeros$counts[i], a = det_hist_no_zeros$lower[i], b = det_hist_no_zeros$upper[i], c = (det_hist_no_zeros$lower[i]+det_hist_no_zeros$upper[i])/2))
          }
        } #end of If statement
        disc_det_noleave_vect_list[[det]] <- round(unbinned_data_vect)
        
      } # end of discrete detectors loop
      disc_detector_samples_leave_df <- bind_cols(disc_det_leave_vect_list)
      colnames(disc_detector_samples_leave_df) <- discrete_detectors
      disc_detector_samples_noleave_df <- bind_cols(disc_det_noleave_vect_list)
      colnames(disc_detector_samples_noleave_df) <- discrete_detectors
      
      
      all_detector_samples_leave <- bind_cols(list(cont_detector_samples_leave_df, disc_detector_samples_leave_df))
      all_detector_samples_leave <- all_detector_samples_leave[order(colnames(all_detector_samples_leave))]
      all_detector_samples_noleave <- bind_cols(list(cont_detector_samples_noleave_df, disc_detector_samples_noleave_df))
      all_detector_samples_noleave <- all_detector_samples_noleave[order(colnames(all_detector_samples_noleave))]
      
      list_of_all_detector_samples_leave_test[[month]] <- all_detector_samples_leave
      list_of_all_detector_samples_noleave_test[[month]] <- all_detector_samples_noleave
      
    } #end of ELSE
    
  } # end of test month loop
  
  
  ### code block to randomize the arrangement of detector samples in each month's population. This is to avoid any fluke spearman correlation ###
  
  list_of_all_detector_samples_leave_train <- lapply(list_of_all_detector_samples_leave_train, row_randomizer_by_column) #imported helper function
  list_of_all_detector_samples_noleave_train <- lapply(list_of_all_detector_samples_noleave_train, row_randomizer_by_column)
  list_of_all_detector_samples_leave_test <- lapply(list_of_all_detector_samples_leave_test, row_randomizer_by_column)
  list_of_all_detector_samples_noleave_test <- lapply(list_of_all_detector_samples_noleave_test, row_randomizer_by_column)
  
  ###  end of code block for randomization ###
  
  
  
  # doing the temporal correlation requires equal population across months so I'm oversampling and undersampling populations to make them equal to average population size
  leave_train_pop_sizes <- lapply(list_of_all_detector_samples_leave_train, function(df){nrow(df)}) %>% as.numeric
  noleave_train_pop_sizes <- lapply(list_of_all_detector_samples_noleave_train, function(df){nrow(df)}) %>% as.numeric
  leave_train_avg_pop_size <- mean(leave_train_pop_sizes) %>% round
  noleave_train_avg_pop_size <- mean(noleave_train_pop_sizes) %>% round
  #loop to over-sample and under-sample populations to have equal length populations across months
  # aps stands for average population size
  list_of_all_detector_samples_leave_train <- lapply(list_of_all_detector_samples_leave_train, 
                                                     function(df, aps){if (nrow(df) < aps){
                                                       nrows <- aps - nrow(df)
                                                       df <- bind_rows(df, df[sample(nrows),])
                                                     } else {
                                                       nrows <- aps
                                                       df <- df[sample(nrows),]
                                                     } 
                                                     }, aps = leave_train_avg_pop_size)
  
  
  list_of_all_detector_samples_noleave_train <- lapply(list_of_all_detector_samples_noleave_train, 
                                                       function(df, aps){if (nrow(df) < aps){
                                                         nrows <- aps - nrow(df)
                                                         df <- bind_rows(df, df[sample(nrows),])
                                                       } else {
                                                         nrows <- aps
                                                         df <- df[sample(nrows),]
                                                       } 
                                                       }, aps = noleave_train_avg_pop_size)
  
  
  # now treating testing data
  leave_test_pop_sizes <- lapply(list_of_all_detector_samples_leave_test, function(df){nrow(df)}) %>% as.numeric
  noleave_test_pop_sizes <- lapply(list_of_all_detector_samples_noleave_test, function(df){nrow(df)}) %>% as.numeric
  leave_test_avg_pop_size <- mean(leave_test_pop_sizes) %>% round
  noleave_test_avg_pop_size <- mean(noleave_test_pop_sizes) %>% round
  
  list_of_all_detector_samples_leave_test <- lapply(list_of_all_detector_samples_leave_test, 
                                                    function(df, aps){if (nrow(df) < aps){
                                                      nrows <- aps - nrow(df)
                                                      df <- bind_rows(df, df[sample(nrows),])
                                                    } else {
                                                      nrows <- aps
                                                      df <- df[sample(nrows),]
                                                    } 
                                                    }, aps = leave_test_avg_pop_size)
  
  list_of_all_detector_samples_noleave_test <- lapply(list_of_all_detector_samples_noleave_test, 
                                                      function(df, aps){if (nrow(df) < aps){
                                                        nrows <- aps - nrow(df)
                                                        df <- bind_rows(df, df[sample(nrows),])
                                                      } else {
                                                        nrows <- aps
                                                        df <- df[sample(nrows),]
                                                      } 
                                                      }, aps = noleave_test_avg_pop_size)
  
  
  
  
  list_obj_storage <- dummy_2dcorrelated_data_generator(all_detectors = all_detectors,
                                                        months = given_months,
                                                        leave_avg_pop_size = leave_train_avg_pop_size,
                                                        noleave_avg_pop_size = noleave_train_avg_pop_size,
                                                        list_of_time_corr_mats = list_of_time_corr_mats,
                                                        det_to_det_corr_leave_list = det_to_det_train_corr_leave_list,
                                                        det_to_det_corr_noleave_list = det_to_det_train_corr_noleave_list)
  
  list_of_time_and_det_correlated_gaussian_samples_leave_train <- list_obj_storage[[1]] #list at 1st index is for leave group
  list_of_time_and_det_correlated_gaussian_samples_noleave_train <- list_obj_storage[[2]] #list at 2nd index is for noleave group
  
  correlated_train_leave_list <- list()
  correlated_train_noleave_list <- list()
  correlated_train_list <- list()
  for (month in given_months){
    
    correlated_train_leave_list[[month]] <- DataCorrelationCopier(uncorrelated_data = as.data.frame(list_of_all_detector_samples_leave_train[[month]]), chol_correlated_data = as.data.frame(list_of_time_and_det_correlated_gaussian_samples_leave_train[[month]]))
    correlated_train_leave_list[[month]]["month"] <- rep(month, nrow(correlated_train_leave_list[[month]])) #adding month column
    correlated_train_leave_list[[month]]["label"] <- rep(1, nrow(correlated_train_leave_list[[month]]))
    correlated_train_noleave_list[[month]] <- DataCorrelationCopier(uncorrelated_data = as.data.frame(list_of_all_detector_samples_noleave_train[[month]]), chol_correlated_data = as.data.frame(list_of_time_and_det_correlated_gaussian_samples_noleave_train[[month]]))
    correlated_train_noleave_list[[month]]["month"] <- rep(month, nrow(correlated_train_noleave_list[[month]])) #adding month column
    correlated_train_noleave_list[[month]]["label"] <- rep(0, nrow(correlated_train_noleave_list[[month]]))
    correlated_train_list[[month]] <- bind_rows(correlated_train_noleave_list[[month]], correlated_train_leave_list[[month]])
    correlated_train_list[[month]]["uid"] <- 1:nrow(correlated_train_list[[month]])
  }
 
  
  
  
  
  # dim = length(test_months)
  # dim_calc <- dim * (dim - 1) / 2
  # loc <- which(given_months == names(det_to_det_test_corr_leave_list)[1])
  # corr_col_identifier <- function(loc){
  #   var <- length(given_months)
  #   var2 <- 0
  #   for (i in 1:loc){
  #     var <- var - 1
  #     var2 <- var2+var
  #   }
  #   return(var2)
  # }
  # start_col <- corr_col_identifier(loc) - dim_calc + 1
  # end_col <- corr_col_identifier(loc)

  #### correlating the test period data according to the assumption ####    
  
  if (assumption_token == 1)
  {
    det_to_det_test_corr_leave_list <-  det_to_det_train_corr_leave_list[names(det_to_det_train_corr_leave_list) %in% assumption_one_months]
    det_to_det_test_corr_noleave_list <- det_to_det_train_corr_noleave_list[names(det_to_det_train_corr_noleave_list) %in% assumption_one_months]
    
    
    
    list_obj_storage_test <- dummy_2dcorrelated_data_generator(all_detectors = all_detectors,
                                                          months = assumption_one_months,
                                                          leave_avg_pop_size = leave_test_avg_pop_size,
                                                          noleave_avg_pop_size = noleave_test_avg_pop_size,
                                                          list_of_time_corr_mats = lapply(list_of_time_corr_mats, function(df,row,col){df[row,col]}, row = c(which(given_months %in% assumption_one_months)), col = c(which(given_months %in% assumption_one_months))), #to filter out the correlations of assumption months only
                                                          det_to_det_corr_leave_list = det_to_det_test_corr_leave_list,
                                                          det_to_det_corr_noleave_list = det_to_det_test_corr_noleave_list)
    
  } else if (assumption_token == 2)
  {
    det_to_det_test_corr_leave_list <-  det_to_det_train_corr_leave_list[names(det_to_det_train_corr_leave_list) %in% assumption_two_months]
    det_to_det_test_corr_noleave_list <- det_to_det_train_corr_noleave_list[names(det_to_det_train_corr_noleave_list) %in% assumption_two_months]
    
    list_obj_storage_test <- dummy_2dcorrelated_data_generator(all_detectors = all_detectors,
                                                          months = assumption_two_months,
                                                          leave_avg_pop_size = leave_test_avg_pop_size,
                                                          noleave_avg_pop_size = noleave_test_avg_pop_size,
                                                          list_of_time_corr_mats = lapply(list_of_time_corr_mats, function(df,row,col){df[row,col]}, row = c(which(given_months %in% assumption_two_months)), col = c(which(given_months %in% assumption_two_months))), #to filter out the correlations of assumption months only
                                                          det_to_det_corr_leave_list = det_to_det_test_corr_leave_list,
                                                          det_to_det_corr_noleave_list = det_to_det_test_corr_noleave_list)
    
  } else
  {
    det_to_det_test_corr_leave_list <-  det_to_det_train_corr_leave_list[names(det_to_det_train_corr_leave_list) %in% assumption_three_months]
    det_to_det_test_corr_noleave_list <- det_to_det_train_corr_noleave_list[names(det_to_det_train_corr_noleave_list) %in% assumption_three_months]
    
    list_obj_storage_test <- dummy_2dcorrelated_data_generator(all_detectors = all_detectors,
                                                          months = assumption_three_months,
                                                          leave_avg_pop_size = leave_test_avg_pop_size,
                                                          noleave_avg_pop_size = noleave_test_avg_pop_size,
                                                          list_of_time_corr_mats = lapply(list_of_time_corr_mats, function(df,row,col){df[row,col]}, row = c(which(given_months %in% assumption_three_months)), col = c(which(given_months %in% assumption_three_months))), #to filter out the correlations of assumption months only
                                                          det_to_det_corr_leave_list = det_to_det_test_corr_leave_list,
                                                          det_to_det_corr_noleave_list = det_to_det_test_corr_noleave_list)
    
  }
  
  list_of_time_and_det_correlated_gaussian_samples_leave_test <- list_obj_storage_test[[1]] #list at 1st index is for leave group
  names(list_of_time_and_det_correlated_gaussian_samples_leave_test) <- names(list_of_all_detector_samples_leave_test) #mask month names of gaussian correlated samples list with test month names
  list_of_time_and_det_correlated_gaussian_samples_noleave_test <- list_obj_storage_test[[2]] #list at 2nd index is for noleave group
  names(list_of_time_and_det_correlated_gaussian_samples_noleave_test) <- names(list_of_all_detector_samples_noleave_test) #mask month names of gaussian correlated samples list with test month names
  
  
  correlated_test_leave_list <- list()
  correlated_test_noleave_list <- list()
  correlated_test_list <- list()
  for (month in test_months){
    
    correlated_test_leave_list[[month]] <- DataCorrelationCopier(uncorrelated_data = as.data.frame(list_of_all_detector_samples_leave_test[[month]]), chol_correlated_data = as.data.frame(list_of_time_and_det_correlated_gaussian_samples_leave_test[[month]]))
    correlated_test_leave_list[[month]]["month"] <- rep(month, nrow(correlated_test_leave_list[[month]])) #adding month column
    correlated_test_leave_list[[month]]["label"] <- rep(1, nrow(correlated_test_leave_list[[month]]))
    correlated_test_noleave_list[[month]] <- DataCorrelationCopier(uncorrelated_data = as.data.frame(list_of_all_detector_samples_noleave_test[[month]]), chol_correlated_data = as.data.frame(list_of_time_and_det_correlated_gaussian_samples_noleave_test[[month]]))
    correlated_test_noleave_list[[month]]["month"] <- rep(month, nrow(correlated_test_noleave_list[[month]])) #adding month column
    correlated_test_noleave_list[[month]]["label"] <- rep(0, nrow(correlated_test_noleave_list[[month]]))
    correlated_test_list[[month]] <- bind_rows(correlated_test_noleave_list[[month]], correlated_test_leave_list[[month]])
    correlated_test_list[[month]]["uid"] <- 1:nrow(correlated_test_list[[month]])
  }
  
  # correlated_train_list <- lapply(correlated_train_list, function(df){cbind(df, uid = 1:nrow(df))}) #assigning user ids
  # correlated_test_list <- lapply(correlated_test_list, function(df){cbind(df, uid = 1:nrow(df))}) #assigning user ids
  # correlated_train_list <- lapply(correlated_train_list, function(df){df[sample(nrow(df)),]}) #randomizing rows
  # correlated_test_list <- lapply(correlated_test_list, function(df){df[sample(nrow(df)),]}) #randomizing rows
  
  all_months_pop <- bind_rows(correlated_train_list, correlated_test_list)
  colnames(all_months_pop)[1:total_num_detectors] <- c("det001a","det002a","det002b","det002c","det002d","det003a","det004a","det004b","det004c","det004d","det005a","det006a","det006b","det006c","det006d","det007a","det008a","det008b","det008c","det008d","det009a","det010a","det011a","det011b")
  all_months_pop <- all_months_pop[reqd_column_names]
  
  write_csv(all_months_pop, str_c(getwd(), "/populations_0125/all_months_2dcorr_pop_",org_num,".csv"))
  
  
} # end of org loop


# tmp <- filter(all_months_pop, month=="2016-01" & label==1) %>% .$det001a
# tmp2 <- filter(all_months_pop, month=="2016-02" & label==1) %>% .$det001a
# tmp3 <- filter(all_months_pop, month=="2016-03" & label==1)
# tmp3 <- tmp3[,c("det001a","det002a","det002b","det002c","det002d","det003a","det004a","det004b","det004c","det004d","det005a","det006a","det006b","det006c","det006d","det007a","det008a","det008b","det008c","det008d","det009a","det010a","det011a","det011b")]
# cor(tmp, tmp2, method = "spearman")
# View(cor(tmp3, method = "spearman"))
# View(avg_det_to_det_leave_train_corr)

#### This is the code section for classifiers ####

population_files <- list.files(str_c(getwd(), "/populations/"), pattern = "2dcorr")
population_list <- list()
for (file in population_files){
  population_list[[file]] <- suppressMessages(read_csv(str_c(getwd(), "/populations/", file)))
  population_sub_list <- split(population_list[[file]], f = population_list[[file]]$month)
  population_sub_list <- lapply(population_sub_list, function(df){df[sample(nrow(df)),]}) #randomizing rows
  population_list[[file]] <- bind_rows(population_sub_list)
  population_list[[file]]$label <- as.factor(population_list[[file]]$label)
  levels(population_list[[file]]$label) <- make.names(levels(population_list[[file]]$label))
}

logit_jul_precisions <- c()
logit_aug_precisions <- c()
logit_sep_precisions <- c()
logit_jul_recalls <- c()
logit_aug_recalls <- c()
logit_sep_recalls <- c()
logit_jul_fprs <- c()
logit_aug_fprs <- c()
logit_sep_fprs <- c()

rf_jul_precisions <- c()
rf_aug_precisions <- c()
rf_sep_precisions <- c()
rf_jul_recalls <- c()
rf_aug_recalls <- c()
rf_sep_recalls <- c()
rf_jul_fprs <- c()
rf_aug_fprs <- c()
rf_sep_fprs <- c()

pop_token <- sample(names(population_list), 1)

train_data_for_jul <- population_list[[pop_token]] %>% filter(month %in% given_months)
train_data_for_aug <- population_list[[pop_token]] %>% filter(month %in% c(given_months, "2016-07"))
train_data_for_sep <- population_list[[pop_token]] %>% filter(month %in% c(given_months, "2016-07", "2016-08"))
#levels(train_data_for_jul$label) <- make.names(levels(train_data_for_jul$label)) #required for making models
#levels(train_data_for_aug$label) <- make.names(levels(train_data_for_aug$label)) #required for making models
#levels(train_data_for_sep$label) <- make.names(levels(train_data_for_sep$label)) #required for making models
#train_data_for_jul$label <- as.factor(train_data_for_jul$label)
#test_data_for_jul$label <- as.factor(test_data_for_jul$label)
#train_data_for_aug$label <- as.factor(train_data_for_aug$label)
#test_data_for_aug$label <- as.factor(test_data_for_aug$label)
#train_data_for_sep$label <- as.factor(train_data_for_sep$label)
#test_data_for_sep$label <- as.factor(test_data_for_sep$label)

# training the random forest and logistic regression model on one randomly selected population

mtry <- floor(sqrt(total_num_detectors))
tunegrid <- expand.grid(.mtry=mtry)
control <- trainControl(method = "repeatedcv", repeats = 1, classProbs = TRUE, summaryFunction = twoClassSummary, sampling = "up")
rf_jul_model_up <- train(label ~ ., data=subset(train_data_for_jul, select = -c(uid, month)), method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)
rf_aug_model_up <- train(label ~ ., data=subset(train_data_for_aug, select = -c(uid, month)), method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)
rf_sep_model_up <- train(label ~ ., data=subset(train_data_for_sep, select = -c(uid, month)), method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)
control$sampling <- "smote"
rf_jul_model_smote <- train(label ~ ., data=subset(train_data_for_jul, select = -c(uid, month)), method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)
rf_aug_model_smote <- train(label ~ ., data=subset(train_data_for_aug, select = -c(uid, month)), method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)
rf_sep_model_smote <- train(label ~ ., data=subset(train_data_for_sep, select = -c(uid, month)), method="rf", metric="ROC", tuneGrid=tunegrid, trControl=control)

control$sampling <- "up"
control$repeats <- 3
logit_jul_model_up <- train(label ~ ., data=subset(train_data_for_jul, select = -c(uid, month)), method="glm", family="binomial", metric="ROC", trControl=control)
logit_aug_model_up <- train(label ~ ., data=subset(train_data_for_aug, select = -c(uid, month)), method="glm", family="binomial", metric="ROC", trControl=control)
logit_sep_model_up <- train(label ~ ., data=subset(train_data_for_sep, select = -c(uid, month)), method="glm", family="binomial", metric="ROC", trControl=control)
control$sampling <- "smote"
logit_jul_model_smote <- train(label ~ ., data=subset(train_data_for_jul, select = -c(uid, month)), method="glm", family="binomial", metric="ROC", trControl=control)
logit_aug_model_smote <- train(label ~ ., data=subset(train_data_for_aug, select = -c(uid, month)), method="glm", family="binomial", metric="ROC", trControl=control)
logit_sep_model_smote <- train(label ~ ., data=subset(train_data_for_sep, select = -c(uid, month)), method="glm", family="binomial", metric="ROC", trControl=control)

#producing predictions for all the populations
pop_names <- names(population_list)
for (pop_name in pop_names){
  
  test_data_for_jul <- population_list[[pop_name]] %>% filter(month == "2016-07")
  test_data_for_aug <- population_list[[pop_name]] %>% filter(month == "2016-08")
  test_data_for_sep <- population_list[[pop_name]] %>% filter(month == "2016-09")
  
  # levels(test_data_for_jul$label) <- make.names(levels(test_data_for_jul$label)) #required for making models
  # levels(test_data_for_aug$label) <- make.names(levels(test_data_for_aug$label)) #required for making models
  # levels(test_data_for_sep$label) <- make.names(levels(test_data_for_sep$label)) #required for making models
  
  #predictions for random forest
  predictions <- predict(rf_jul_model_up, newdata = subset(test_data_for_jul, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_jul$label, mode = "prec_recall", positive = "X1")
  rf_jul_precision <- cfm_obj$byClass[["Precision"]]
  rf_jul_recall <- cfm_obj$byClass[["Recall"]]
  rf_jul_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_jul_precisions <- c(rf_jul_precisions, rf_jul_precision)
  rf_jul_recalls <- c(rf_jul_recalls, rf_jul_recall)
  rf_jul_fprs <- c(rf_jul_fprs, rf_jul_fpr)
  
  predictions <- predict(rf_aug_model_up, newdata = subset(test_data_for_aug, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_aug$label, mode = "prec_recall", positive = "X1")
  rf_aug_precision <- cfm_obj$byClass[["Precision"]]
  rf_aug_recall <- cfm_obj$byClass[["Recall"]]
  rf_aug_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_aug_precisions <- c(rf_aug_precisions, rf_aug_precision)
  rf_aug_recalls <- c(rf_aug_recalls, rf_aug_recall)
  rf_aug_fprs <- c(rf_aug_fprs, rf_aug_fpr)
  
  predictions <- predict(rf_sep_model_up, newdata = subset(test_data_for_sep, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_sep$label, mode = "prec_recall", positive = "X1")
  rf_sep_precision <- cfm_obj$byClass[["Precision"]]
  rf_sep_recall <- cfm_obj$byClass[["Recall"]]
  rf_sep_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_sep_precisions <- c(rf_sep_precisions, rf_sep_precision)
  rf_sep_recalls <- c(rf_sep_recalls, rf_sep_recall)
  rf_sep_fprs <- c(rf_sep_fprs, rf_sep_fpr)
  
  
  #predictions for logistic regression
  
  #predictions <- predict(logit_jul_model_up, newdata = subset(test_data_for_jul, select = -c(uid, month, label)), type="prob")
  predictions <- predict(logit_jul_model_up, newdata = subset(test_data_for_jul, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_jul$label, mode = "prec_recall", positive = "X1")
  logit_jul_precision <- cfm_obj$byClass[["Precision"]]
  logit_jul_recall <- cfm_obj$byClass[["Recall"]]
  logit_jul_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  logit_jul_precisions <- c(logit_jul_precisions, logit_jul_precision)
  logit_jul_recalls <- c(logit_jul_recalls, logit_jul_recall)
  logit_jul_fprs <- c(logit_jul_fprs, logit_jul_fpr)
  
  predictions <- predict(logit_aug_model_up, newdata = subset(test_data_for_aug, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_aug$label, mode = "prec_recall", positive = "X1")
  logit_aug_precision <- cfm_obj$byClass[["Precision"]]
  logit_aug_recall <- cfm_obj$byClass[["Recall"]]
  logit_aug_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  logit_aug_precisions <- c(logit_aug_precisions, logit_aug_precision)
  logit_aug_recalls <- c(logit_aug_recalls, logit_aug_recall)
  logit_aug_fprs <- c(logit_aug_fprs, logit_aug_fpr)
  
  predictions <- predict(logit_sep_model_up, newdata = subset(test_data_for_sep, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_sep$label, mode = "prec_recall", positive = "X1")
  logit_sep_precision <- cfm_obj$byClass[["Precision"]]
  logit_sep_recall <- cfm_obj$byClass[["Recall"]]
  logit_sep_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  logit_sep_precisions <- c(logit_sep_precisions, logit_sep_precision)
  logit_sep_recalls <- c(logit_sep_recalls, logit_sep_recall)
  logit_sep_fprs <- c(logit_sep_fprs, logit_sep_fpr)
  
  
  
  
  
  #predicitions from SMOTE based models
  
  # predictions <- predict(rf_jul_model_smote, newdata = subset(test_data_for_jul, select = -c(uid, month, label)), type = "prob")
  # predictions <- ifelse(test = predictions[,2]>0.5,1,0) 
  # levels(predictions) <- make.names(levels(predictions))
  # 
  # y <- ifelse(test_data_for_jul$label=="X1",1,0)
  # pred <- prediction(predictions, y)
  # 
  # # Recall-Precision curve             
  # RP.perf <- performance(pred, "prec", "rec")
  # 
  # plot (RP.perf)
  # 
  # f1.perf <- performance(pred, "f")
  # f1.perf@y.values
  # # ROC curve
  # ROC.perf <- performance(pred, "tpr", "fpr")
  # plot (ROC.perf)
  # 
  # # ROC area under the curve
  # auc.tmp <- performance(pred,"auc")
  # auc <- as.numeric(auc.tmp@y.values)
  
  predictions <- predict(rf_jul_model_smote, newdata = subset(test_data_for_jul, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_jul$label, mode = "prec_recall", positive = "X1")
  rf_jul_precision <- cfm_obj$byClass[["Precision"]]
  rf_jul_recall <- cfm_obj$byClass[["Recall"]]
  rf_jul_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_jul_precisions <- c(rf_jul_precisions, rf_jul_precision)
  rf_jul_recalls <- c(rf_jul_recalls, rf_jul_recall)
  rf_jul_fprs <- c(rf_jul_fprs, rf_jul_fpr)
  
  predictions <- predict(rf_aug_model_smote, newdata = subset(test_data_for_aug, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_aug$label, mode = "prec_recall", positive = "X1")
  rf_aug_precision <- cfm_obj$byClass[["Precision"]]
  rf_aug_recall <- cfm_obj$byClass[["Recall"]]
  rf_aug_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_aug_precisions <- c(rf_aug_precisions, rf_aug_precision)
  rf_aug_recalls <- c(rf_aug_recalls, rf_aug_recall)
  rf_aug_fprs <- c(rf_aug_fprs, rf_aug_fpr)
  
  predictions <- predict(rf_sep_model_smote, newdata = subset(test_data_for_sep, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_sep$label, mode = "prec_recall", positive = "X1")
  rf_sep_precision <- cfm_obj$byClass[["Precision"]]
  rf_sep_recall <- cfm_obj$byClass[["Recall"]]
  rf_sep_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  rf_sep_precisions <- c(rf_sep_precisions, rf_sep_precision)
  rf_sep_recalls <- c(rf_sep_recalls, rf_sep_recall)
  rf_sep_fprs <- c(rf_sep_fprs, rf_sep_fpr)
  
  
  #predictions for logistic regression
  
  #predictions <- predict(logit_jul_model_smote, newdata = subset(test_data_for_jul, select = -c(uid, month, label)), type="prob")
  predictions <- predict(logit_jul_model_smote, newdata = subset(test_data_for_jul, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_jul$label, mode = "prec_recall", positive = "X1")
  logit_jul_precision <- cfm_obj$byClass[["Precision"]]
  logit_jul_recall <- cfm_obj$byClass[["Recall"]]
  logit_jul_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  logit_jul_precisions <- c(logit_jul_precisions, logit_jul_precision)
  logit_jul_recalls <- c(logit_jul_recalls, logit_jul_recall)
  logit_jul_fprs <- c(logit_jul_fprs, logit_jul_fpr)
  
  predictions <- predict(logit_aug_model_smote, newdata = subset(test_data_for_aug, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_aug$label, mode = "prec_recall", positive = "X1")
  logit_aug_precision <- cfm_obj$byClass[["Precision"]]
  logit_aug_recall <- cfm_obj$byClass[["Recall"]]
  logit_aug_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  logit_aug_precisions <- c(logit_aug_precisions, logit_aug_precision)
  logit_aug_recalls <- c(logit_aug_recalls, logit_aug_recall)
  logit_aug_fprs <- c(logit_aug_fprs, logit_aug_fpr)
  
  predictions <- predict(logit_sep_model_smote, newdata = subset(test_data_for_sep, select = -c(uid, month, label)))
  cfm_obj <- confusionMatrix(data = predictions, reference = test_data_for_sep$label, mode = "prec_recall", positive = "X1")
  logit_sep_precision <- cfm_obj$byClass[["Precision"]]
  logit_sep_recall <- cfm_obj$byClass[["Recall"]]
  logit_sep_fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])
  logit_sep_precisions <- c(logit_sep_precisions, logit_sep_precision)
  logit_sep_recalls <- c(logit_sep_recalls, logit_sep_recall)
  logit_sep_fprs <- c(logit_sep_fprs, logit_sep_fpr)
  
}

answers_df <- cbind(logit_jul_precisions, logit_jul_recalls, logit_jul_fprs, logit_aug_precisions, logit_aug_recalls, logit_aug_fprs, logit_sep_precisions, logit_sep_recalls, logit_sep_fprs,
                    rf_jul_precisions, rf_jul_recalls, rf_jul_fprs, rf_aug_precisions, rf_aug_recalls, rf_aug_fprs, rf_sep_precisions, rf_sep_recalls, rf_sep_fprs)

up_answers_df <- answers_df[1:length(pop_names),] %>% as.data.frame
smote_answers_df <- answers_df[(1+length(pop_names)):nrow(answers_df),] %>% as.data.frame
getwd()
write_csv(up_answers_df, "up_answers_01-08.csv")
write_csv(smote_answers_df, "smote_answers_01-08.csv")

