#==============================================================================
#title           :	RCP17_hist_sampler.R
#description     :	Script to import the histogram files and sample the histograms to produce marginal un-correlated distributions for detectors
#author          :	Muhammad Imran
#date            :	12 DEC 2017
#notes           :	N/A 
#R_version       :	Microsoft, R Open, R-3.4.2 (64-bit)
#==============================================================================


#setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Seventh Quarter/RCP17") #this line is for the sake of R studio

library(stringr)
library(readr)
library(dplyr)
library(triangle)
options(scipen = 100)

all_detectors <- c("001a","002a","002b","002c","002d","003a","004a","004b","004c","004d","005a","006a","006b","006c","006d","007a","008a","008b","008c","008d","009a","010a","011a","011b")
continuous_detectors <-c("001a","002a","002b","002c","002d","003a","004a","004b","004c","004d","005a","006a","006b","006c","006d","007a","008a","008b","008c","008d")
discrete_detectors <- c("009a","010a","011a","011b")
given_months <- c("2015-11", "2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06")
test_months <- c("2016-07", "2016-08", "2016-09")
total_num_detectors <- length(continuous_detectors)+length(discrete_detectors)

# leave_hist_files <- list.files(path = str_c(getwd(), "/RCP17data"), pattern = "[_]det\\d{3}[a-z][_]Leave") # get the names of histogram file names in the data folder
# noleave_hist_files <- list.files(path = str_c(getwd(), "/RCP17data"), pattern = "[_]det\\d{3}[a-z][_]NoLeave")




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
    continuous_hist_list_leave[[file_name_leave]] <- suppressMessages(read_csv(str_c(getwd(), "/RCP17Data/", file_name_leave))) #load histograms as into list of dataframes
    continuous_hist_list_noleave[[file_name_noleave]] <- suppressMessages(read_csv(str_c(getwd(), "/RCP17Data/", file_name_noleave))) #load histograms as into list of dataframes
    continuous_hist_leave <- continuous_hist_list_leave[[file_name_leave]] #extract histogram from list at a time for usage
    continuous_hist_noleave <- continuous_hist_list_noleave[[file_name_noleave]]
    #for leave group
    unbinned_data_vect <- c()
    count_zeros <- continuous_hist_leave$counts[continuous_hist_leave$lower==0 & continuous_hist_leave$upper==0] #count the zeros in the zero bin
    unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
    det_hist_no_zeros <- continuous_hist_leave[(continuous_hist_leave$lower!=0 & continuous_hist_leave$upper!=0) & (continuous_hist_leave$counts!=0),] #filter out the bins with no data points
    if (nrow(det_hist_no_zeros)>0){
    det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros # filter out the count of zeros from the bins which contain zero data points in them
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
    det_hist_no_zeros <- continuous_hist_noleave[(continuous_hist_noleave$lower!=0 & continuous_hist_noleave$upper!=0) & (continuous_hist_noleave$counts!=0),]
    if (nrow(det_hist_no_zeros)>0){
    det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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
    discrete_hist_list_leave[[file_name_leave]] <- suppressMessages(read_csv(str_c(getwd(), "/RCP17Data/", file_name_leave)))
    discrete_hist_list_noleave[[file_name_noleave]] <- suppressMessages(read_csv(str_c(getwd(), "/RCP17Data/", file_name_noleave)))
    discrete_hist_leave <- discrete_hist_list_leave[[file_name_leave]]
    discrete_hist_noleave <- discrete_hist_list_noleave[[file_name_noleave]]
    #for leave group
    unbinned_data_vect <- c()
    count_zeros <- discrete_hist_leave$counts[discrete_hist_leave$lower==0 & discrete_hist_leave$upper==0]
    unbinned_data_vect <- c(unbinned_data_vect, rep(0, count_zeros))
    det_hist_no_zeros <- discrete_hist_leave[(discrete_hist_leave$lower!=0 & discrete_hist_leave$upper!=0) & (discrete_hist_leave$counts!=0),]
    if (nrow(det_hist_no_zeros)>0){
    det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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
    det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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

}

}#end of org loop






### following code block is to generate test period data ###


assumption_one_months <- c("2015-11", "2015-12", "2016-01") #july august september are similar to november decemeber january
assumption_two_months <- c("2016-04", "2016-05", "2016-06") #future three months are similar to past three months
assumption_three_months <- c("2016-03", "2016-03", "2016-05") #labor stats posted by freedman on basecamp

test_months_assumption <- c(1,2,3)
#assumption_token <- sample(test_months_assumption, 1)

for(org in start_org:end_org){

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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros # filter out the count of zeros from the bins which contain zero data points in them
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros # filter out the count of zeros from the bins which contain zero data points in them
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros # filter out the count of zeros from the bins which contain zero data points in them
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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
        det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] <- det_hist_no_zeros$counts[(det_hist_no_zeros$lower < 0) & (det_hist_no_zeros$upper > 0)] - count_zeros 
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


} # end of org loop