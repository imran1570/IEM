#==============================================================================
#title           :	RCP17_data_correlator.R
#description     :	Script to use i.i.d detector samples generated from RCP17_hist_sampler.R and correlate them using spearman rank correlation and using copulas
#author          :	Muhammad Imran
#date            :	12 DEC 2017
#notes           :	N/A 
#R_version       :	Microsoft, R Open, R-3.4.2 (64-bit)
#==============================================================================


#setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Seventh Quarter/RCP17") #this line is for the sake of R studio

library(stringr)
library(readr)
library(dplyr)
library(Matrix) # isSymmetric
library(matrixcalc) # is.positive.definite
library(copula)
options(scipen = 100)
source("helper_functions.R")

all_detectors <- c("001a","002a","002b","002c","002d","003a","004a","004b","004c","004d","005a","006a","006b","006c","006d","007a","008a","008b","008c","008d","009a","010a","011a","011b")
continuous_detectors <-c("001a","002a","002b","002c","002d","003a","004a","004b","004c","004d","005a","006a","006b","006c","006d","007a","008a","008b","008c","008d")
discrete_detectors <- c("009a","010a","011a","011b")
given_months <- c("2015-11", "2015-12", "2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06")
test_months <- c("2016-07", "2016-08", "2016-09")
reqd_column_names <- c("uid", "month", "label", "det001a","det002a","det002b","det002c","det002d","det003a","det004a","det004b","det004c","det004d","det005a","det006a","det006b","det006c","det006d","det007a","det008a","det008b","det008c","det008d","det009a","det010a","det011a","det011b")
total_num_detectors <- length(continuous_detectors)+length(discrete_detectors)

raw_corr_file_names <- list.files(path = str_c(getwd(), "/RCP17data/Detector Spearman Correlations")) #get the correlation file names in the data folder
corr_months <- str_split(raw_corr_file_names, "_") %>% sapply(function (x) x[1]) %>% unique #extracting month names from file names

assumption_one_months <- c("2015-11", "2015-12", "2016-01") #july august september are similar to november decemeber january
assumption_two_months <- c("2016-04", "2016-05", "2016-06") #future three months are similar to past three months
assumption_three_months <- c("2016-03", "2016-03", "2016-05") #labor stats posted by freedman on basecamp

#assumption_token <- read_csv() #carry on the token from training hist sampler

### code block to correlate training data ###

det_to_det_train_corr_leave_list <- list()
det_to_det_train_corr_noleave_list <- list()
#loop for reading in the correlation files of leave and no leave group and making positive definite correlation matrices of them
for (month in corr_months){
  corr_vect <- suppressMessages(read_csv(str_c(getwd(), "/RCP17data/Detector Spearman Correlations/", month, "_Correlations_Leave.csv")))$`Spearman Correlation` %>% as.numeric
  det_to_det_corr <- vec2symmat(invec = corr_vect)
  det_to_det_corr <- round(det_to_det_corr, 10)
  det_to_det_corr[is.na(det_to_det_corr)] <- 0
  print(isSymmetric(det_to_det_corr))
  print(is.positive.definite(det_to_det_corr))
  if (is.positive.definite(det_to_det_corr) == FALSE){
  det_to_det_corr <- nearPD(det_to_det_corr) %>% .$mat %>% as.matrix
  diag(det_to_det_corr) <- 1
  }
  print(is.positive.definite(det_to_det_corr))
  det_to_det_train_corr_leave_list[[month]] <- det_to_det_corr
  
  
  corr_vect <- suppressMessages(read_csv(str_c(getwd(), "/RCP17data/Detector Spearman Correlations/", month, "_Correlations_NoLeave.csv")))$`Spearman Correlation` %>% as.numeric
  det_to_det_corr <- vec2symmat(invec = corr_vect)
  det_to_det_corr <- round(det_to_det_corr, 10)
  det_to_det_corr[is.na(det_to_det_corr)] <- 0
  print(isSymmetric(det_to_det_corr))
  print(is.positive.definite(det_to_det_corr))
  if (is.positive.definite(det_to_det_corr) == FALSE){
    det_to_det_corr <- nearPD(det_to_det_corr) %>% .$mat %>% as.matrix
    diag(det_to_det_corr) <- 1
  }
  print(is.positive.definite(det_to_det_corr))
  det_to_det_train_corr_noleave_list[[month]] <- det_to_det_corr
  
} # end of correlations loop



### loop to correlate the leave training data ###

copula_train_leave_samples_list <- list()
correlated_train_leave_list <- list()
copula_train_noleave_samples_list <- list()
correlated_train_noleave_list <- list()
correlated_train_list <- list()
for (month in given_months){
  #copula samples generating for leave group
  leave_norm_cop_obj <- normalCopula(param = P2p(as.data.frame(det_to_det_train_corr_leave_list[[month]])), dispstr = "un", dim = nrow(as.data.frame(det_to_det_train_corr_leave_list[[month]]))) # create gaussian copula oject
  copula_train_leave_samples_list[[month]] <- data.frame(rCopula(copula = leave_norm_cop_obj , n = nrow(as.data.frame(list_of_all_detector_samples_leave_train[[month]])))) #sample from copula object
  #copula samples generating for noleave group
  noleave_norm_cop_obj <- normalCopula(param = P2p(as.data.frame(det_to_det_train_corr_noleave_list[[month]])), dispstr = "un", dim = nrow(as.data.frame(det_to_det_train_corr_noleave_list[[month]]))) # create gaussian copula oject
  copula_train_noleave_samples_list[[month]] <- data.frame(rCopula(copula = noleave_norm_cop_obj , n = nrow(as.data.frame(list_of_all_detector_samples_noleave_train[[month]])))) #sample from copula object
  
  #correlating the leave group data
  correlated_train_leave_list[[month]] <- CorrelateData(DetectorData = as.data.frame(list_of_all_detector_samples_leave_train[[month]]), CopulaMatrix = as.data.frame(copula_train_leave_samples_list[[month]]))
  correlated_train_leave_list[[month]]["month"] <- rep(month, nrow(correlated_train_leave_list[[month]])) #adding month column
  correlated_train_leave_list[[month]]["label"] <- rep(1, nrow(correlated_train_leave_list[[month]]))
  correlated_train_noleave_list[[month]] <- CorrelateData(DetectorData = as.data.frame(list_of_all_detector_samples_noleave_train[[month]]), CopulaMatrix = as.data.frame(copula_train_noleave_samples_list[[month]]))
  correlated_train_noleave_list[[month]]["month"] <- rep(month, nrow(correlated_train_noleave_list[[month]])) #adding month column
  correlated_train_noleave_list[[month]]["label"] <- rep(0, nrow(correlated_train_noleave_list[[month]]))
  correlated_train_list[[month]] <- bind_rows(correlated_train_noleave_list[[month]], correlated_train_leave_list[[month]])
}

### end of loop to correlate the training data ###






#### correlating the test period data according to the assumption ####

if (assumption_token == 1){
  
  det_to_det_test_corr_leave_list <-  det_to_det_train_corr_leave_list[names(det_to_det_train_corr_leave_list) %in% assumption_one_months]
  det_to_det_test_corr_noleave_list <- det_to_det_train_corr_noleave_list[names(det_to_det_train_corr_noleave_list) %in% assumption_one_months]
  ### loop to correlate the leave training data ###
  copula_test_leave_samples_list <- list()
  correlated_test_leave_list <- list()
  copula_test_noleave_samples_list <- list()
  correlated_test_noleave_list <- list()
  correlated_test_list <- list()
  counter <- 0
  for (month in assumption_one_months){
    counter <- counter+1
    #copula samples generating for leave group
    leave_norm_cop_obj <- normalCopula(param = P2p(as.data.frame(det_to_det_test_corr_leave_list[[month]])), dispstr = "un", dim = nrow(as.data.frame(det_to_det_test_corr_leave_list[[month]]))) # create gaussian copula oject
    copula_test_leave_samples_list[[test_months[counter]]] <- data.frame(rCopula(copula = leave_norm_cop_obj , n = nrow(as.data.frame(list_of_all_detector_samples_leave_train[[month]])))) #sample from copula object
    #copula samples generating for noleave group
    noleave_norm_cop_obj <- normalCopula(param = P2p(as.data.frame(det_to_det_test_corr_noleave_list[[month]])), dispstr = "un", dim = nrow(as.data.frame(det_to_det_test_corr_noleave_list[[month]]))) # create gaussian copula oject
    copula_test_noleave_samples_list[[test_months[counter]]] <- data.frame(rCopula(copula = noleave_norm_cop_obj , n = nrow(as.data.frame(list_of_all_detector_samples_noleave_train[[month]])))) #sample from copula object
    #correlating the leave group data
    correlated_test_leave_list[[test_months[counter]]] <- CorrelateData(DetectorData = as.data.frame(list_of_all_detector_samples_leave_test[counter]), CopulaMatrix = as.data.frame(copula_test_leave_samples_list[[test_months[counter]]]))
    correlated_test_leave_list[[test_months[counter]]]["month"] <- rep(test_months[counter], nrow(correlated_test_leave_list[[test_months[counter]]])) #adding month column
    correlated_test_leave_list[[test_months[counter]]]["label"] <- rep(1, nrow(correlated_test_leave_list[[test_months[counter]]]))
    correlated_test_noleave_list[[test_months[counter]]] <- CorrelateData(DetectorData = as.data.frame(list_of_all_detector_samples_noleave_test[counter]), CopulaMatrix = as.data.frame(copula_test_noleave_samples_list[[test_months[counter]]]))
    correlated_test_noleave_list[[test_months[counter]]]["month"] <- rep(test_months[counter], nrow(correlated_test_noleave_list[[test_months[counter]]])) #adding month column
    correlated_test_noleave_list[[test_months[counter]]]["label"] <- rep(0, nrow(correlated_test_noleave_list[[test_months[counter]]]))
    correlated_test_list[[test_months[counter]]] <- bind_rows(correlated_test_noleave_list[[test_months[counter]]], correlated_test_leave_list[[test_months[counter]]])
    
    } #end of FOR
  } else if (assumption_token == 2){
    
    det_to_det_test_corr_leave_list <-  det_to_det_train_corr_leave_list[names(det_to_det_train_corr_leave_list) %in% assumption_two_months]
    det_to_det_test_corr_noleave_list <- det_to_det_train_corr_noleave_list[names(det_to_det_train_corr_noleave_list) %in% assumption_two_months]
    ### loop to correlate the leave training data ###
    copula_test_leave_samples_list <- list()
    correlated_test_leave_list <- list()
    copula_test_noleave_samples_list <- list()
    correlated_test_noleave_list <- list()
    counter <- 0
    for (month in assumption_two_months){
      counter <- counter+1
      #copula samples generating for leave group
      leave_norm_cop_obj <- normalCopula(param = P2p(as.data.frame(det_to_det_test_corr_leave_list[[month]])), dispstr = "un", dim = nrow(as.data.frame(det_to_det_test_corr_leave_list[[month]]))) # create gaussian copula oject
      copula_test_leave_samples_list[[test_months[counter]]] <- data.frame(rCopula(copula = leave_norm_cop_obj , n = nrow(as.data.frame(list_of_all_detector_samples_leave_train[[month]])))) #sample from copula object
      #copula samples generating for noleave group
      noleave_norm_cop_obj <- normalCopula(param = P2p(as.data.frame(det_to_det_test_corr_noleave_list[[month]])), dispstr = "un", dim = nrow(as.data.frame(det_to_det_test_corr_noleave_list[[month]]))) # create gaussian copula oject
      copula_test_noleave_samples_list[[test_months[counter]]] <- data.frame(rCopula(copula = noleave_norm_cop_obj , n = nrow(as.data.frame(list_of_all_detector_samples_noleave_train[[month]])))) #sample from copula object
      #correlating the leave group data
      correlated_test_leave_list[[test_months[counter]]] <- CorrelateData(DetectorData = as.data.frame(list_of_all_detector_samples_leave_test[counter]), CopulaMatrix = as.data.frame(copula_test_leave_samples_list[[test_months[counter]]]))
      correlated_test_leave_list[[test_months[counter]]]["month"] <- rep(test_months[counter], nrow(correlated_test_leave_list[[test_months[counter]]])) #adding month column
      correlated_test_leave_list[[test_months[counter]]]["label"] <- rep(1, nrow(correlated_test_leave_list[[test_months[counter]]]))
      correlated_test_noleave_list[[test_months[counter]]] <- CorrelateData(DetectorData = as.data.frame(list_of_all_detector_samples_noleave_test[counter]), CopulaMatrix = as.data.frame(copula_test_noleave_samples_list[[test_months[counter]]]))
      correlated_test_noleave_list[[test_months[counter]]]["month"] <- rep(test_months[counter], nrow(correlated_test_noleave_list[[test_months[counter]]])) #adding month column
      correlated_test_noleave_list[[test_months[counter]]]["label"] <- rep(0, nrow(correlated_test_noleave_list[[test_months[counter]]]))
      correlated_test_list[[test_months[counter]]] <- bind_rows(correlated_test_noleave_list[[test_months[counter]]], correlated_test_leave_list[[test_months[counter]]])
      
    } #end of FOR
    
    } else {
      
      det_to_det_test_corr_leave_list <-  det_to_det_train_corr_leave_list[names(det_to_det_train_corr_leave_list) %in% assumption_three_months]
      det_to_det_test_corr_noleave_list <- det_to_det_train_corr_noleave_list[names(det_to_det_train_corr_noleave_list) %in% assumption_three_months]
      ### loop to correlate the leave training data ###
      copula_test_leave_samples_list <- list()
      correlated_test_leave_list <- list()
      copula_test_noleave_samples_list <- list()
      correlated_test_noleave_list <- list()
      counter <- 0
      for (month in assumption_three_months){
        counter <- counter+1
        #copula samples generating for leave group
        leave_norm_cop_obj <- normalCopula(param = P2p(as.data.frame(det_to_det_test_corr_leave_list[[month]])), dispstr = "un", dim = nrow(as.data.frame(det_to_det_test_corr_leave_list[[month]]))) # create gaussian copula oject
        copula_test_leave_samples_list[[test_months[counter]]] <- data.frame(rCopula(copula = leave_norm_cop_obj , n = nrow(as.data.frame(list_of_all_detector_samples_leave_train[[month]])))) #sample from copula object
        #copula samples generating for noleave group
        noleave_norm_cop_obj <- normalCopula(param = P2p(as.data.frame(det_to_det_test_corr_noleave_list[[month]])), dispstr = "un", dim = nrow(as.data.frame(det_to_det_test_corr_noleave_list[[month]]))) # create gaussian copula oject
        copula_test_noleave_samples_list[[test_months[counter]]] <- data.frame(rCopula(copula = noleave_norm_cop_obj , n = nrow(as.data.frame(list_of_all_detector_samples_noleave_train[[month]])))) #sample from copula object
        #correlating the leave group data
        correlated_test_leave_list[[test_months[counter]]] <- CorrelateData(DetectorData = as.data.frame(list_of_all_detector_samples_leave_test[counter]), CopulaMatrix = as.data.frame(copula_test_leave_samples_list[[test_months[counter]]]))
        correlated_test_leave_list[[test_months[counter]]]["month"] <- rep(test_months[counter], nrow(correlated_test_leave_list[[test_months[counter]]])) #adding month column
        correlated_test_leave_list[[test_months[counter]]]["label"] <- rep(1, nrow(correlated_test_leave_list[[test_months[counter]]]))
        correlated_test_noleave_list[[test_months[counter]]] <- CorrelateData(DetectorData = as.data.frame(list_of_all_detector_samples_noleave_test[counter]), CopulaMatrix = as.data.frame(copula_test_noleave_samples_list[[test_months[counter]]]))
        correlated_test_noleave_list[[test_months[counter]]]["month"] <- rep(test_months[counter], nrow(correlated_test_noleave_list[[test_months[counter]]])) #adding month column
        correlated_test_noleave_list[[test_months[counter]]]["label"] <- rep(0, nrow(correlated_test_noleave_list[[test_months[counter]]]))
        correlated_test_list[[test_months[counter]]] <- bind_rows(correlated_test_noleave_list[[test_months[counter]]], correlated_test_leave_list[[test_months[counter]]])
        
      } #end of FOR
  
} #end of ELSE


correlated_train_list <- lapply(correlated_train_list, function(df){cbind(df, uid = 1:nrow(df))}) #assigning user ids
correlated_test_list <- lapply(correlated_test_list, function(df){cbind(df, uid = 1:nrow(df))}) #assigning user ids
correlated_train_list <- lapply(correlated_train_list, function(df){df[sample(nrow(df)),]}) #randomizing rows
correlated_test_list <- lapply(correlated_test_list, function(df){df[sample(nrow(df)),]}) #randomizing rows

all_months_pop <- bind_rows(correlated_train_list, correlated_test_list)
colnames(all_months_pop)[1:total_num_detectors] <- c("det001a","det002a","det002b","det002c","det002d","det003a","det004a","det004b","det004c","det004d","det005a","det006a","det006b","det006c","det006d","det007a","det008a","det008b","det008c","det008d","det009a","det010a","det011a","det011b")
all_months_pop <- all_months_pop[reqd_column_names]

write_csv(all_months_pop, str_c(getwd(), "/populations/all_months_pop_",org_num,".csv"))









time_corr_mat <- matrix(c(1, 0.462107132, 0.462107132, 1), nrow = 2, ncol = 2, byrow = T)
L_t <- chol(time_corr_mat)
u_norm1 <- rnorm(n=length(tmp1))
u_norm2 <- rnorm(n=length(tmp2))
u_norm <- cbind(u_norm1, u_norm2)
U <- u_norm %*% L_t

det_corr_mat <- matrix(c(1, 0.462107132, 0.462107132, 1), nrow = 2, ncol = 2, byrow = T)
L_t <- chol(time_corr_mat)

tmp1 <- filter(all_months_pop, month == "2015-11") %>% .$det001a
tmp2 <- filter(all_months_pop, month == "2015-12") %>% .$det001a
tmp3 <- cbind(tmp1, tmp2)
tmp <- filter(all_months_pop, month == "2015-11" & label==0) %>% subset(select = -c(uid,label,month))
View(cor(tmp3, method = "spearman"))
