library(tidyverse)
library(copula)
library(BMS)



getwd()
setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Fifth Quarter/RCP14/Dev/RCP14Data (7-21-2017)/Case1/TrainingData")
week_names = list.files()


train_org_df_list = list()
for (week in week_names){
  
  org_num = list.files(str_c(getwd(),"/",week))
  
  for (i in 1:length(org_num)){
    train_org_df_list[[str_c("Org_",i,"_",week)]] = read_csv(str_c(getwd(),"/",week,"/", org_num[i]), col_types = cols(X68 = col_skip())) #read in training orgs week by week
  }
  
}




setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Fifth Quarter/RCP14/Dev/RCP14Data (7-21-2017)/Case1/TestingData")
week_names = list.files()

test_org_df_list = list()
for (week in week_names){
  
  org_num = list.files(str_c(getwd(),"/",week))
  
  for (i in 1:length(org_num)){
    test_org_df_list[[str_c("Org_",i,"_",week)]] = read_csv(str_c(getwd(),"/",week,"/", org_num[i]), col_types = cols(X68 = col_skip())) #read in testing orgs week by week
  }
  
}



#Here we will generate 3097 vectors (with index i) of length 65 (the number of detectors) (with index j).
#For each entry i, get the average value across weeks; take the ith row of each csv file and the column j corresponding to the detector in question to get the values for average. 
#This average will be the jth value of the ith vector. 


detector_names = colnames(train_org_df_list[["Org_1_Week_005"]])[-(1:2)] #pick names from any of the dataframe

for (org in 1:1){ #loop for all organizations
  
  user_det_means_by_week = vector(mode = "list", length = 3097)
  
  for (user in 1:3097){ #loop for each user in an organization
    
    det_means_by_week = vector(mode = "numeric", length = 65)
    det_cnt = 0
    for (det in detector_names){ #loop for each detector of each user in each organization
      
      det_cnt = det_cnt+1
      
      weekly_det_vals = vector(mode = "numeric", length = 29)
      w_cnt = 0
      for (week in str_pad(34:49, pad = 0, width = 3 , "left")){ #loop for each week for the above detector of that user in that organization
        
      w_cnt = w_cnt+1
      weekly_det_vals[w_cnt] = as.numeric(test_org_df_list[[str_c("Org_",org,"_Week_",week)]][[user,det]])
      
      }
      
      det_means_by_week[det_cnt] = mean(weekly_det_vals)
      
    }
    
    user_det_means_by_week[[str_c("Org_",org,"User_",user)]] = det_means_by_week
    
  }
  
}

user_det_means_df = plyr::ldply(user_det_means_by_week)

colnames(user_det_means_df) = c("id", detector_names)



#copula part

correlation_false=plyr::ldply(correlations.for.det059a[[week]][3],data.frame)

correlation_false=correlation_false[,2]

correlation_false=as.vector(correlation_false)

correlation_false=as.numeric(correlation_false)

correlation_false=correlation_false[!is.na(correlation_false)]

correlation_false=vec2symMat(correlation_false,diag=FALSE)

r=correlation_false


pooled_corr <- read_csv("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Fifth Quarter/RCP14/Dev/Pooled Correlation.csv", 
                        col_types = cols(X1 = col_skip()))

n = 3097

## Functions
gen.gauss.cop <- function(r, n){
  rho <- 2 * sin(r * pi/6)        # Pearson correlation
  P <- r        # Correlation matrix
  d <- nrow(P)                    # Dimension
  ## Generate sample
  U <- pnorm(matrix(rnorm(n*d), ncol = d) %*% chol(P))
  return(U)
}

user_det_means_mat = user_det_means_df[,-c(1,64,65)]

U <- gen.gauss.cop(r = pooled_corr, n = n)

correlated_vals_vect_list = vector(mode = "list", length = 63)
for (i in 1:63){
  
  correlated_vals_vect_list[[i]] = as.vector(BMS::quantile.density(density(user_det_means_mat[,i]), U[,i]))
  
}

correlated_detector_vals_df = t(plyr::ldply(correlated_vals_vect_list))

# cor(correlated_detector_vals_df[,1], user_det_means_mat[,1])
# 
# tmp_df = as.data.frame(train_org_df_list[["Org_1_Week_020"]])
# 
# tmp_df = tmp_df[,-c(1,2)]
# 
# View(cor(tmp_df, method = "spearman"))
# 
# tmp2_df = as.data.frame(train_org_df_list[["Org_1_Week_020"]])
# 
# tmp2_df = tmp2_df[,-c(1,2)]
# 
# tmp2_cor = cor(cbind(tmp_df[,1],tmp2_df[,1]), method = "spearman")
# 
# 
# corr_avg_df = cor(user_det_means_df[,-1])
# 
# diff_corr = pooled_corr - corr_avg_df

# decomp = chol(pooled_corr)
# 
# transformed_avg_df = as.matrix(user_det_means_df[,-c(1,64,65)]) %*% decomp
# 
# corr_transformed_avg_df = cor(transformed_avg_df)

