library(readr)
library(dplyr)
setwd("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Seventh Quarter/RCP15") # set this path where your newpop.csv files are located following code will read in all the files automatically

files_list <- list.files()

files_df_list <- list()

for (f in files_list){
  
  files_df_list[[f]] <- suppressMessages(read_csv(f))
  
}

full_df <- bind_rows(files_df_list)
full_df$UserID <- NULL
colnames(full_df)[1] <- c("user_identifier")
colnames(full_df)[2] <- c("Week")
colnames(full_df)[3] <- c("label")
new_col_names <- gsub("X", "det", colnames(full_df))
colnames(full_df) <- new_col_names

write_csv(full_df, "C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Seventh Quarter/RCP15/population001.csv") # set this path where you want your end product to be placed
