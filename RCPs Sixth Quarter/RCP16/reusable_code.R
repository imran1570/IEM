# summarize results
#print(nb_search_model)

#validation_cfm_obj <- confusionMatrix(nb_search_model, mode = "prec_recall", positive = "X1", norm = "none")

#confusionMatrix(nb_search_model, mode = "prec_recall", positive = "X1")

#validation_precision <- validation_cfm_obj$table[2,2] / (validation_cfm_obj$table[2,2] + validation_cfm_obj$table[2,1])

#validation_recall <- validation_cfm_obj$table[2,2] / (validation_cfm_obj$table[2,2] + validation_cfm_obj$table[1,2])

#validation_fpr <- validation_cfm_obj$table[2,1] / (validation_cfm_obj$table[2,1] + validation_cfm_obj$table[1,1])



# print(rf_search_model)
# 
# cfm_obj <- confusionMatrix(rf_search_model, mode = "prec_recall", positive = "1", norm = "none")
# 
# confusionMatrix(nb_exfil_model, mode = "prec_recall", positive = "X1")
# 
# precision <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[2,1])
# 
# recall <- cfm_obj$table[2,2] / (cfm_obj$table[2,2] + cfm_obj$table[1,2])
# 
# fpr <- cfm_obj$table[2,1] / (cfm_obj$table[2,1] + cfm_obj$table[1,1])

non_window_user_ids <- features %>% filter(Windows_User == 0) %>% select(staff_id) %>% unique

window_user_ids <- features %>% filter(Windows_User == 1) %>% select(staff_id) %>% unique

View(filter(gaussian_all_sampled_dist_df_org_1, staff_id %in% window_user_ids$staff_id))

View(filter(triangular_all_sampled_dist_df_org_1, staff_id %in% window_user_ids$staff_id))

View(filter(gaussian_all_sampled_dist_df_org_1, staff_id %in% non_window_user_ids$staff_id))

View(filter(triangular_all_sampled_dist_df_org_1, staff_id %in% non_window_user_ids$staff_id))



Full_Features_with_Group_ID <- inner_join(Full_Features, orgchart, by = c("staff_id" = "StaffId"))

Full_Features_with_Group_ID <- Full_Features_with_Group_ID[c("GroupId", colnames(Full_Features))]

colnames(Full_Features_with_Group_ID)[1] <- c("group_id")


write_csv(Full_Features_with_Group_ID,"./input_dir/Full_Features_with_Group_ID.csv")


colMeans(subset(features_dists_df, select = -c(staff_id))) / colMeans(subset(gauss_norm_six_week_data_df_org_2, select = -c(staff_id, day)))


apply(subset(features_dists_df, select = -c(staff_id)), 2, sd) / apply(subset(gauss_norm_six_week_data_df_org_2, select = -c(staff_id, day)), 2, sd)


colMeans(subset(features_dists_df, select = -c(staff_id))) / colMeans(subset(tri_norm_six_week_data_df_org_2, select = -c(staff_id, day)))


apply(subset(features_dists_df, select = -c(staff_id)), 2, sd) / apply(subset(tri_norm_six_week_data_df_org_2, select = -c(staff_id, day)), 2, sd)


plot(subset(norm_cop_samples_df_org_2, select = -c(staff_id))[,1:2])

plot(subset(t_cop_samples_df_org_3, select = -c(staff_id))[,1:2])



exfil_non_window <- Full_Features %>% filter(Exfil_Threat == 1 & Windows_User == 0) %>% select(staff_id)
length(exfil_non_window$staff_id)


plot(Full_Features$USB_Transfers, Full_Features$Exfil_Threat)

plot(Full_Features$Avg_USB_Transfer, Full_Features$Exfil_Threat)

plot(Full_Features$Total_USB_Transfer, Full_Features$Exfil_Threat)


plot(Full_Features$USB_Transfers, Full_Features$Windows_User)

plot(Full_Features$Avg_USB_Transfer, Full_Features$Windows_User)

plot(Full_Features$Total_USB_Transfer, Full_Features$Windows_User)


gauss_norm_estimated_data_df <- filter(gauss_norm_six_week_data_df_org_1, day %in% c(31:40))

gauss_norm_estimated_data_df$Exfil_Threat <- Full_Features$Exfil_Threat

gauss_norm_estimated_data_df$Windows_User <- Full_Features$Windows_User


plot(gauss_norm_estimated_data_df$USB_Transfers, gauss_norm_estimated_data_df$Exfil_Threat)

plot(gauss_norm_estimated_data_df$Avg_USB_Transfer, gauss_norm_estimated_data_df$Exfil_Threat)

plot(gauss_norm_estimated_data_df$Total_USB_Transfer, gauss_norm_estimated_data_df$Exfil_Threat)


plot(gauss_norm_estimated_data_df$USB_Transfers, gauss_norm_estimated_data_df$Windows_User)

plot(gauss_norm_estimated_data_df$Avg_USB_Transfer, gauss_norm_estimated_data_df$Windows_User)

plot(gauss_norm_estimated_data_df$Total_USB_Transfer, gauss_norm_estimated_data_df$Windows_User)



colMeans(subset(features_dists_df, select = -c(staff_id))) / colMeans(subset(gaussian_all_sampled_dist_df_org_1, select = -c(staff_id, day)))


apply(subset(features_dists_df, select = -c(staff_id)), 2, sd) / apply(subset(gaussian_all_sampled_dist_df_org_1, select = -c(staff_id, day)), 2, sd)


colMeans(subset(features_dists_df, select = -c(staff_id))) / colMeans(subset(tri_norm_six_week_data_df_org_2, select = -c(staff_id, day)))


apply(subset(features_dists_df, select = -c(staff_id)), 2, sd) / apply(subset(tri_norm_six_week_data_df_org_2, select = -c(staff_id, day)), 2, sd)

cor(subset(features_dists_df, select = -c(staff_id))) - cor()


given_df <- filter(tri_norm_six_week_data_df_org_1, day %in% 31:40)

estimated_df <- filter(tri_norm_six_week_data_df_org_1, day %in% 51:60)

View(cor(subset(features_dists_df, select = -c(staff_id))) - cor(subset(estimated_df, select = -c(staff_id, day))))


colMeans(subset(features_dists_df, select = -c(staff_id))) / colMeans(subset(estimated_df, select = -c(staff_id, day)))


apply(subset(features_dists_df, select = -c(staff_id)), 2, sd) / apply(subset(estimated_df, select = -c(staff_id, day)), 2, sd)





