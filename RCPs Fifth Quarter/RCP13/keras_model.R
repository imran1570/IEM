devtools::install_github("rstudio/keras")
library(keras)
install_tensorflow()

library(tidyverse)
library(MLmetrics)
library (ROCR)


# function for F1-score calculation
calc_f1_score <- function(threshold, mse_output, true_values){
  return(F1_Score(true_values, as.integer(mse_output > threshold)))
}


# program works in this loop

for(org_number in seq(20, 39)){
  T_y <- read_csv(stringr::str_c("T_y_", org_number, ".csv"), col_names = FALSE)
  if(sum(T_y) >= 45){

Z12_x <- read_csv(stringr::str_c("Z12_x_", org_number, ".csv"), col_names = FALSE)
Z12_y <- read_csv(stringr::str_c("Z12_y_", org_number, ".csv"), col_names = FALSE)
Z3_x <- read_csv(stringr::str_c("Z3_x_", org_number, ".csv"), col_names = FALSE)
Z3_y <- read_csv(stringr::str_c("Z3_y_", org_number, ".csv"), col_names = FALSE)
Ones_x <- read_csv(stringr::str_c("Ones_x_", org_number, ".csv"), col_names = FALSE)
Ones_y <- read_csv(stringr::str_c("Ones_y_", org_number, ".csv"), col_names = FALSE)

Z12_x_mat = as.matrix(Z12_x)

#Z12_y_mat = matrix(data = 0, nrow = dim(Z12_x)[1], ncol = dim(Z12_x)[2])

#Algorithm 2 RCP 13

ES = keras::callback_early_stopping(monitor = "val_loss", min_delta = 10^-5, patience = 2)

# #for (i in 1:100){
#   model = keras_model_sequential()
#   model %>%
#     layer_dense(22, activation = 'tanh', input_shape = c(65)) %>%
#     layer_dense(65, activation = 'linear') %>%
#     compile(
#       loss = 'mse',
#       optimizer = 'sgd'
#     )
  
  model2 = keras_model_sequential()
  model2 %>%
    layer_dense(10, activation = 'tanh', input_shape = c(65)) %>%
    layer_dense(65, activation = 'linear') %>%
    compile(
      loss = 'mse',
      optimizer = 'sgd'
    )
  
  #one_hot_labels_Z12 <- to_categorical(Z12_y_mat)
  
  #history <- model %>% keras::fit(x = Z12_x_mat, y = Z12_x_mat, epochs = 250, callbacks = c(ES), validation_split = 0.1)
  
  history <- model2 %>% keras::fit(x = Z12_x_mat, y = Z12_x_mat, epochs = 250, callbacks = c(ES), validation_split = 0.1)
  
  #classes <- model %>% predict(x_test, batch_size = 128)
  
  Z3_union_O_x <- bind_rows(Z3_x, Ones_x)
  #Z3_union_O_y <- bind_rows(Z3_y, Ones_y)
  
  #write_csv(x = Z3_union_O_y, path = "What_Sean_calls_Y3.csv")
  
  n_net_predictions <- model2 %>% predict(as.matrix(Z3_union_O_x))
  
  #mse_for_test_split <- apply((n_net_predictions - as.matrix(Z3_union_O_x))^2, MARGIN = 1, FUN = mean)
  
  
  
  # y <- ... # logical array of positive / negative cases
  # predictions <- ... # array of predictions
  # 
  # pred <- prediction(mse_for_test_split, as.matrix(Z3_union_O_y))
  # 
  # f1_perf <- performance(pred, "f")
  # 
  # # Recall-Precision curve             
  # RP.perf <- performance(pred, "prec", "rec")
  # 
  # plot (RP.perf)
  # 
  # # ROC curve
  # ROC.perf <- performance(pred, "tpr", "fpr")
  # plot (ROC.perf)
  # 
  # # ROC area under the curve
  # auc.tmp <- performance(pred,"auc")
  # auc <- as.numeric(auc.tmp@y.values)
  
  
  # best_threshold <- optimize(calc_f1_score, c(min(mse_for_test_split), max(mse_for_test_split)), 
  #                            mse_output = mse_for_test_split, 
  #                            true_values = as.matrix(Z3_union_O_y), maximum = TRUE)
  # possible_cutoffs <- seq(0.1, 5,  0.1)
  # scores <- vector(length = length(possible_cutoffs))
  # for(i in 1:length(scores)){
  #   scores[i] = calc_f1_score(possible_cutoffs[i], mse_for_test_split, as.matrix(Z3_union_O_y))
  # }
  #   
  # plot(possible_cutoffs, scores)
#}
  
  T_x <- read_csv("T_x.csv", col_names = FALSE)

  
  pred_for_test_data <- model2 %>% predict(as.matrix(T_x))
  
  #mse_for_test_data <- apply((pred_for_test_data - as.matrix(T_x))^2, MARGIN = 1, FUN = mean)
  
  write_csv(x = as_data_frame(mse_for_test_split), path = "MSE_Output_to_find_cutoff.csv")
  
  T_labels <- as.integer(mse_for_test_data > 2.6255574753683319)
  
  write_csv(as_data_frame(T_labels), stringr::str_c("T_labels", org_number, ".csv"))
  }
} #end of the loop



keras::save_model_hdf5(object = model2, filepath = "keras_model2_from_org39.hdf5")
model2 <- keras::load_model_hdf5("keras_model2_from_org39.hdf")



  # print(sum(T_labels))
  # 
  # print(mean(mse_for_test_data))
  # print(mean(mse_for_test_split[1:25980]))
  # print(mean(mse_for_test_split[-(1:25980)]))
  # print(median(mse_for_test_split[1:25980]))
  # print(median(mse_for_test_split[-(1:25980)]))
  # 
  
  # # create model
  # model <- keras_model_sequential()
  # 
  # # define and compile the model
  # model %>% 
  #   layer_dense(units = 32, activation = 'relu', input_shape = c(100)) %>% 
  #   layer_dense(units = 10, activation = 'softmax') %>% 
  #   compile(
  #     optimizer = 'rmsprop',
  #     loss = 'categorical_crossentropy',
  #     metrics = c('accuracy')
  #   )
  # 
  # # Generate dummy data
  # data <- matrix(runif(1000*100), nrow = 1000, ncol = 100)
  # labels <- matrix(round(runif(1000, min = 0, max = 9)), nrow = 1000, ncol = 1)
  # 
  # # Convert labels to categorical one-hot encoding
  # one_hot_labels <- to_categorical(labels, num_classes = 10)
  # 
  # # Train the model, iterating on the data in batches of 32 samples
  # model %>% fit(data, one_hot_labels, epochs=10, batch_size=32)
  # 