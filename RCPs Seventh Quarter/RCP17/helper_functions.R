library(Matrix)

vec2symmat <- function(invec, diag = 1, byrow = TRUE) {
  Nrow <- ceiling(sqrt(2*length(invec)))
  
  if (!sqrt(length(invec)*2 + Nrow) %% 1 == 0) {
    stop("invec is wrong length to create a square symmetrical matrix")
  }
  
  mempty <- matrix(0, nrow = Nrow, ncol = Nrow)
  mindex <- matrix(sequence(Nrow^2), nrow = Nrow, ncol = Nrow, byrow = byrow)
  if (isTRUE(byrow)) {
    mempty[mindex[lower.tri(mindex)]] <- invec
    mempty[lower.tri(mempty)] <- t(mempty)[lower.tri(t(mempty))]
  } else {
    mempty[mindex[upper.tri(mindex)]] <- invec
    mempty[lower.tri(mempty)] <- t(mempty)[lower.tri(t(mempty))]
  }
  
  diag(mempty) <- diag
  mempty
}



CorrelateData <- function(DetectorData, CopulaMatrix){
  #This is the copula function implemted by Zhengyang Fan
  # gen.gauss.cop = function(r, n3){
  #   rho <- 2 * sin(r * pi/6) 
  #   P <- r     
  #   d <- nrow(P)
  #   U3 <- pnorm(matrix(rnorm(n3*d), ncol = d) %*% chol(P))
  #   return(U3)
  # }
  numUser=dim(DetectorData)[1]
  numVectors=dim(CopulaMatrix)[2]
  #Generate copula
  #CopulaMatrix=gen.gauss.cop(CorrelationMatrix,numUser)
  VectorData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  sortedData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  SortedData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  Index2=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  CorrelatedDataMatrix=as.data.frame(matrix(0, nrow = numUser, ncol = numVectors))
  for (i in 1:numVectors){
    Index=sort.int(CopulaMatrix[,i], index.return=TRUE)$ix
    #Index2=c(matrix(unlist(Index[2]),nrow=1))
    VectorData=DetectorData[,i]
    sortedData=sort(VectorData)
    SortedData=sortedData[Index]
    SortedData[Index]=sortedData
    CorrelatedDataMatrix[,i]=SortedData
    VectorData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
    sortedData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
    SortedData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
    Index=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  }
  return(as.data.frame(CorrelatedDataMatrix))
}


DataCorrelationCopier <- function(uncorrelated_data, chol_correlated_data){
  num_rows <- nrow(chol_correlated_data)
  num_cols <- ncol(chol_correlated_data)
  CorrelatedDataMatrix=as.data.frame(matrix(0, nrow = num_rows, ncol = num_cols))
for (i in 1:num_cols){
  VectorData <- uncorrelated_data[,i]
  Index <- sort.int(chol_correlated_data[,i], index.return=TRUE)$ix
  SortedData <- rep(0, length(Index))
  SortedData[Index] <- sort(VectorData)
  CorrelatedDataMatrix[,i] <- SortedData
}
  return(as.data.frame(CorrelatedDataMatrix))
}#end of function


CorrelateDataByIndeces <- function(UncorrelatedData, CorrelatedData){
  
  numUser=dim(CorrelatedData)[1]
  numVectors=dim(CorrelatedData)[2]
  #Generate copula
  #CopulaMatrix=gen.gauss.cop(CorrelationMatrix,numUser)
  #VectorData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  #SortedData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  #Index2=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  CorrelatedDataMatrix=as.data.frame(matrix(0, nrow = numUser, ncol = numVectors))
  for (i in 1:numVectors){
    Index=sort.int(CorrelatedData[,i], index.return=TRUE)$ix
    #Index2=c(matrix(unlist(Index[2]),nrow=1))
    VectorData=UncorrelatedData[,i]
    SortedData=VectorData[Index]
    CorrelatedDataMatrix[,i]=SortedData
    #VectorData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
    #SortedData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
    #Index=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  }
  return(as.data.frame(CorrelatedDataMatrix))
}


row_randomizer_by_column <- function(df){
  df <- as.data.frame(df)
  for (col in 1:ncol(df)){
    df[,col] <- sample(df[,col])
  }
  return(df)
}


dummy_2dcorrelated_data_generator <- function(all_detectors, months, leave_avg_pop_size, noleave_avg_pop_size, list_of_time_corr_mats, det_to_det_corr_leave_list, det_to_det_corr_noleave_list){
  
  # correlate dummy detectors across time using cholesky decomposition of time correlations
  list_of_uncorrelated_gaussian_samples_leave <- list()
  list_of_uncorrelated_gaussian_samples_noleave <- list()
  list_of_time_correlated_gaussian_samples_leave <- list()
  list_of_time_correlated_gaussian_samples_noleave <- list()
  counter <- 0
  Sigma <- diag(length(months))
  for(det in all_detectors){
    counter <- counter+1
    list_of_uncorrelated_gaussian_samples_leave[[det]] <- mvrnorm(n=leave_avg_pop_size, mu = rep(0, length(months)), Sigma = Sigma)
    list_of_uncorrelated_gaussian_samples_noleave[[det]] <- mvrnorm(n=noleave_avg_pop_size, mu = rep(0, length(months)), Sigma = Sigma)
    list_of_time_correlated_gaussian_samples_leave[[det]] <- list_of_uncorrelated_gaussian_samples_leave[[det]] %*% chol(list_of_time_corr_mats[[det]])
    list_of_time_correlated_gaussian_samples_noleave[[det]] <- list_of_uncorrelated_gaussian_samples_noleave[[det]] %*% chol(list_of_time_corr_mats[[det]])
  }
  
  # correlate data across dummy detectors using cholesky decomposition of detector correlations
  list_of_time_and_det_correlated_gaussian_samples_leave <- list()
  list_of_time_and_det_correlated_gaussian_samples_noleave <- list()
  counter <- 0
  for (month in months){
    counter <- counter+1
    time_correlated_gaussian_samples_by_month_leave <- lapply(list_of_time_correlated_gaussian_samples_leave, function(df,month){df[,month]}, month = counter) %>% bind_cols # tranforming back to original format that is, each matrix is monthly data for all detectors
    time_correlated_gaussian_samples_by_month_noleave <- lapply(list_of_time_correlated_gaussian_samples_noleave, function(df,month){df[,month]}, month = counter) %>% bind_cols # tranforming back to original format that is, each matrix is monthly data for all detectors
    list_of_time_and_det_correlated_gaussian_samples_leave[[month]] <- as.matrix(time_correlated_gaussian_samples_by_month_leave) %*% as.matrix(chol(det_to_det_corr_leave_list[[month]]))
    list_of_time_and_det_correlated_gaussian_samples_noleave[[month]] <- as.matrix(time_correlated_gaussian_samples_by_month_noleave) %*% as.matrix(chol(det_to_det_corr_noleave_list[[month]]))
  }
  return(list(list_of_time_and_det_correlated_gaussian_samples_leave, list_of_time_and_det_correlated_gaussian_samples_noleave))
}#end of function
