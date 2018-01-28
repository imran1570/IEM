# this file contains the helper functions for RCP16 Modeling

gen.gauss.cop <- function(r, n){
  rho <- 2 * sin(r * pi/6)        # Pearson correlation
  P <- r        # Correlation matrix
  d <- nrow(P)                    # Dimension
  ## Generate sample
  U <- pnorm(matrix(rnorm(n*d), ncol = d) %*% chol(P))
  return(U)
}


make_corr_mat <- function(df){
  
  m = diag(26)   ## 26x26 diagonal matrix
  m[lower.tri(m)] = df$`Spearman Coefficient`  ## fill in lower triangle
  m = t(m)  ## flip around matrix
  m[lower.tri(m)] = df$`Spearman Coefficient`  ## fill in lower triangle
  
  return(m)

}



getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


compute_marginals <- function(x){
  
  return(cumsum(table(x))/length(x))
  
}

calc_prob <- function(x){
  
  probs <- as.data.frame(table(x)/length(x))
  
  (probs$Freq[match(x, probs$Var1)])
  
}






CorrelateData <- function(CorrelationMatrix, DetectorData, CopulaMatrix){
  #This is the copula function implemted by Zhengyang Fan
  # gen.gauss.cop = function(r, n3){
  #   rho <- 2 * sin(r * pi/6) 
  #   P <- r     
  #   d <- nrow(P)
  #   U3 <- pnorm(matrix(rnorm(n3*d), ncol = d) %*% chol(P))
  #   return(U3)
  # }
  numUser=dim(DetectorData)[1]
  numVectors=dim(CorrelationMatrix)[2]
  #Generate copula
  #CopulaMatrix=gen.gauss.cop(CorrelationMatrix,numUser)
  VectorData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  sortedData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  SortedData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  Index2=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  CorrelatedDataMatrix=as.data.frame(matrix(0, nrow = numUser, ncol = numVectors))
  for (i in 1:numVectors){
    Index=sort.int(CopulaMatrix[,i], index.return=TRUE)
    Index2=c(matrix(unlist(Index[2]),nr=1))
    Index2T=t(Index2)
    VectorData=DetectorData[,i]
    sortedData=sort(VectorData)
    SortedData=sortedData[Index2T]
    SortedData[Index2T]=sortedData
    CorrelatedDataMatrix[,i]=SortedData
    VectorData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
    sortedData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
    SortedData=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
    #Index2=as.data.frame(matrix(0, nrow = numUser, ncol = 1))
  }
  return(CorrelatedDataMatrix)	
}


