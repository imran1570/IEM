library(BMS)

correlation_false=ldply(correlations.for.det059a[[week]][3],data.frame)

correlation_false=correlation_false[,2]

correlation_false=as.vector(correlation_false)

correlation_false=as.numeric(correlation_false)

correlation_false=correlation_false[!is.na(correlation_false)]

correlation_false=vec2symMat(correlation_false,diag=FALSE)

r=correlation_false

n=num_det058a0det059a0

## Functions
gen.gauss.cop <- function(r, n){
    rho <- 2 * sin(r * pi/6)        # Pearson correlation
    P <- r        # Correlation matrix
    d <- nrow(P)                    # Dimension
    ## Generate sample
    U <- pnorm(matrix(rnorm(n*d), ncol = d) %*% chol(P))
    return(U)
}



U <- gen.gauss.cop(r = r, n = n)
