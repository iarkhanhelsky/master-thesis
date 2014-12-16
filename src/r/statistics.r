P_j <- function(X, j){
  
  d <- function(X, j, k)
  {
    t <- 2**(-j/2) * (sum(X[(2**j * k) : (2**j * (k + 1/2) - 1)]) - sum(X[(2**j * (k + 1/2)) : (2**j * (k+1) - 1)]))
    return(t)
  }
  
  I_j <- function(X, j){ max(sapply(c(0:((2^log2(length(X))) / 2^(j+1) )), FUN=function(k){I_jk(X, j, k)}))}
  
  F_hi2 <- function(t){ t**(-1/2) * exp(-t/2) / sqrt(2*pi) }
  
  return(1 - (dchisq(I_j(X, j), 1) ** (log2(length(X)) - j)))
}