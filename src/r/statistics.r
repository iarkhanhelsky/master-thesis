#' DWT - discrete wavelet transform
#'
d.jk <- function(X, j, k, wavelet = wavelet.haar){
  return(sum(X * wavelet(1:length(X), j, k)))
}

P_j <- function(X, j) {
  
  d <- d.jk
  
  I_jk <- function(X, j, k) {d(X, j, k) ** 2}
  
  I_j <- function(X, j) { 
    jk <- sapply(c(0:((2^log2(length(X))) / 2^(j+1) )), FUN=function(k){I_jk(X, j, k)})
    list(max.value = max(jk), max.k = which.max(jk))
  }
  
  ij <- I_j(X, j)
  p.value <- (1 - (dchisq(ij$max.value, 1) ** (log2(length(X)) - j)))
  k <- ij$max.k
  
  list(p.value = p.value, k = k)
}

#' Wavelet autocorellation function generator
#' @param u - points for evaluation
#' @param j - scale level
#' @param k - translation level
#' @param T - size of series
#' @param psi - mother wavelet
#' 
#' @references wavelet.haar
#' 
#' @return function Psi.jk(u) which is autocovariative function
#' for specified levels and wavelet
#' 
Psi.jk <- function(u, j, k, T, psi = wavelet.haar) {
  return (Vectorize(function(u) {
    sum(psi(0:T-1, j, k) * psi((0:T-1)+abs(u), j, k))
  })(u))
}

#' Wavelet spectrum function generator
#' @param X - time series
#' @param j - scale level
#' @param k - translation level
#' @param psi - mother wavelet
#' 
#' @references wavelet.haar
#' 
#' @return  wavelet spectrum of time series X at scale 
#' level j and translation level k
#'
eta.jk <- function(X, j, k, psi = wavelet.haar) {
  T <- length(X)
  
  # Cumpute autocovariation values for time series
  acf.values <- acf(X, lag.max=length(X), type='covariance', plot=F)$acf  
  
  # Compute wavelet autocorrelation values for time series
  wacf.values <- Psi.jk(0:(T-1), j, k, T, psi) # wavelet autocorrelation function
  
  sum(acf.values * wacf.values)
} 