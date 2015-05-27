#' DWT - discrete wavelet transform (Haar)
#'
d.jk <- function(X, j, k) {
  h <- (2**j * k) : (2**j * (k + 1/2) - 1)
  l <- (2**j * (k + 1/2)) : (2**j * (k + 1) -1)

  (2**(-j/2))*(sum(X[h]) - sum(X[l]))
}

#' Statistics scheme based on squared
#' coefficients of discrete wavelet
#' transform
#'
#' @param X - data vector
#' @param j - scaling level
#' @param f - function which applies over group of
#' squared wavelet coefficient on current scaling
#' level
#'
T.j <- function(X, j, f) {

  I.jk <- function(X, j, k) {
    d.jk(X, j, k) ** 2
  }

  k.max <- 2**(log2(length(X)) - j) - 1
  f(sapply(0:k.max, function(k) I.jk(X, j, k)))
}

#' Wavelet periodogram
#' Statistics which chooses maximum
#' squared wavelet coefficient
#'
#' @param X - data vector
#' @param j - scaling level
P.j <- function(X, j) {
  T.j(X, j, max)
}

#' Wavelet periodogram
#' Statistics which calculates sum of squared
#' wavelet coefficients
#'
#' @param X - data vector
#' @param j - scaling level
S.j <- function(X, j) {
  T.j(X, j, sum)
}

#' Criterium scheme.
#'
#' @param data - data vector to check hypotesis
#' @param alpha - alpha value
#' @param t - statistics which applied to data
#' @param F - error distribution function
#'
criterium <- function(data, alpha = 0.05, t, F) {
  values <- 2 * data - 1
  T <- log2(length(data))
  p.values <- sapply(1:T, function(j) F(t(values, j), T, j))
  all(p.values > alpha / T)
}

#' Criterium to check binary sequence over null hypotesis
#' which postulates that all sequence counts have same
#' distribution (Be(0.5))
#'
#' Criterium uses periodogram statistics to determine discord
criterium.periodogram <- function(data, alpha=0.05) {
  criterium(data, alpha, P.j, function(p, T, j) 1 - pchisq(p, 1) ** (2 ** (T - j)))
}

criterium.scalegram <- function(data, alpha=0.05) {
  criterium(data, alpha, S.j, function(p, T, j) 1 - pchisq(p, 2**(T - j)))
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


#' DWT for current level (periodogramm)
d.j <- function(X, j) {
  k.max <- 2^(log2(length(X)) - j) - 1
  return(sapply(0:k.max, function(k) d.jk(X, j, k)))
}
