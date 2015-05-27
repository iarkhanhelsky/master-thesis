daubechies <- c("haar", "d4", "d6", "d8", "d10", "d12", "d14", "d16", "d18", "d20")

#' Statistics scheme based on squared
#' coefficients of discrete wavelet
#' transform
#'
#' @param dj - coefficients on current level
#' @param j - scaling level
#' @param f - function which applies over group of
#' squared wavelet coefficient on current scaling
#' level
#'
T.j <- function(dj, f) {
  f(dj**2)
}

#' Wavelet periodogram
#' Statistics which chooses maximum
#' squared wavelet coefficient
#'
#' @param X - data vector
P.j <- function(X) {
  T.j(X, max)
}

#' Wavelet periodogram
#' Statistics which calculates sum of squared
#' wavelet coefficients
#'
#' @param X - data vector
S.j <- function(X) {
  T.j(X, sum)
}

#' Criterium scheme.
#'
#' @param data - data vector to check hypotesis
#' @param alpha - alpha value
#' @param t - statistics which applied to data
#' @param F - error distribution function
#'
criterium <- function(data, filter, alpha = 0.05, t, F) {
  values <- 2 * data -1
  w <- dwt(values, filter)@W
  T <- length(w)
  p.values <- mapply(w, 1:T, FUN = function(w, j) F(t(w), T, length(w)))
  print(p.values)
  all(p.values > alpha / T)
}


gnorm <- function(n) {
  g <- function() {
    t <- 100
    (sum(rbinom(t, 1, 0.5)) - t * 0.5) / (0.25 * sqrt(t))
  }


  replicate(n, g())
}





criterium.periodogram <- function(X, filter, alpha) {
  data <- 2*X - 1

  w <- dwt(data, filter)@W$W1

  1 - pnorm(max(w)) ^ length(w)
}
