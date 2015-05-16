haar.mother <- function(t) {
  if(0 <= t && t < 1/2) {
    return(1)
  } else if (1/2 <= t && t < 1) {
    return(-1)
  } else {
    return(0)
  }
}

haar.jk <- function(t, j, k) {
  return(2^(-j/2) * haar.mother(2^-j * t - k))
}
# Или в векторной реализации
wavelet.haar <- function(t, j = 0, k = 0) {
  return(Vectorize(function(t) haar.jk(t, j, k))(t))
}



#' DWT - discrete wavelet transform
#'
d.jk <- function(X, j, k, wavelet = wavelet.haar){  
  return(sum(X * wavelet(0:(length(X)-1), j, k)))
}

#' DWT for current level (periodogramm)
d.j <- function(X, j) {
  k.max <- 2^(log2(length(X)) - j) - 1
  return(sapply(0:k.max, function(k) d.jk(X, j, k)))
}

wavelet.approx <- function(f, lower=0, upper=1, wavelet=wavelet.haar, levels=10) {
  c0 <- integrate(f = f, 0, 1)$value
  djk <- function(j, k) integrate(function(t) f(t) * wavelet(t, j, k), 0, 1)$value
  
  coeffs <- matrix(nrow=levels, ncol=2**levels)
  coeffs[, ] <- 0
  
  for(j in 0:(levels - 1)) {
    cc <- coeffs
    for(k in 0:(2**j - 1)) {
       coeffs[j+1, k + 1] <- djk(j, k)
    }
  }
  
  Vectorize(function(t) {
    c0 + sum(outer(1:levels, 1:(2**levels), function(j, k) coeffs[j,k]*wavelet(t, j, k)))
  })
}