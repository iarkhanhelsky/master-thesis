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