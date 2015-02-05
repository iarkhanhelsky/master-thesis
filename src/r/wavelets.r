
wavelet.haar <- function(t, j = 0, k = 0) {
#   haar <- function (t) { 2^(-j/2) * ifelse(2^j * k <= t && t < 2^j * (k + 0.5), 1,
#          ifelse(2^j * (k + 0.5) <= t && t < 2^j * (k + 1), -1, 0))
#   } 
#   sapply(t, function(u) haar(u))
  j <- -j
  haar <- function(t){
    if(2^j * k <= t && t < 2^j * (k + 0.5)) {
      return(2^(-j/2))
    } else if (2^j * (k + 0.5) <= t && t < 2^j * (k + 1)){
      return(-2^(-j/2))
    } else {
      return(0)
    }    
  }
  
  return(sapply(t, function(u) haar(u)))
}