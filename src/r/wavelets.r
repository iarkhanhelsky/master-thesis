wavelet.haar <- function(t, j = 0, k = 0) {
  haar.mother <- function(t) {
    sapply(t, function(x) ifelse( 0 <= x && x < 0.5, 1, ifelse(0.5 <= x && x <= 1, -1, 0)))
  }

  return(2^(-j/2) * haar.mother(2^-j * t - k))
}

wavelet.approx <- function(f, lower=0, upper=1, wavelet=wavelet.haar, levels=10) {
  c0 <- integrate(f = f, 0, 1)$value
  djk <- function(j, k) integrate(function(t) f(t) * wavelet(t, -j, k), 2**(-j)*k, 2**(-j)*(k+1))$value

  coeffs <- outer(1:levels-1, 1:2**levels - 1, Vectorize(djk, vectorize.args = c("j", "k")))

  Vectorize(function(t) {
    c0 + sum(outer(1:levels, 1:(2**levels), Vectorize(function(j, k) coeffs[j, k] * wavelet(t, -j+1, k-1), vectorize.args = c("j", "k"))))
  })
}

space.shrink <- function(f, lower, upper) function(t) f(lower + t * (upper - lower))

space.expand <- function(f, lower, upper) function(t) f((t - lower) / (upper - lower))
