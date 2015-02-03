#' White noise process with variance sigma^2
#'
white.noise <- rnorm

#' Sinusoidal process of 2^M observations and period of 2n*pi
#' 
sin.series <- function(n = 1, M = 10) {
  return (sin(seq(0, 1, length.out=2**M)*n*2*pi))
}