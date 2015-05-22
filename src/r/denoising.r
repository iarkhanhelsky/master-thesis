thresholding.hard <- function(x, lambda) {
  t <- c(x)
  t[abs(t) <= lambda] <- 0
  t
}

thresholding.soft <- function(x, lambda) {
  t <- c(x)
  t[abs(x) <= lambda] <- 0
  t[x < -lambda] <- t[x < -lambda] + lambda
  t[x > lambda] <- t[x > lambda] - lambda
  t
}

lambda.universal <- function(s) sqrt(2 * log(s))

lambda.c <- 4.505

