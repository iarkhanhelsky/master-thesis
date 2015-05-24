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


vis.thresholding.types <- function(lambda, bound) {
  t <- seq(-bound, bound, 0.01)
  r.s <- thresholding.soft(t, lambda)
  r.h <- thresholding.hard(t, lambda)

  visual.multiplot(
    qplot(t, r.h, geom='line', main="Жесткая замена", ylab=expression(delta[h])),
    qplot(t, r.s, geom='line', main="Мягкая замена", ylab=expression(delta[s])),
    cols=2)
}

 vis.universal.bound <- function(s = 1000) {
  t <- 1:s
  v <- rnorm(t)

  d <- data.frame(amp = abs(v), t = t)
  u <- data.frame(t = c(-Inf, Inf), y = lambda.universal(s), u = factor(lambda.universal(s)))
  m <- data.frame(t = c(-Inf, Inf), mean = mean(v))

  ggplot(d, aes( t, amp ) ) +
    ylab('Амплитуда') +
    geom_point() +
    geom_hline(color='red', yintercept=lambda.universal(s)) +
    annotate("text", 0, ymin = lambda.universal(s),y=lambda.universal(s) * 1.05, label = "lambda[U]", parse=TRUE, show_guide=TRUE)
}
