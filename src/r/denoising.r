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

SURE <- function(lambda, X) {
  s <- length(X)
  s + sum(sapply(X, function(x) min(lambda, X)^2)) - 2*sum(X[abs(X) < lambda])
}

lambda.sure <- function(X) {
  optimise(function(l) SURE(l, X), lower=0, upper=lambda.universal(X))$minimum
}

lambda.sureShrink <- function(X) {
  ifelse(sparsity(X) <= 1, lambda.universal(length(X)), lambda.sure(X))
}

sparsity <- function(X) {
  s <- length(X)
  s^(1/2) * sum(X^2 - 1) / log2(s)^(3/2)
}

sure.shrink <- function(X, filter='d4') {
    d <- dwt(X, filter = filter)
    d@W <- lapply(d@W, function(w) t(t(thresholding.soft(w, lambda.sureShrink(w)))))
    return(idwt(d))
}

sure.shrink2 <- function(X, filter) {
  d <- dwt.2d(X, filter)
  for(name in grep('HH', names(d), value=TRUE)) {
    w <- c(d[[name]])
    r <- thresholding.soft(w, lambda.sureShrink(w))
    d[[name]] <- matrix(r, nrow=dim(d[[name]])[1])
  }
  idwt.2d(d)
}

vis.thresholding.types <- function(lambda, bound) {
  t <- seq(-bound, bound, 0.01)
  r.s <- thresholding.soft(t, lambda)
  r.h <- thresholding.hard(t, lambda)

  visual.multiplot(
    qplot(t, r.h, geom='line', main="Жесткая замена", ylab=expression(delta[h])),
    qplot(t, r.s, geom='line', main="Мягкая замена", ylab=expression(delta[s])),
    cols=2)
}

vis.noisy <- function(t, clean, noisy) {
  data <- data.frame(
    t = t,
    noisy = noisy,
    clean = clean
  )
  data.long <- melt(data, id='t', value.name='x')

  ggplot(data=data.long, aes(x=t, y=x, colour=variable)) +
    theme(legend.position="none") +
    geom_line()
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


vis.diff <- function(t, clean, restored) {
  d <- data.frame(
    t = t,
    clean = clean,
    restored = restored
  )

  d$min <- pmin(d$clean, d$restored)
  d$max <- pmax(d$clean, d$restored)

  ggplot(d) +
    ylab("x(t)") +
    xlab("t") +
    geom_line(aes(x=t, y=clean, color='blue')) +
    geom_ribbon(aes(x=t, ymin=min, ymax=max), fill="grey", alpha=.4) +
    geom_line(aes(x=t, y=restored, color='red'))
}
