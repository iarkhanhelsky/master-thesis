require(ggplot2)
require(reshape2)

d.jk <- function(X, j, k) {
  h <- (2**j * k) : (2**j * (k + 1/2) - 1)
  l <- (2**j * (k + 1/2)) : (2**j * (k + 1) -1)

  (2**(-j/2))*(sum(X[h]) - sum(X[l]))
}

I.jk <- function(X, j, k) {
  d.jk(X, j, k) ** 2
}

T.j <- function(X, j, f) {
  k.max <- 2**(log2(length(X)) - j) - 1
  f(sapply(0:k.max, function(k) I.jk(X, j, k)))
}

P.j <- function(X, j) {
  T.j(X, j, max)
}

S.j <- function(X, j) {
  T.j(X, j, sum)
}

K <- 10000
q <- seq(0.05, 0.45, 0.05)
data.false.negative <- lapply(rep(sample(c(8, 10, 12)), length.out=K), function(t) rbinom(2**t, 1, 0.5))
data.false.positive <- lapply(rep(sample(c(8, 10, 12)), length.out=K), function(t) {
  c(rbinom(2**(t-1), 1, 0.5), rbinom(2**(t - 1), 1, sample(q)))
})


criterium <- function(data, alpha = 0.05, t, F) {
  values <- 2 * data - 1
  T <- log2(length(data))
  p.values <- sapply(1:T, function(j) F(t(values, j), T, j))
  all(p.values > alpha / T)
}

criterium.periodogram <- function(data, alpha=0.05) {
  criterium(data, alpha, P.j, function(p, T, j) 1 - pchisq(p, 1) ** (2 ** (T - j)))
}

criterium.scalegram <- function(data, alpha=0.05) {
 criterium(data, alpha, S.j, function(p, T, j) 1 - pchisq(p, 2**(T - j)))
}

false.negative.scale <- sapply(data.false.negative, criterium.scalegram)
false.negative.period <- sapply(data.false.negative, criterium.periodogram)

false.positive.scale <- sapply(data.false.positive, criterium.scalegram)
false.positive.period <- sapply(data.false.positive, criterium.periodogram)

errors <- data.frame(
  a = sapply(1:K, function(k) length(which(!false.negative.scale [1:k])) / k), # Ошибка первого рода, считаем все FALSE
  b = sapply(1:K, function(k) length(which(!false.negative.period[1:k])) / k), # Ошибка первого рода, считаем все FALSE
  c = sapply(1:K, function(k) length(which(false.positive.scale  [1:k])) / k), # Ошибка второго рода, считаем все TRUE
  d = sapply(1:K, function(k) length(which(false.positive.period [1:k])) / k), # Ошибка вторго рода, считаем все TRUE
  k = 1:K
)

data.long <- melt(errors, id='k', value.name='error')
ggplot(data=data.long,
       aes(x=k, y=error, colour=variable)) +
  scale_colour_hue(labels =(c("а) False negative. Scalegram",
                              "b) False negative. Periodogram",
                              "c) False positive. Scalegram",
                              "d) False positive. Periodogram"))) +
  facet_wrap(~variable) +
  geom_line()


