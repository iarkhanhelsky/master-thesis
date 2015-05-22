require(wavelets)

t <- seq(0, 5*pi, length.out=4096)
x <- sin(t) + cos(t)*exp(-t)
n <- 0.05 * rnorm(length(t), 0, 1)
u <- x + n
plot(t, x, type='l')
plot(t, u, type='l')


d <- dwt(u, filter='d10')
snr <- (max(t) - min(t))^2 / var(n)
print(snr)

lambda.u <- function(s) sqrt(2*log(s))

threshold <- function(t, lambda) {
  t[abs(t) < lambda] <- 0
  t
}

l <- lapply(d@W, function(t) threshold(t, lambda.u(length(t))))

d@W <- l

r <- idwt(d)

plot(t, r, type='l')

print(mean(sapply(1:length(t), function(t) (x[t] - r[t])^2)))
