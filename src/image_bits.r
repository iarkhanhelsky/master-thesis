source('r/images/images.r')
lenna <- image.read('res/Lenna.png')
require('ggplot2')

freqs <- function(data) {
  sapply(unique(data), function(t) sum(data == t) / length(data))
}

model.length <- 10000
model.p.good <- 0.5
model.p.bad <- 0.3

model.good <- rbinom(model.length, 1, model.p.good)
model.bad <- rbinom(model.length, 1, model.p.bad)

gen.data.list <- function(data, desc='', alpha=0.05) {
  f <- freqs(data)
  p.value <- chisq.test(f)$p.value
  list(
    Description= desc,
    Length = length(data),
    Mean = mean(data),
    Variance = var(data),
    'P-value Chi' = p.value,
    'Null Hypotesis' = ifelse(1-p.value < alpha, 'Accepted', 'Denied'),
    Freqs = f
  )
}

bits <- function(x, len = 8) {
  sapply(len:1, function(p) bitwShiftR(x, p - 1) %% 2)
}

vec.bits <- Vectorize(bits, vectorize.args = c('x'))

data <- gen.data.list(model.good, paste('Выборка случайных величин распределенных по закону Бернулли с параметром p =', model.p.good))

data <- rbind(data, 
              gen.data.list(model.bad, paste('Выборка случайных величин распределенных по закону Бернулли с параметром p =', model.p.bad)))

lenna <- image.bytes(image.read('res/Lenna.png'))
lenna.mod.2 <- lenna %% 2
lenna.mod.8 <- lenna %% 8
data <- rbind(data, gen.data.list(as.vector(lenna.mod.2[, , 1]),'Младший бит цветовой компоненты картинки. Красный'))
data <- rbind(data, gen.data.list(as.vector(lenna.mod.2[, , 2]),'Младший бит цветовой компоненты картинки. Зеленый'))
data <- rbind(data, gen.data.list(as.vector(lenna.mod.2[, , 3]),'Младший бит цветовой компоненты картинки. Синий'))
data <- rbind(data, gen.data.list(as.vector(lenna.mod.2),'Младший бит цветовой компоненты картинки. Совместно'))

data <- rbind(data, gen.data.list(as.vector(lenna.mod.8[, , 1]),'Младшее битовое слово (длины 3) цветовой компоненты картинки. Красный'))
data <- rbind(data, gen.data.list(as.vector(lenna.mod.8[, , 2]),'Младшее битовое слово (длины 3) цветовой компоненты картинки. Зеленый'))
data <- rbind(data, gen.data.list(as.vector(lenna.mod.8[, , 3]),'Младшее битовое слово (длины 3) цветовой компоненты картинки. Синий'))
data <- rbind(data, gen.data.list(as.vector(lenna.mod.8),'Младшее битовое слово (длины 3) компоненты картинки. Совместно'))

data <- rbind(data, gen.data.list(as.vector(vec.bits(as.vector(lenna.mod.8[, , 1]), len=3)),'Младшее битовое слово (длины 3, побитно) цветовой компоненты картинки. Красный'))
data <- rbind(data, gen.data.list(as.vector(vec.bits(as.vector(lenna.mod.8[, , 2]), len=3)),'Младшее битовое слово (длины 3, побитно) цветовой компоненты картинки. Зеленый'))
data <- rbind(data, gen.data.list(as.vector(vec.bits(as.vector(lenna.mod.8[, , 3]), len=3)),'Младшее битовое слово (длины 3, побитно) цветовой компоненты картинки. Синий'))
data <- rbind(data, gen.data.list(as.vector(vec.bits(as.vector(lenna.mod.8), len=3)),'Младшее битовое слово (длины 3, побитно) компоненты картинки. Совместно'))


View(data.frame(data))