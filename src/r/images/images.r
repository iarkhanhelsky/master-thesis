require('png')
require('tools')
require('hash')
require('abind')

COLOR_QUANT <- 255
COLOR_BITS <- as.integer(log2(COLOR_QUANT)) + 1

image.read <- function(src) {
	read.routes <- hash()
	read.routes[['png']] <- readPNG
	ext <- tolower(file_ext(src))

	stopifnot(!is.null(read.routes[[ext]]))

	return(read.routes[[ext]](src))
}


image.bytes <- function(floats) {
	apply(floats, 1:length(dim(floats)), function(x) as.integer(x * COLOR_QUANT))
}

image.bits <- function(data) {
	data <- if(is.integer(data)) data else image.bytes(data)
	word.length <- COLOR_BITS
	dim.len <- length(dim(data))
	result.dim <- c(dim(data), word.length)
	aperm(apply(data, 1:dim.len, integer.bits, word.length = word.length), c(c(1:dim.len) + 1, 1))
}

integer.bits <- function(x, word.length) {
	as.integer(intToBits(x))[1:word.length]
}


