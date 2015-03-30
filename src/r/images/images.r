require('png')
require('tools')
require('hash')

image.read <- function(src) {
	read.routes <- hash()
	read.routes[['png']] <- readPNG
	ext <- tolower(file_ext(src))

	stopifnot(!is.null(read.routes[[ext]]))

	return(read.routes[[ext]](src))
}


image.bytes <- function(floats, color.quant = 255) {
	apply(floats, 1:length(dim(floats)), function(x) as.integer(x * color.quant))
}

image.bits <- function(data, keep = 8) {
	apply(data, 1, integer.bits, keep = keep)
}

integer.bits <- function(x, keep = 8) {
	intToBits(x)
}
