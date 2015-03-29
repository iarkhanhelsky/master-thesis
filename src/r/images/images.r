require('png')
require('tools')
image.read <- function(src) {
	read.routes <- hash()
	read.routes[['png']] <- readPNG
	ext <- tolower(file_ext(src))
	stopifnot(read.routes[[ext]])

	return(read.routes[[ext]](src))
}


image.bytes <- function(floats, color.quant = 255) {
	apply(floats, 1:length(dim(floats)), function(x) as.integer(x * color.quant))
}

image.bits <- function(data, keep = 8) {
	bytes <- ifelse(is.integer(data), data, image.bytes(data))

	apply(bytes, 1:length(dim(bytes)), integer.bits, keep = keep)
}

integer.bits <- function(x, keep = 8) {
	len <- as.integer(log2(x)) + 1
	shift <- len - keep
	bits <- intToBits(x)[(max(shift, 0) + 1):len]
	c(replicate(ifelse(shift < 0, -shift, 0), 0), rev(as.integer(bits)))
}