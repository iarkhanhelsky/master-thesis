getFilename <- function() {
   args <- commandArgs()
   filearg <- grep("^--file=", args, value=TRUE)
   if (length(filearg))
     sub("^--file=", "", filearg)
   else
     invisible(NULL)
}
setwd(paste(dirname(normalizePath(getFilename())),'/src', sep=""))
require('rmarkdown')
rmarkdown::render('diploma.Rmd', 'pdf_document', output_dir='../out')
