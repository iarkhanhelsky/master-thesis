setwd('~/Projects/uni/diploma/src')
require('rmarkdown')
rmarkdown::render('diploma.Rmd', 'pdf_document', output_dir='../out')
