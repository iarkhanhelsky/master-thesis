library(shiny)

shinyServer(function(input, output) {
  srv.thresholds(input, output)
  srv.denoise1d(input, output)
  srv.denoise2d(input, output)
})
