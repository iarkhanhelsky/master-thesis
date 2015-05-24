library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  withMathJax(),
  titlePanel("Решение задачи фильтрации сигнала на"),
  navlistPanel(ui.thresholds(), ui.denoise1d(), ui.denoise2d())
))
