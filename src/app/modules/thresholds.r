lambda.range.max <- 10

ui.thresholds <- function() {
  tabPanel("Расчет и визуализация пороговых значений",
           withMathJax(
             mainPanel(
               h3("Визуализация жесткой и мягкой замены значений вейвлет коэффициентов"),
               sliderInput("lambda", "$$\\lamda$$",
                           min = 0, max = lambda.range.max, value = 2, step= 0.5),
               plotOutput("thresholding"),
               h3("Расчет универсального порогового значения в зависимости от размера выборки"),
               sliderInput("sample.length", "n",
                           min = 100, max = 10000, value = 1000, step = 100),
               plotOutput("unviversal"))))
}

srv.thresholds <- function(input, output) {
  output$thresholding <- renderPlot({
    lambda <- input$lambda
    t <- seq(-lambda.range.max, lambda.range.max, 0.01)
    visual.multiplot(
      qplot(t, thresholding.hard(t, lambda), geom='line', main='Жесткая замена', xlab='t', ylab=expression(delta[h])),
      qplot(t, thresholding.soft(t, lambda), geom='line', main='Mягкая замена', xlab='t', ylab=expression(delta[s])),
      cols = 2)})

  output$universal <- renderPlot({
    bound <- input$sample.length

  })
}
