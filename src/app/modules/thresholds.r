
ui.thresholds <- function() {
  lambda.range.max <- 10
  
  tabPanel("Расчет и визуализация пороговых значений",
         mainPanel(
           h3("Визуализация жесткой и мягкой замены значений вейвлет коэффициентов"),
           sliderInput("lambda", "Lambda", 
                       min = 0, max = lambda.range.max, value = 2, step= 0.5),
           plotOutput("thresholding"),
           h3("Расчет универсального порогового значения в зависимости от размера выборки"),
           sliderInput("sample.length", "n",
                       min = 100, max = 10000, value = 1000, step = 100),
           plotOutput("unviversal")))
}

srv.thresholds <- function(input, output) {
  output$thresholding <- renderPlot({
    lambda <- input$lambda
    visual.multiplot(
      qplot(seq(0, lambda*pi, 0.05), sin(seq(0, lambda*pi, 0.05)), geom='line', main='Жесткая замена'),
      qplot(seq(0, lambda*pi, 0.05), cos(seq(0, lambda*pi, 0.05)), geom='line', main='Mягкая замена'),      
      cols = 2)})
  
  output$universal <- renderPlot({
    bound <- input$sample.length
    
  })
}