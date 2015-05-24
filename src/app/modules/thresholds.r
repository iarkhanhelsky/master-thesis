lambda.range.max <- 10

ui.thresholds <- function() {
  tabPanel("Расчет и визуализация пороговых значений",
           withMathJax(
             mainPanel(
               h3("Визуализация жесткой и мягкой замены значений вейвлет коэффициентов"),
               sliderInput("lambda", "Указать пороговое значение \\(\\lambda\\)",
                           min = 0, max = lambda.range.max, value = 2, step= 0.5),
               plotOutput("thresholding"),
               h3("Расчет универсального порогового значения \\(\\lambda_U\\) в зависимости от размера выборки"),
               sliderInput("sample.length", "Длинна выборки \\(n\\) из нормального распределения \\(\\mathcal{N}(0, 1)\\)",
                           min = 100, max = 10000, value = 1000, step = 100),
               plotOutput("universal"))))
}

srv.thresholds <- function(input, output) {
  output$thresholding <- renderPlot({
    vis.thresholding.types(input$lambda, lambda.range.max)})

  output$universal <- renderPlot({
    vis.universal.bound(input$sample.length)})
}
