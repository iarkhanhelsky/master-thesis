ui.denoise1d <- function() {
  tabPanel("Решение задачи фильтрации в одномерном случае",
           plotOutput('plot.function'),
           fluidRow("Отношение сишнал/шум \\(\\frac{D(X)}{D(X - Y)}\\):", textOutput('snr', inline = T)),
           hr(),
           fixedRow(
             column(4,
                    textInput('f', "Исходная фунцкия \\(f(t)\\)", 'sin(10* pi * t) * exp(cos(t))')),
             column(4, offset=5,
                    sliderInput('noise', 'Уровень шума \\(\\sigma\\)', min = 0, max=10, value = 0.05, step=0.01))
             ),
           hr(),
           plotOutput('noise.decomp'),
           hr(),
           fixedRow(
             column(2, sliderInput('decomp.levels', 'Количество уровней разложения', min = 1, max=16, value = 8)),
             column(2, offset=1, uiOutput('c.decomp.cj')),
             column(2, offset=1, uiOutput('c.decomp.dj')),
             column(2, offset=1,selectInput('filter', 'Вейвлет фильтры', choices = wt.list(), selected = 'd4'))
             ),
           h3('SureShrink'),
           plotOutput('sure.shrink'),
           h3('NeighBlock'),
           plotOutput('neight.block')
           )
}

ui.denoise2d <- function() {
  tabPanel("Решение задачи фильтрации в двумерном случае",
           h3("This is the first panel"))
}

srv.denoise1d <- function(input, output) {

  model.function <- reactive({
    function(t) eval(parse(text=input$f))
  })

  model.data <- reactive({
    t <- seq(0, 1, length.out=2**(input$decomp.levels + 2))
    x <- sapply(t, model.function())
    list(noise=x + input$noise * rnorm(length(t)), clean = x, t = t)
  })

  noisy.data <- reactive({
    t <- seq(0, 1, length.out=2048)
    x <- sapply(t, model.function())
    u <- x + input$noise * rnorm(length(t))
    list(t = t, x = x, y=u)
  })

  output$plot.function <- renderPlot({
     vis.noisy(noisy.data()$t, noisy.data()$x, noisy.data()$y)
  })

  output$snr <- renderText({
    round(var(noisy.data()$x) / var(noisy.data()$x - noisy.data()$y), 3)
  })

  output$c.decomp.cj <- renderUI({
    withMathJax(sliderInput('decomp.app', min = 1, value = 1, max = input$decomp.levels, step=1, 'Уровень аппроксимации \\(c_j\\)'))
  })

  output$c.decomp.dj <- renderUI({
    withMathJax(sliderInput('decomp.dj', min = 1, value = 1, max = input$decomp.levels, step=1, 'Уровень детализации \\(d_j\\)'))
  })

  output$noise.decomp <- renderPlot({
    vis.decomp(model.data()$noise, filter=input$filter, input$decomp.dj, input$decomp.app)
  })

  output$sure.shrink <- renderPlot({
      qplot(model.data()$t, sure.shrink(model.data()$noise, filter=input$filter), geom='line')
  })
}

srv.denoise2d <- function(input, output) {

}
