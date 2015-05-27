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
           fluidRow("Среднеквадратическая ошибка \\(\\int (f(t) - r(t))^2 dt = \\)", textOutput('sure.error', inline = T)),
           h3('NeighBlock'),
           plotOutput('neigh.block'),
           fluidRow("Среднеквадратическая ошибка \\(\\int (f(t) - r(t))^2 dt = \\)", textOutput('neigh.error', inline = T))
           )
}

ui.denoise2d <- function() {
  tabPanel("Решение задачи фильтрации в двумерном случае",
           selectInput('select.img', 'Изображение для обработки', choices = list.files('www/', '*.png')),
           plotOutput('src.img'),
           sliderInput('noise.level', 'Уровень шума', min = 0, max=1, step=0.05, value=0.5),
           plotOutput('img.noisy'),
           hr(),
           selectInput('wt', 'Вейвлет фильтры', choices = wt2d.list(), selected = 'd4'),
           hr(),
           h3('SureShrink'),
           plotOutput('sure.res'),
           fluidRow("Среднеквадратическая ошибка \\(\\int (f(t) - r(t))^2 dt = \\)", textOutput('sure2.error', inline = T)),
           fluidRow("Отношение сигнал / шум", fixedRow("До ", textOutput("snr.sure.before", inline = T)),
                    fixedRow("После ", textOutput('snr.sure.after', inline = T))),
           hr(),
           h3('NeighBlock'),
           plotOutput('neigh.res'),
           fluidRow("Среднеквадратическая ошибка \\(\\int (f(t) - r(t))^2 dt = \\)", textOutput('neigh2.error', inline = T)),
           fluidRow("Отношение сигнал / шум", fixedRow("До ", textOutput("snr.neigh.before", inline = T)),
                    fixedRow("После ", textOutput('snr.neigh.after', inline = T)))
           )

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

  sure.result <- reactive({
    sure.shrink(model.data()$noise, input$filter)
  })

  output$sure.shrink <- renderPlot({
   vis.diff(model.data()$t, model.data()$clean, sure.result())
  })

  output$sure.error <- renderText({
    mean((model.data()$clean - sure.result())^2)
  })

  neigh.result <- reactive({
    neigh.block(model.data()$noise, input$filter)
  })

  output$neigh.block <- renderPlot({
    vis.diff(model.data()$t, model.data()$clean, neigh.result())
  })

  output$neigh.error <- renderText({
    mean((model.data()$clean - neigh.result())^2)
  })
}

srv.denoise2d <- function(input, output) {

  img <- reactive({
    im <- readPNG(paste('www', input$select.img, sep='/'))
    if (length(dim(im)) == 3) {
      return (im[, , 1])
    }
    else
      return(im)
  })

  noisy.img <- reactive({
    i <- img()
    d <- dim(i)
    i + input$noise.level * matrix(rnorm(d[1] * d[2]), ncol=d[1])
  })

  sure2.result <- reactive({
    sure.shrink2(noisy.img(), input$wt)
  })

  sure2.error <- reactive({
    var(as.vector(img() - sure2.result()))
  })

  output$source.img <- renderImage({
    list(
      src = "www/Lenna.png",
      filetype = "image/png",
      alt = "Lenna"
    )
  }, deleteFile=FALSE)

  output$src.img <- renderPlot({
    plot.image(img())
  })

  output$img.noisy <- renderPlot({
    plot.image(noisy.img())
  })

  snr <- reactive({
    var(as.vector(img())) / var(as.vector(img() - noisy.img()))
  })

  snr.sure <- reactive({
    var(as.vector(img())) / var(as.vector(img() - sure2.result()))
  })

  output$sure.res <- renderPlot({
    plot.image(sure2.result())
  })

  output$sure2.error <- renderText({
    round(sure2.error(), 4)
  })

  output$snr.sure.before <- renderText({
    round(snr(), 4)
  })

  output$snr.sure.after <- renderText({
    round(snr.sure(), 4)
  })


  neigh2.result <- reactive({
    neigh.block2(noisy.img(), input$wt)
  })

  neigh2.error <- reactive({
    var(as.vector(neigh2.result() - img()))
  })

  snr.neigh <- reactive({
    var(as.vector(img())) / var(as.vector(img() - neigh2.result()))
  })

  output$neigh.res <- renderPlot({
    plot.image(neigh2.result())
  })

  output$neigh2.error <- renderText({
    round(neigh2.error(), 4)
  })

  output$snr.neigh.before <- renderText({
    round(snr(), 4)
  })

  output$snr.neigh.after <- renderText({
    round(snr.neigh(), 4)
  })
}
