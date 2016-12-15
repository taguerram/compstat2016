library(shiny)
library(plotly)
library(ggplot2movies)
library(DT)

server <- function(input,output){

# TAREA 1  

  Finv <- reactive ({
    seed <- set.seed(101010)
    inversa <- function(lambda, U){
      -1/input$lambda*log(1-U)
    }
    U <- inversa(input$lambda, runif(input$sim))
    U
  })
  
  E <- reactive ({
    E <- rexp(input$sim)
    E
  })
  
  # Histograma
  
  output$hist <- renderPlot({
    hist(Finv(), breaks = input$bin)})
  
  # Xi cuadrada
  
  output$text <- renderText({ 
    
    numberBins <- input$bin 
    
    bins <- quantile(E(),seq(-0, 1, by=1/numberBins))
    bins[1] = bins[1] - 0.001
    
    theoretical <- cut(E(),breaks=bins)
    predicted <- cut(Finv(),breaks=bins)
    
    rest <-  as.numeric(theoretical) -  as.numeric(predicted)
    
    result <- sum(as.numeric(rest)**2/as.numeric(theoretical),na.rm = TRUE)
    
    final <- (result < qchisq(.95, df=numberBins -1 - 1))
    if(final)
    {
      "Prueba Xi cuadrada: Se puede rechazar la hipótesis nula."  
    }
    else
    {
      "Prueba Xi cuadrada: No se puede rechazar la hipótesis nula."
    }
    
  })
  
  # Comparación visual
  
  output$comp <- renderPlotly ({
    
    plot_ly(x = E(), type="histogram", alpha=0.5, name = "Distribución Teórica", nbinsx = input$bin) %>%
      add_histogram(x = Finv(), alpha=0.5, name = "Distribución Predicha", nbinsx = input$bin) %>%
      layout(barmode = "overlay")
    
  })

# TAERA 2
  
# Otras cosas que puedo poner:
  # Gráfica de la curva y los puntitos de la simulación
  # comparación con método del trapecio
  
    
  funcion_t2 <- reactive ({
    texto <- paste("aux <- ", input$funcion)
    eval(parse(text=texto))
    aux
  })
  
  
  FMC <- reactive({
    
    seed <- set.seed(101010)
    n <- input$sim2
    inferior <- input$inferior
    superior <- input$superior
    f <- funcion_t2()
    x <- runif(n, inferior, superior)
    f_x <- sapply(x, f)
    
    estimador <- (superior-inferior)*mean(f_x)
    
    estimador
  })
  
  # Área bajo la curva
  
  output$area <- renderText({
    
    paste("El área bajo la curva es", FMC())
  })

  # Gráfica
  
  output$areaplot <- renderPlotly({
    
    N <- c(10, 100, 1000, 10000, 100000)
    f <- N*10
    seed <- set.seed(101010)
    inferior <- input$inferior
    superior <- input$superior
    a <- 1-(input$conf)
    
    sim <- function(N) {runif(N, inferior, superior)}
    x <- sapply(N, sim)
    
    f <- funcion_t2()
    f_x <- sapply(x, f)
    
    media_x <- function(x) {(superior-inferior)*mean(x)}
    estimador <- sapply(f_x, media_x)
    
    cuantil <- qnorm(a/2, lower.tail = FALSE)
    
    varianza <- function(x) {var(x)}
    varianza_x <- sapply(f_x, varianza)
    
    limsup <- estimador + cuantil*sqrt(varianza_x/N)
    liminf <- estimador - cuantil*sqrt(varianza_x/N)

    plot_ly(x = N, y = estimador, type = 'scatter', mode = 'lines', name = 'estimación') %>%
      add_lines(x= N, y = limsup, name = 'límite superior') %>%
      add_lines(x= N, y = liminf, name = 'límite inferior') %>%
      layout(xaxis = list(type = "log"))

  })
  
  # TAREA 3
  
  # Tabla con datos
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- movies[which(movies$budget != 'NA'), c(1:6)]
    data
  }))
  
  # Definir variables
  
  varindep <- reactive({
    data <- movies[which(movies$budget != 'NA'), c(1:6)]
    varindep <- data[ ,which(names(data) == input$varindep)]
    varindep
  })
  
  vardep <- reactive({
    data <- movies[which(movies$budget != 'NA'),  c(1:6)]
    varindep <- data[ ,which(names(data) == input$vardep)]
    varindep
  })
  
  datos <- reactive({
    datos <- cbind(varindep(), vardep())
    nombres <- c('varindep', 'vardep')
    colnames(datos) <- nombres
    datos
  })
  
  # Scatter plot 
  
  output$scatterplot <- renderPlotly ({
    datos <- datos()
    plot_ly(datos, x = datos$varindep, y = datos$vardep, type = 'scatter', mode = 'markers')
  })
  
  # Distribuciones a priori
  
  alphaDist <- reactive({
    input$alpha
  })
  
  betaDist <- reactive({
    input$beta
  })
  
  sigmaDist <- reactive({
    input$sigma
  })
    
  plotPrior <- function(dist){
    
    if(dist == "normal") {
      x <- seq(-10, 10, by=.1)
      y <- dnorm(x)
      plt <- plot_ly(x = x, y = y, type = 'scatter', mode = 'lines', name = 'prior')
    }
    
    else if(dist == "gamma")
    {
      x <- seq(0, 10, by=.1)
      y <- dgamma(x,2,2)
      plt <- plot_ly(x = x, y = y,  type = 'scatter', mode = 'lines', name = 'prior')
    } 
    else if(dist == "uniform")
    {
      x <- seq(0, 10, by=.1)
      y <- dunif(x)
      plt <- plot_ly(x = x, y = y, type = 'scatter', mode = 'lines', name = 'prior')
    }
    plt
  }
  
  
  output$alphaPlot <- renderPlotly({
    plotPrior(alphaDist())
  })
  
  output$betaPlot <- renderPlotly({
    plotPrior(betaDist())
  })
  
  output$sigmaPlot <- renderPlotly({
    plotPrior(sigmaDist())
  })
  
  # Simulación
  
  ncadenas <- reactive({
    input$ncadenas
  })
  
  longitud <- reactive({
    input$longitud
  })
  
  cadena <- eventReactive(input$go, {
    
    n <- longitud()
    datos <- datos()

    x <- datos$varindep
    y <- datos$vardep
    
    mcmc <- mcmc(n, c(0,0,0), x, y)
    mcmc$chain
  })
  
  # Cadenas
  
  output$cadenaalpha<- renderPlotly({
    ncadenas <- longitud() - ncadenas()
    cadena <- cadena()
    cadena <- cadena[-(1:ncadenas) , 1]
    plot_ly(y = cadena, type = 'scatter', mode = 'lines')
  })
  
  output$cadenabeta <- renderPlotly({
    ncadenas <- longitud() - ncadenas()
    cadena <- cadena()
    cadena <- cadena[-(1:ncadenas) , 2]
    plot_ly(y = cadena, type = 'scatter', mode = 'lines')
  })
  
  output$cadenasigma <- renderPlotly({
    ncadenas <- longitud() - ncadenas()
    cadena <- cadena()
    cadena <- cadena[-(1:ncadenas) , 3]
    plot_ly(y = cadena, type = 'scatter', mode = 'lines')
  })
  
  # Histogramas 

  output$alphapostplot <- renderPlotly({
    ncadenas <- longitud() - ncadenas()
    cadena <- cadena()
    cadena <- cadena[-(1:ncadenas) , 1]
    plot_ly(x = cadena, type="histogram", name = "Posterior alpha", nbinsx = 10)
  })
  
  output$betapostplot <- renderPlotly({
    ncadenas <- longitud() - ncadenas()
    cadena <- cadena()
    cadena <- cadena[-(1:ncadenas) , 2]
    plot_ly(x=cadena, type="histogram", name = "Posterior beta", nbinsx = 10)
  })
  
  output$sigmapostplot <- renderPlotly({
    ncadenas <- longitud() - ncadenas()
    cadena <- cadena()
    cadena <- cadena[-(1:ncadenas) , 3]
    plot_ly(x=cadena, type="histogram", name = "Posterior sigma", nbinsx = 10)
  })
  
  
  # Comparación de densidad
  
  output$compalpha <- renderPlotly({
    ncadenas <- longitud() - ncadenas()
    cadena <- cadena()
    cadena <- cadena[-(1:ncadenas) , 1]
    densidad <- density(cadena)
    d1 <- densidad[[1]]
    d2 <- densidad[[2]]
    
    plotPrior(alphaDist()) %>%
      add_trace(x = d1, y = d2, type = 'scatter', mode = 'lines', name = 'posterior') %>%
      layout(yaxis2 = list(overlaying = "y", side = "right"))
    
  })
    
    output$compbeta <- renderPlotly({
      ncadenas <- longitud() - ncadenas()
      cadena <- cadena()
      cadena <- cadena[-(1:ncadenas) , 2]
      densidad <- density(cadena)
      d1 <- densidad[[1]]
      d2 <- densidad[[2]]
      
      plotPrior(betaDist()) %>%
        add_trace(x = d1, y = d2, type = 'scatter', mode = 'lines', name = 'posterior') %>%
        layout(yaxis2 = list(overlaying = "y", side = "right"))
  
  })
      
    output$compsigma <- renderPlotly({
      ncadenas <- longitud() - ncadenas()
      cadena <- cadena()
      cadena <- cadena[-(1:ncadenas) , 3]
      densidad <- density(cadena)
      d1 <- densidad[[1]]
      d2 <- densidad[[2]]
    
      plotPrior(sigmaDist()) %>%
        add_trace(x = d1, y = d2, type = 'scatter', mode = 'lines', name = 'posterior') %>%
        layout(yaxis2 = list(overlaying = "y", side = "right"))
  })
    
}
