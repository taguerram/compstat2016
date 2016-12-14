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
  
  output$hist <- renderPlot({
    hist(Finv(), breaks = input$bin)})
  
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
  
  
  output$area <- renderText({
    
    paste("El área bajo la curva es", FMC())
  })

  
  
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
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- movies[which(movies$budget != 'NA'), ]
    data
  }))
  
  
  
  output$scatterplot <- renderPlotly ({
    
    data <- movies[which(movies$budget != 'NA'), ]
    
    varindep <- data[ ,which(names(data) == input$varindep)]
    vardep <- data[ ,which(names(data) == input$vardep)]
    
    datos <- cbind(varindep, vardep)
    nombres <- c('varindep', 'vardep')
    colnames(datos) <- nombres
    
    plot_ly(datos, x = datos$varindep, y = datos$vardep, type = 'scatter', mode = 'markers')
  })  
  
}
