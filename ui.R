library(shiny)
library(plotly)
library(ggplot2movies)
library(DT)
library(Rcpp)
library(RcppArmadillo)

sourceCpp("/home/thalia/Maestría/Estadística_Computacional/compstat2016/mcmc.cpp")

ui <- (fluidPage (
  
  # Application title
  titlePanel("Estadística Computacional"),
  
  # Sidebar con opciones de tareas
  sidebarLayout(
    sidebarPanel(
      radioButtons("tarea", label="Escoge tarea",
                   choices = c(
                     "Funcion Inversa"="FunInv",
                     "Montecarlo"="MC",
                     "MCMC" = "MCMC"
                   ),
                   selected="FunInv"
      )
    ),

    
    mainPanel(
      
  # Tarea de función inversa
      
        conditionalPanel(
          condition="input.tarea=='FunInv'",
          h2("Funcion Inversa"),
          
        column(3, sliderInput(inputId = "lambda", value = 0.5, label = "Escoge el valor de lambda", max = 1, min = 0, step = 0.001)),
        column(3, sliderInput(inputId = "bin", value = 10, label = "Escoge el número de bins", min = 10, max = 100, step = 10)),
        column(3, sliderInput(inputId = "sim", value = 1000, label = "Escoge el número de simulaciones", min=100, max=10000, step = 500, animate = TRUE)),
        
        column(10, plotOutput(outputId = "hist")),
        column(7, textOutput(outputId = "text")),
        column(10, plotlyOutput("comp"))
        ),
      
  # Tarea de Montecarlo
  
       conditionalPanel(
         condition="input.tarea=='MC'",
         h2("MonteCarlo"),
         
         column(5,textInput("funcion", label = "Escribe la función que quieres integrar", value="function(x)  {2*(x**2)}")),
         column(5,sliderInput(inputId = "sim2", value = 1000, label = "Elige el número de simulaciones", min=100, max=10000, step = 100)),
         column(5,numericInput("inferior", label = "Escribe el límite inferior de la integral", value=0, min=0)),
         column(5,sliderInput(inputId = "conf", value = 0.95, label = "Selecciona el intervalo de confianza", min=0.90, max=0.99, step = 0.05)),
         column(5,numericInput("superior", label = "Escribe el límite superior de la integral", value=10)),
         
         column(7, textOutput(outputId = "area")),
         column(10, plotlyOutput(outputId = "areaplot"))
       ),
  
  # Tarea de MCMC
  
       conditionalPanel(
         condition="input.tarea=='MCMC'",
         h2("MCMC"),
        
         titlePanel("Movies DataTable"),
           
         column(4, selectInput("vardep", "Selecciona la variable dependiente",
                   c(names(movies)), selected = 'length')),
         column(4, selectInput("varindep", "Selecciona la variable independiente",
                   c(names(movies)), selected = 'budget')),

         fluidRow(
           DT::dataTableOutput("table")),
         
         column(10, plotlyOutput(outputId = "scatterplot")),
         
         column(4, selectInput("alpha", "Seleciona la adistribución a priori para alpha:", c("normal","gamma","uniform"))),
         column(4, selectInput("beta", "Seleciona la adistribución a priori para beta:", c("normal","gamma","uniform"))),
         column(4, selectInput("sigma", "Seleciona la adistribución a priori para sigma:", c("normal","gamma","uniform"))),
         
         column(4, plotlyOutput(outputId = "alphaPlot")),
         column(4, plotlyOutput(outputId = "betaPlot")),
         column(4, plotlyOutput(outputId = "sigmaPlot")),
         
         column(5, sliderInput(inputId = "longitud", value = 100, label = "Escoge la longitud de las cadenas", max = 10000, min = 100, step = 1)),
         column(5, sliderInput(inputId = "ncadenas", value = 100, label = "Escoge el número de cadenas", max = 1000, min = 100, step = 1)),
         column(10, actionButton("go", "Go")),
         
         column(4, plotlyOutput((outputId = "cadenaalpha"))),
         column(4, plotlyOutput((outputId = "cadenabeta"))),
         column(4, plotlyOutput((outputId = "cadenasigma"))),
         
         column(4, plotlyOutput((outputId = "alphapostplot"))),
         column(4, plotlyOutput((outputId = "betapostplot"))),
         column(4, plotlyOutput((outputId = "sigmapostplot"))),
         
         column(4, plotlyOutput((outputId = "compalpha"))),
         column(4, plotlyOutput((outputId = "compbeta"))),
         column(4, plotlyOutput((outputId = "compsigma")))
       )
       )
    )
  )
)

