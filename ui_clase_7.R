library(plotly)
pg<-fluidPage(

  h2("Cortes de Gomory"),
  fluidRow(
    plotlyOutput('plot1'),
    numericInput('clusters', 'selecione el alfa', 0,min = 0, max = 1)
    
  )
)