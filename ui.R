shinyUI(pageWithSidebar(
  headerPanel('Modelo de programación lineal'),
  sidebarPanel(
    
    numericInput('clusters', 'seleccione valor de Z', 0,
                 min = 0, max = 2000)
  ),
  mainPanel(
    plotOutput('plot1')
  )
))