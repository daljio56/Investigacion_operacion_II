library(shiny)
source("ui_clase_7.R")
source("server_clase_7.R")
runApp(list(
  ui = pg,
  server = f
))
