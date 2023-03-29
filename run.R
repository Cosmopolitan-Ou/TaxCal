
library(shiny)
library(shinyWidgets)


port <- Sys.getenv('PORT')

# source R files
source(file.path(getwd(), 'utils.R'), encoding="utf-8")
source(file.path(getwd(), 'ui.R'), encoding="utf-8")
source(file.path(getwd(), 'server'), encoding="utf-8")

# run the application 
shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)