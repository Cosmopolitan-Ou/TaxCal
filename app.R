
library(shiny)
library(shinyWidgets)


# source R files
source(file.path(getwd(), 'utils.R'), encoding="utf-8")
source(file.path(getwd(), 'ui.R'), encoding="utf-8")
source(file.path(getwd(), 'server.R'), encoding="utf-8")

# run the application
shinyApp(ui = ui, server = server)
