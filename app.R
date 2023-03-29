library(shiny)
library(shinyWidgets)

# source R files
for (file in list.files(file.path(getwd(), 'R'))) {
  source(file.path(getwd(), 'R', file), encoding="utf-8")
}

# run the application
shinyApp(ui = ui, server = server)
