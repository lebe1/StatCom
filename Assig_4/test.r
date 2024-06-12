library(shiny)
library(jsonlite)

ui <- fluidPage(
  titlePanel("CIA World Factbook 2020"),
  p("Welcome to the CIA World Factbook 2020"),
  tabsetPanel(
    tabPanel("Map",
             titlePanel("Beatutiful title"),
             
             fileInput("file", "Data", buttonLabel = "Upload..."),
             textInput("delim", "Delimiter (leave blank to guess)", ""),
             numericInput("skip", "Rows to skip", 0, min = 0),
             numericInput("rows", "Rows to preview", 10, min = 1)
    ),
    tabPanel("Boxplot (overall)"),
    tabPanel("Boxplot per continent")
  )
)


server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)
