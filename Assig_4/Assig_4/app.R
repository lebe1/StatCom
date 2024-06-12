#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(jsonlite)
library(DT)

df <- fromJSON("../data_cia.json")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CIA World Factbook 2020"),
    helpText('This data '),
    tabsetPanel(
      tabPanel("Univariate analysis",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("variable", "Select a variable",
                               choices = c("median age", 
                                           "youth unemployment rate", 
                                           "net migration rate", 
                                           "population growth rate", 
                                           "electricity fossil fuel", 
                                           "life expectancy")),
                   actionButton("raw", "View raw data", icon = icon("th", lib="glyphicon")),
                   DTOutput("dynamic"),
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Map",
                              titlePanel("Beatutiful title"),
                              ),
                     tabPanel("Boxplot (overall)"),
                     tabPanel("Boxplot per continent"),
                    ),
                   )
               ),
               
               
               ),
      tabPanel("Multivariate analysis"),
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$raw, {
    output$dynamic <- renderDT({
      df
    }, options = list)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
