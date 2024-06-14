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
library(plotly)
library(ggplot2)
library(dplyr)

df <- fromJSON("../data_cia.json")

column_names <- c("Median Age" = "median_age", 
                  "Youth Unemployment Rate" = "youth_unempl_rate", 
                  "Net Migration Rate" = "net_migr_rate",
                  "Population Growth Rate" = "pop_growth_rate", 
                  "Electricity Fossil Fuel" = "electricity_fossil_fuel", 
                  "Life Expectancy" = "life_expectancy")

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
                               choices = column_names),
                   actionButton("raw", "View raw data", icon = icon("th", lib="glyphicon")),
                   DTOutput("dynamic"),
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Map",
                              titlePanel("Beatutiful title"),
                              plotlyOutput("map", width = "100%")
                              ),
                     tabPanel("Boxplot (overall)",
                              plotlyOutput("boxplot", width = "100%")
                              ),
                     tabPanel("Boxplot per continent",
                              plotlyOutput("box_cnt", width = "100%")),
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
      df_subset <- df[c('country', 'continent', input$variable)]
      colnames(df_subset) <- c("Country", "Continent", 
                               names(column_names)[column_names == input$variable])
      df_subset
    }, options = list(pageLength = 15,
                      scrollX = TRUE))
    
  })
  
  output$map <- renderPlotly({

    fig <- plot_ly(df, type='choropleth', locations=df$ISO3, 
                   z=df[[input$variable]], text = df$country,
                   colorscale='Viridis')
    fig
  })
  
  output$boxplot <- renderPlotly({
    fig <- plot_ly(y = df[[input$variable]], type = "box")
    
    fig
  })
  
  output$box_cnt <- renderPlotly({
    fig <- plot_ly(x = df$continent , y = df[[input$variable]], 
                   type = "box", color = df$continent)
    
    fig
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
