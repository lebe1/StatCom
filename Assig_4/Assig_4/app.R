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

# Convert population to numeric
df$population <- as.numeric(gsub(",", "", df$population))


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
    helpText('Welcome to our shiny app. This app is designed to help you explore the CIA World Factbook 2020. 
             You can select a variable from the dropdown menu and view the data in a table (raw data), a map, and boxplots. 
             Enjoy! '),
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
                              titlePanel("Map visualization of the selected variable"),
                              HTML("<p>The map shows the selected variable for each country. Areas in white are missing data.</p>"),
                              plotlyOutput("map", width = "100%"),
                              ),
                     tabPanel("Boxplot (overall)",
                              titlePanel("Boxplot of the selected variable"),
                              plotlyOutput("boxplot", width = "100%")
                              ),
                     tabPanel("Boxplot per continent",
                              titlePanel("Boxplot of the selected variable per continent"),
                              plotlyOutput("box_cnt", width = "100%")),
                    ),
                   )
               ),
               
               
               ),
      tabPanel("Multivariate analysis",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("variable1", "Select variable 1",
                               choices = column_names),
                   selectInput("variable2", "Select variable 2",
                               choices = column_names),
                   selectInput("scalefactor", "Select variable for sizing:",
                               choices = c("Population" = "population", "Area" = "area"),
                               selected = "population")
                   # DTOutput("dynamic2"),
                 ),
                 mainPanel(
                   plotlyOutput("scatterPlot")
                 )
               ),
      ),
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
                   colorscale='Viridis'
                   ) %>%
                  layout(
                      geo = list(
                        showframe = FALSE,
                        showcoastlines = TRUE,
                        projection = list(type = 'orthographic')
                      ))
    fig
  })
  
  output$boxplot <- renderPlotly({
    fig <- plot_ly(y = df[[input$variable]], type = "box") %>%
      layout(
        yaxis = list(title = input$variable),
        xaxis = list(title = "Aggregation over all countries")
      )
    
    fig
  })
  
  
  output$box_cnt <- renderPlotly({
    fig <- plot_ly(x = df$continent , y = df[[input$variable]], 
                   type = "box", color = df$continent) %>%
      layout(
        yaxis = list(title = input$variable),
        xaxis = list(title = "Continent")
      )
    
    fig
  })
  
  output$scatterPlot <- renderPlotly({

    # Check if the columns selected exist in the dataframe
    req(input$variable1, input$variable2, input$scalefactor)
    req(input$variable1 %in% names(df), input$variable2 %in% names(df))
    
    # Dynamically scale point size based on the user's choice
    size_by <- if (input$scalefactor == "Population") df$Population else df$Area
    
    sizeData <- df[[input$scalefactor]]
    
    # Create the plot
    plot <- plot_ly(df, x = ~get(input$variable1), y = ~get(input$variable2), 
                    type = 'scatter', mode = 'markers',
                    color = ~continent, sizes = c(10, 50),
                    marker = list(
                      size = ~sizeData,  
                      sizemode = 'diameter',
                      sizeref = 2.0 * max(sizeData, na.rm = TRUE) / (80),
                      sizemin = 5
                    ),
                    hoverinfo = 'text',
                    text = ~paste('Continent:', continent,
                                  '<br>', input$variable1, ':', get(input$variable1),
                                  '<br>', input$variable2, ':', get(input$variable2)
                                  )) %>%
      layout(title = paste("Scatterplot of", input$variable1, "vs", input$variable2),
             xaxis = list(title = input$variable1),
             yaxis = list(title = input$variable2),
             legend = list(title = list(text = "Continents")))
    
    plot
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
