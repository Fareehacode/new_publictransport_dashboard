library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(DT)

# ---- UI ----
ui <- fluidPage(
  titlePanel("NSW Transit Demand & Flow Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Placeholder for interactive controls
      selectInput("station", "Select Station:", choices = NULL),
      dateRangeInput("dates", "Select Date Range:", start = "2025-01-01", end = Sys.Date())
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Trends", plotlyOutput("trend_plot")),
        tabPanel("Data Table", DTOutput("data_table"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Placeholder for reactive dataset
  data <- reactive({
    # To be filled after data cleaning
    tibble()
  })
  
  # Update station choices dynamically
  observe({
    updateSelectInput(session, "station", choices = c("Station A", "Station B"))
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  
  # Trend plot
  output$trend_plot <- renderPlotly({
    plot_ly()
  })
  
  # Data table
  output$data_table <- renderDT({
    datatable(data())
  })
}

# ---- Run App ----
shinyApp(ui = ui, server = server)

