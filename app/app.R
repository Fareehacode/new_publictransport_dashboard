
library(shiny)
library(tidyverse)
library(readr)
library(plotly)
library(leaflet)
library(DT)
library(lubridate)
library(bslib)
library(scales)


time_trends <- read_csv("../data/processed/time_trends.csv", show_col_types = FALSE)
stop_taps   <- read_csv("../data/processed/stop_taps.csv", show_col_types = FALSE)
stop_locations <- read_csv("../data/raw/stop_locations.csv", show_col_types = FALSE)


time_trends <- time_trends %>%
  mutate(Date = as.Date(Date))

stop_taps <- stop_taps %>%
  mutate(Date = as.Date(Date))


time_trends <- time_trends %>%
  mutate(
    weekday  = wday(Date, label = TRUE, week_start = 1),
    day_type = if_else(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday")
  )

stop_taps <- stop_taps %>%
  mutate(
    weekday  = wday(Date, label = TRUE, week_start = 1),
    day_type = if_else(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday")
  )


stop_taps <- stop_taps %>%
  left_join(stop_locations, by = "stop") %>%
  filter(!is.na(lat), !is.na(lon))

if (nrow(stop_taps) == 0) {
  stop("No matched stops with coordinates in stop_taps. Check stop_locations.csv.")
}

all_modes <- sort(unique(time_trends$mode))
all_modes <- all_modes[all_modes != "unknown"]
min_date <- min(time_trends$Date, na.rm = TRUE)
max_date <- max(time_trends$Date, na.rm = TRUE)

ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "darkly"),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #121212 !important; color: #e0e0e0 !important; }

      .top-bar {
        padding: 14px 20px;
        background: #1e1e1e;
        border-bottom: 1px solid #333;
        margin-bottom: 15px;
      }

      .kpi-card {
        background: #1c1c1c;
        padding: 15px;
        border-radius: 12px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.7);
        margin-bottom: 15px;
        text-align: center;
        color: #ffffff;
      }

      .kpi-card h6 {
        text-transform: uppercase;
        font-size: 11px;
        color: #9e9e9e;
        margin-bottom: 6px;
      }

      .kpi-card h3 {
        font-size: 22px;
        margin: 0;
        color: #ffffff;
      }

      .kpi-card p {
        font-size: 11px;
        color: #b0b0b0;
        margin: 0;
      }

      .well {
        background-color: #1c1c1c !important;
        border: 1px solid #333 !important;
        color: #e0e0e0 !important;
      }

      .dataTables_wrapper {
        color: #ffffff !important;
      }

      table.dataTable tbody tr {
        background-color: #1a1a1a !important;
        color: #ffffff !important;
      }
    "))
  ),
  
  
  div(
    class = "top-bar",
    h3("NSW Public Transport Demand Dashboard", style = "margin:0;color:#eee;"),
    p(
      "This dashboard visualises opal tap - on and tap - off activity linked with GTFS stop locations across NSW, offering an interactive overview of passenger demand across stops, stations and transport modes. It brings together multiple insights like bubble maps, time - series plots, top - stop rankings and a detailed data table that helps to provide a comprehensive understanding of travel patterns across the network. Using the filters, you can explore specific modes, tap types, weekdays or weekends and custom date ranges. The bubble map highlights the busiest locations geographically, the top stops/stations bar chart compares major stops, the time - series reveals how demand shifts over time and the table allows deeper examination of individual records. Together, these tools help you uncover usage patterns, identify peak periods and detect pressure points across the NSW public transport system.",
      style = "margin:0;color:#bbb;"
    )
  ),
  
  
  fluidRow(
    column(
      3,
      div(
        class = "kpi-card",
        h6("Total taps"),
        h3(textOutput("kpi_total")),
        p("Across selection")
      )
    ),
    column(
      3,
      div(
        class = "kpi-card",
        h6("Avg daily taps"),
        h3(textOutput("kpi_avg")),
        p("Mean per active day")
      )
    ),
    column(
      3,
      div(
        class = "kpi-card",
        h6("Peak day"),
        h3(textOutput("kpi_peak_day")),
        p(textOutput("kpi_peak_detail"))
      )
    ),
    column(
      3,
      div(
        class = "kpi-card",
        h6("Active stops"),
        h3(textOutput("kpi_unique_stops")),
        p("Stations & stops")
      )
    )
  ),
  
  
  fluidRow(
    column(
      3,
      wellPanel(
        selectInput(
          "mode", "Mode",
          choices = c("All" = "All", all_modes),
          selected = "All"
        ),
        radioButtons(
          "taptype", "Tap type",
          choices = c("Both", "Tap On", "Tap Off"),
          selected = "Both"
        ),
        radioButtons(
          "daytype", "Day type",
          choices = c("All", "Weekday", "Weekend"),
          selected = "All"
        ),
        dateRangeInput(
          "daterange", "Date range",
          start = min_date,
          end   = max_date,
          min   = min_date,
          max   = max_date,
          startview = "month"
        )
      )
    ),
    
    column(
      6,
      h4("Stop / Station Bubble Map", style = "color:#ffffff;"),
      leafletOutput("map", height = "400px")
    ),
    
    column(
      3,
      h4("Top Stops / Stations", style = "color:#ffffff;"),
      plotlyOutput("top10", height = "400px")
    )
  ),
  
  
  fluidRow(
    column(
      12,
      h4("Tap Demand Over Time", style = "color:#ffffff;"),
      plotlyOutput("timeseries", height = "320px")
    )
  ),
  
  
  fluidRow(
    column(
      12,
      h4("Detailed Stop-Level Data", style = "color:#ffffff;"),
      DTOutput("table")
    )
  )
)


server <- function(input, output, session) {
  
  
  filtered_trends <- reactive({
    df <- time_trends
    
    if (input$mode != "All") {
      df <- df %>% filter(mode == input$mode)
    }
    
    df <- df %>%
      filter(
        Date >= input$daterange[1],
        Date <= input$daterange[2]
      )
    
    if (input$taptype != "Both") {
      df <- df %>% filter(tap_type == input$taptype)
    }
    
    if (input$daytype != "All") {
      df <- df %>% filter(day_type == input$daytype)
    }
    
    df
  })
  
  
  filtered_stops <- reactive({
    df <- stop_taps
    
    if (input$mode != "All") {
      df <- df %>% filter(mode == input$mode)
    }
    
    df <- df %>%
      filter(
        Date >= input$daterange[1],
        Date <= input$daterange[2]
      )
    
    if (input$taptype != "Both") {
      df <- df %>% filter(tap_type == input$taptype)
    }
    
    if (input$daytype != "All") {
      df <- df %>% filter(day_type == input$daytype)
    }
    
    df %>% filter(!is.na(lat), !is.na(lon))
  })

  output$kpi_total <- renderText({
    df <- filtered_trends()
    comma(sum(df$TotalTaps, na.rm = TRUE))
  })
  
  output$kpi_avg <- renderText({
    df <- filtered_trends()
    if (is.null(df) || nrow(df) == 0) return("0")
    ndays <- max(1, n_distinct(df$Date))
    avg <- sum(df$TotalTaps, na.rm = TRUE) / ndays
    comma(round(avg))
  })
  
  output$kpi_peak_day <- renderText({
    df <- filtered_trends()
    
    if (is.null(df) || nrow(df) == 0) return("—")
    
    by_day <- df %>%
      filter(!is.na(Date)) %>%
      group_by(Date) %>%
      summarise(Total = sum(TotalTaps, na.rm = TRUE), .groups = "drop")
    
    if (nrow(by_day) == 0) return("—")
    
    peak_idx  <- which.max(by_day$Total)
    peak_date <- by_day$Date[peak_idx]
    
    format(peak_date, "%d %b %Y")
  })
  
  output$kpi_peak_detail <- renderText({
    df <- filtered_trends()
    
    if (is.null(df) || nrow(df) == 0) return("No data in selection")
    
    by_day <- df %>%
      filter(!is.na(Date)) %>%
      group_by(Date) %>%
      summarise(Total = sum(TotalTaps, na.rm = TRUE), .groups = "drop")
    
    if (nrow(by_day) == 0) return("No data in selection")
    
    peak_idx   <- which.max(by_day$Total)
    peak_total <- by_day$Total[peak_idx]
    
    paste("Taps:", comma(peak_total))
  })
  
  output$kpi_unique_stops <- renderText({
    df <- filtered_stops()
    comma(n_distinct(df$stop))
  })
  
  
  output$map <- renderLeaflet({
    df <- filtered_stops() %>%
      group_by(stop, lat, lon) %>%
      summarise(Total = sum(TotalTaps, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df) == 0) {
      # Empty map if no data
      return(
        leaflet() %>%
          addTiles()
      )
    }
    
    pal <- colorNumeric("YlOrRd", df$Total)
    
    leaflet(df) %>%
      addTiles() %>%  
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~pmax(4, Total / max(df$Total) * 20),
        fillColor = ~pal(Total),
        fillOpacity = 0.8,
        stroke = FALSE,
        label = ~paste0(stop, ": ", comma(Total), " taps")
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = df$Total,
        title = "Tap volume"
      )
  })
  
  
  output$top10 <- renderPlotly({
    df <- filtered_stops() %>%
      group_by(stop) %>%
      summarise(Total = sum(TotalTaps, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      slice_head(n = 10)
    
    if (nrow(df) == 0) return(NULL)
    
    plot_ly(
      df,
      x = ~Total,
      y = ~reorder(stop, Total),
      type = "bar",
      orientation = "h",
      marker = list(color = "orange"),
      hovertemplate = "%{y}<br>%{x} taps<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Total taps"),
        yaxis = list(title = "Stop / Station")
      )
  })
  
  
  output$timeseries <- renderPlotly({
    df <- filtered_trends()
    
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df <- df %>%
      group_by(Date, tap_type) %>%
      summarise(TotalTaps = sum(TotalTaps, na.rm = TRUE), .groups = "drop")
    
    plot_ly(
      df,
      x = ~Date,
      y = ~TotalTaps,
      color = ~tap_type,
      type = "scatter",
      mode = "lines",
      stackgroup = "one",
      hovertemplate = "Date: %{x}<br>Type: %{color}<br>Taps: %{y}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Total taps"),
        legend = list(title = list(text = "Tap type"))
      )
  })
  
  
  output$table <- renderDT({
    df <- filtered_stops() %>%
      arrange(desc(TotalTaps)) %>%
      select(stop, Date, tap_type, TotalTaps)
    
    datatable(
      df,
      options = list(pageLength = 20),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
