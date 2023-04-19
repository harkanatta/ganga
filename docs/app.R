# Define app title and GitHub link
title <- "My Shiny App"
github <- "https://github.com/harkanatta/ganga"

library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(sf)


# Load data
BBS_linur <- st_read(dsn = "BBS_linur.gpkg")
df <- read.table("df_filtered.txt", sep = "\t", header = T)
df$timestamps <- as.POSIXct(df$timestamps)
#lesa inn punktana (VP) og sniðin (BBS)
wp <- read.table("waypoints.txt", sep = "\t", header = T)
# Convert wp to sf object with point geometry
wp_sf <- st_as_sf(wp, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Get unique dates in data
date_options <- unique(df$timestamps)
date_options <- format(date_options, "%Y/%m/%d")

# Define UI
ui <- fluidPage(  tags$head(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Roboto:400,700&display=swap")),
                  tags$style(type='text/css', "
          body {
            font-family: 'Roboto', sans-serif;
            background-color: #f2f2f2;
          }
          .btn-primary {
            background-color: #1abc9c;
            border-color: #1abc9c;
          }
          .sidebar {
            box-shadow: 0 0 10px rgba(0,0,0,0.3);
          }
          "),
  tags$style(type="text/css","#map { height: calc(100vh - 80px) !important; width:100%; }"
                  ),titlePanel("GPS Data Explorer"),
  theme = shinytheme("united"), # Add this line
  sidebarLayout(
    sidebarPanel(
      selectInput("date", "Select a date:", choices = date_options),
      sliderInput("elevation_range", "Elevation Range:",
                  min = min(df$elevation), max = max(df$elevation),
                  value = c(min(df$elevation), max(df$elevation)), step = 0.1),
      actionButton("reset_filters", "Reset Filters")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
# Define server logic
server <- function(input, output, session) {
  
  # Filter data based on selected date and elevation range
  filtered_data <- reactive({
    df %>%
      filter(format(timestamps, "%Y/%m/%d") == input$date,
             elevation >= input$elevation_range[1],
             elevation <= input$elevation_range[2])
  })
  
  # Calculate total distance
  total_distance <- reactive({
    df_filtered <- filtered_data()
    if (nrow(df_filtered) > 1) {
      df_filtered$dist <- c(0, geosphere::distHaversine(df_filtered[, c("lon", "lat")][-1, ], df_filtered[, c("lon", "lat")][-nrow(df_filtered), ]))
      sum(df_filtered$dist)
    } else {
      0
    }
  })
  
  # Create leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>% 
      addPolylines(data = filtered_data(), ~lon, ~lat, weight = 2, color = "yellow") %>% 
      addPolylines(
        data = BBS_linur, 
        weight = 2,  # line weight
        color = "red"  # line color
      ) %>% 
      addStaticLabels(data = BBS_linur, label = BBS_linur$Nafn, style = list("color" = "white", "font-weight" = "bold")) %>% 
      addCircleMarkers(data = wp_sf[wp_sf$dataset=="VP",]) %>% 
      addStaticLabels(data = wp_sf[wp_sf$dataset=="VP",], label = wp_sf$name[wp_sf$dataset=="VP"], style = list("color" = "red", "font-weight" = "bold")) %>% 
      addScaleBar(position = "bottomright") %>% 
      addControl(paste("Total distance:", round(total_distance()/1000, 2), "km"), position = "bottomleft")
  })
  
  # Reset filters when the button is clicked
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "date", selected = NULL)
    updateSliderInput(session, "elevation_range",
                      value = c(min(df$elevation), max(df$elevation)))
  })
  
}

# Run the
# Wrap shinyApp function in runApp function to make it compatible with GitHub Pages
runApp(shinyApp(ui, server), launch.browser = FALSE)
