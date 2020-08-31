library(shiny)
library(leaflet)

# Get working data set from source script
source("data_wrangling.R")

# User interface
ui <- fluidPage(
  fluidRow(
    column(6,
           leafletOutput("pedmap"),
           div(style = "position:relative; top:20px",
               actionButton("reload_map", label = "Reset map"))
    ),
    column(6,
           plotOutput("ped_counts"),
           div(style = "position:relative; left:35px;",
               selectInput("sensor_filter", label = "Select a sensor",
                           choices = unique(peds$Sensor_Name), selected = unique(peds$Sensor_Name)[1],
                           width = "95%")
           )
    )
  ),
  fluidRow(
    textOutput("text")
  )
)

# Server side
server <- function(input, output, session){
  
  # Initial map setup
  lng_center <- 144.961360
  lat_center <- -37.810316
  zoom_init <- 13.75
  fill_color <- "black"
  scale_factor = 100
  
  # Create custom legend functions
  # Based on https://rb.gy/tr1xcr (stack overflow)
  make_label <- function(sizes, labels) {
    paste0("<p style='display: inline-block; height: ", sizes, 
           "px; position:absolute; left:40px; margin-top:6px'>",
           labels, "</p>")
  }
  
  make_symbol <- function(color, sizes, borders) {
    l_margin <- max(sizes)/2 - sizes/2
    paste0(color, "; width:", sizes, "px; height:", sizes, 
           "px; border:0px solid ", color, 
           "; border-radius:50%; position:relative; left:", l_margin, "px; margin-top:", l_margin+1.5, "px;")
  }
  
  sizes <- sort(unique(sensors$low_bound)/(scale_factor*0.5))
  labels <- sort(unique(sensors$low_bound))
  
  legend_labels <- make_label(sizes, labels)
  legend_symbols <- make_symbol(fill_color, sizes, fill_color)
  
  # Render map
  output$pedmap <- renderLeaflet({
    input$reload_map
    leaflet(data = sensors, options = leafletOptions(zoomSnap = 0.25, zoomDelta = 1)) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      addProviderTiles("CartoDB.Voyager") %>% 
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = ~ped_avg/scale_factor,
                       stroke = FALSE, fillColor = fill_color, fillOpacity = 0.5,
                       label = ~paste("Sensor:", sensor_name),
                       layerId = ~sensor_name) %>% 
      addLegend("topleft", colors = legend_symbols, labels = legend_labels,
                title = "2019 average")
    
  })
  
  # Render line plots
  output$ped_counts <- renderPlot({
    peds %>% filter(Sensor_Name == input$sensor_filter) %>% 
      ggplot(aes(x = Time, y = avg_count)) +
      geom_line() +
      labs(title = "Hourly counts per day of the week", y = "Average") +
      facet_wrap(~Day)
  })
  
  # Get selected sensor from map
  observe({
    map_input <- input$pedmap_marker_click
    updateSelectInput(session, inputId = "sensor_filter", selected = map_input$id)
  })
}

# Run App
shinyApp(ui, server)
