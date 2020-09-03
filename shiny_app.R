library(shiny)
library(leaflet)

# Get working data set from source script
source("data_wrangling.R")

# User interface
ui <- fluidPage(
  fluidRow(
    h2("2019 pedestrian traffic in Melbourne CBD", align = "center"),
    column(6,
           p("Sensor location map.", br("Circle size represents pedestrian year average.")),
           div(style = "border:1px solid; color:grey", 
               leafletOutput("pedmap", height = 500)),
           div(style = "position:relative; top:30px",
               actionButton("reload_map", label = "Reset map"),
               actionButton("center_selection", label = "Fly to selected"))
    ),
    column(6,
           p("Pedestrian hourly average per day of the week.", br("Sensor: ", strong(textOutput("sensor", container = span))), 
             style = "position:relative; left:45px"),
           div(style = "border:1px solid; color:grey",
               plotOutput("ped_counts", height = 500)),
           div(style = "position:relative; left:25px; top:10px",
               selectInput("sensor_filter", label = "Choose sensor",
                           choices = unique(peds$Sensor_Name), selected = unique(peds$Sensor_Name)[1],
                           width = "95%")
           )
    )
  )
)

# Server side
server <- function(input, output, session){
  
  # Format variables
  fill_color <- "black"
  scale_factor = 100
  
  # Custom legend functions (HTML)
  # Based on https://rb.gy/tr1xcr (stack overflow)
  make_label <- function(sizes, labels) {
    paste0("<p style='display: inline-block; height: ", sizes, 
           "px; position:absolute; left:40px; margin-top:6px; font-size:13px'>",
           labels, "</p>")
  }
  
  make_symbol <- function(color, sizes, borders) {
    l_margin <- max(sizes)/2 - sizes/2
    paste0(color, "; width:", sizes, "px; height:", sizes, 
           "px; border:0px solid ", color, 
           "; border-radius:50%; position:relative; left:", l_margin, "px; margin-top:", l_margin+1.5, "px;")
  }
  
  # Get sizes and labels from dataset
  sizes <- sort(unique(sensors$low_bound)/(scale_factor*0.5))
  labels <- sort(unique(sensors$low_bound))
  
  # Create HTML legend elements
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
                title = "Year avg")
    
  })
  
  # Filter selected sensor data
  sel_sensor <- reactive({
    sensors %>% filter(sensor_name == input$sensor_filter)
  })
    
  # Render selected sensor
  observe({
    input$reload_map
    leafletProxy("pedmap", data = sel_sensor()) %>%
      removeMarker(layerId = "selection") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = ~ped_avg/scale_factor,
                       stroke = FALSE, fillColor = "red", fillOpacity = 0.8,
                       label = ~paste("Sensor:", sensor_name),
                       layerId = "selection")
  })
  
  # Fly to selected sensor
  observeEvent(input$center_selection, {
    leafletProxy("pedmap") %>% 
      flyTo(lng = sel_sensor()$longitude, lat = sel_sensor()$latitude, zoom = 17)
  })
  
  # Render line plots
  output$ped_counts <- renderPlot({
    peds %>% filter(Sensor_Name == input$sensor_filter) %>% 
      ggplot(aes(x = Time, y = avg_count)) +
      geom_line() +
      labs(y = "Average") +
      facet_wrap(~Day)
  })
  
  # Get selected sensor from map
  observe({
    map_input <- input$pedmap_marker_click
    updateSelectInput(session, inputId = "sensor_filter", selected = map_input$id)
  })
  
  # Update line plot title
  output$sensor <- renderText({input$sensor_filter})
}

# Run App
shinyApp(ui, server)