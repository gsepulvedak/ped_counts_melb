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

server <- function(input, output, session){
  
  # Initial map setup
  lng_center <- 144.961360
  lat_center <- -37.810316
  zoom_init <- 13.75
  fill_color <- "black"
  scale_factor = 150
  
  # Render map
  output$pedmap <- renderLeaflet({
    input$reload_map
    leaflet(data = sensors, options = leafletOptions(zoomSnap = 0.25, zoomDelta = 1)) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      addProviderTiles("CartoDB.Voyager") %>% 
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = ~ped_avg/scale_factor,
                 stroke = FALSE, fillColor = fill_color, fillOpacity = 0.5,
                 label = ~paste("Sensor:", sensor_name),
                 # highlightOptions = highlightOptions(fillColor = "green", bringToFront = FALSE),
                 layerId = ~sensor_name)
      
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
  
  layer_change <- eventReactive(input$pedmap_marker_mouseover, {
    filtered_data <- reactive({sensors %>% filter(sensor_name == input$pedmap_marker_mouseover$id)})
    leafletProxy("pedmap", session, data = filtered_data()) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = ~ped_avg/scale_factor,
                       stroke = FALSE, fillColor = "red", fillOpacity = 0.5,
                       label = ~paste("Sensor:", sensor_name),
                       layerId = ~sensor_name)
  })
  
  layer_delay <- layer_change %>% debounce(1000)
  
  observe({layer_delay})
  # observeEvent(input$pedmap_marker_mouseover, {
  #   print(input$pedmap_marker_mouseover)
  #   filtered_data <- reactive({sensors %>% filter(sensor_name == input$pedmap_marker_mouseover$id)})
  #   leafletProxy("pedmap", session, data = filtered_data()) %>%
  #     addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = ~ped_avg/scale_factor,
  #                      stroke = FALSE, fillColor = "red", fillOpacity = 0.5,
  #                      label = ~paste("Sensor:", sensor_name),
  #                      layerId = ~sensor_name)
  # })
}



shinyApp(ui, server)
