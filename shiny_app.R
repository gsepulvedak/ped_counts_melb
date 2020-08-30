library(shiny)
library(leaflet)

# Get working data set from source script
source("data_wrangling.R")

# user interface
# ui <- 

lng_center <- 144.961360
lat_center <- -37.810316
zoom_init <- 14.45
fill_color <- "black"
scale_factor = 15

############################################ LEGEND #####################################
# Create custom legend labels functions
# Based on https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends
make_labels <- function(sizes, labels) {
  paste0("<div style='display: inline-block;height: ", 
         # sizes, "px;margin-top: 4px;line-height: ", 
         sizes, "px;'>",
         labels, "</div>")
}

make_shapes <- function(color, sizes, borders) {
  # shapes <- gsub("circle", "50%", shapes)
  # shapes <- gsub("square", "0%", shapes)
  paste0(color, "; width:", sizes, "px; height:", sizes, 
         "px; border:3px solid ", borders, 
         "; border-radius:50%")
}

sizes <- unique(sensors$size_class)*3
label_levels <- levels(sensors$size_label)

labels <- make_labels(sizes, label_levels)
colors_shapes <- make_shapes(fill_color, sizes, fill_color)

##########################################################################################


### Map
leaflet(data = sensors) %>% 
  setView(lng = lng_center, lat = lat_center, zoom = zoom_init) %>%
  addProviderTiles("CartoDB.Voyager") %>%
  addCircles(lng = ~longitude, lat = ~latitude, radius = ~ped_avg/scale_factor,
             stroke = FALSE, fillColor = fill_color, fillOpacity = 0.5, 
             label = ~paste("Sensor:", sensor_name),
             highlightOptions = highlightOptions(fillColor = "green", bringToFront = FALSE))# %>% 
  # addLegend("bottomright", colors = colors_shapes, labels = labels)


  
### Line small multiples
filtered_sensor = "Alfred Place"
peds %>% filter(Sensor_Name == filtered_sensor) %>% 
  ggplot(aes(x = Time, y = avg_count)) +
  geom_line() +
  labs(title = "Hourly counts per day of the week", y = "Average") +
  facet_wrap(~Day)
