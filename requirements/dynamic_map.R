dynamic_map <- function(x) {
  # Load country geometries (you can use rnaturalearth for demonstration)
  # library(rnaturalearth)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Merge the geometries with your dataset by ISO3 code
  world_data <- world %>%
    left_join(x, by = c("iso_a3" = "ISO3"))
  
  # Create a color palette for the 'Total' column, assigning very light gray for NA values
  pal <- colorNumeric(palette = "Blues", domain = world_data$Total, na.color = "#D3D3D3")  # Very light gray for NA
  
  # Extract the Indicator title from the dataset
  map_title <- unique(x$Indicator)
  map_title <- ifelse(length(map_title) > 1, paste(map_title, collapse = ", "), map_title)  # Handle multiple indicators
  
  # Create the interactive leaflet map without base tiles
  leaflet(world_data, options = leafletOptions(background = "white")) %>%  # Set water to white
    # Add polygons for country geometries
    addPolygons(
      fillColor = ~ifelse(is.na(Total), "#D3D3D3", pal(Total)),  # Very light gray for NA values
      weight = 0.4,
      opacity = 1,
      color = "white",  # White border color for both selected and non-selected countries
      fillOpacity = 1,
      highlightOptions = highlightOptions(
        weight = 1,
        color = "white",  # White border on hover
        fillOpacity = 1,
        bringToFront = TRUE
      ),
      label = ~ifelse(is.na(Total), 
                      paste0("Country: ", name, " - No Data"),  # Label for countries with no data
                      paste0("Country: ", name, " - Total: ", Total, " - Year: ", Year))  # Label with available data
    ) %>%
    # Add a legend for the color scale
    addLegend(pal = pal, values = ~Total, opacity = 0.7, title = "Total Value",
              position = "bottomright") %>%
    # Add a title to the map, sourced from the 'Indicator' column
    addControl(
      html = paste0("<h4>", map_title, "</h4>"),
      position = "topright"
    )
}