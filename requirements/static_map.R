
# Static map function with automatic zoom
static_map <- function(x) {
  # Load country geometries (using rnaturalearth)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Merge the geometries with your dataset by ISO3 code
  world_data <- world %>%
    left_join(x, by = c("iso_a3_eh" = "ISO3"))
  
  # Filter out rows with NA values in the 'Total' column to focus on areas with data
  world_data_filtered <- world_data %>% filter(!is.na(Total))
  
  # Extract the Indicator title from the dataset
  map_title <- unique(x$Indicator)
  map_title <- ifelse(length(map_title) > 1, paste(map_title, collapse = ", "), map_title)  # Handle multiple indicators
  
  # Create a bounding box based on the filtered data
  bbox <- st_bbox(world_data_filtered)  # Calculate bounding box for data
  
  # Create the static map using ggplot2 with automatic zoom
  p <- ggplot(data = world_data) +
    geom_sf(aes(fill = Total), color = "white", size = 0.2) +  # Use geom_sf to plot country shapes
    scale_fill_gradient(
      low = "pink",  # Light red for low values
      high = "darkred",  # Dark red for high values
      na.value = "lightgray"  # Gray for missing values
    ) + 
    labs(
      title = map_title,
      fill = "Total Value"  # Legend title
    ) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +  # Set limits based on bounding box
    theme_minimal() +  # Minimal theme for clean visualization
    theme(
      panel.background = element_rect(fill = "white"),  # Set background to white
      plot.title = element_text(hjust = 0.5, size = 16),  # Center the title and set font size
      legend.position = "right",  # Position legend on the right side
      legend.background = element_rect(fill = "white")  # Set legend background to white
    )
  
  # Print the map
  print(p)
}

