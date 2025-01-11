rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")


icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

mics <- icvac %>% filter(Indicator == "Violent discipline - Any physical punishment")

mics %>% filter(is.na(Year))

mics %<>%
  mutate(AgeGroup = case_when(
    AgeGroup == "9-May" ~ "5-9",
    AgeGroup == "14-Oct" ~ "10-14",
    TRUE ~ AgeGroup
  ))

mics$AgeGroup <- factor(
  mics$AgeGroup,
  levels = c("<1", "2", "3", "4", "5-9", "10-14", "15-17"))

mics %<>%
  mutate(Value = case_when(
    Comments == "Not available/applicable" ~ NA,
    TRUE ~ Value
  ))

mics <- mics %>%
  filter(!grepl("roma", Source, ignore.case = TRUE) &
           !is.na(Year)) %>%  # Exclude rows with "roma" in Source
  group_by(Country) %>%                                  # Group by Country
  filter(Year == max(Year)) %>%                          # Keep rows with the latest Year
  ungroup()     

mics %<>%
  distinct()

# Define the columns to filter and use for visualization
filter_columns <- c("Sex", "AgeGroup", "WealthQuintile", "IDPstatus", "SettlementType")

# Specify the output folder
output_folder <- "analysis/cat2_MICS/"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)  # Create the folder if it doesn't exist
}

# Initialize an empty data frame for STD results
std_results <- data.frame(Country = character(), Category = character(), Column = character(), STD = numeric(), stringsAsFactors = FALSE)

mean_results <- data.frame(Category = character(), Column = character(), Mean = numeric(), stringsAsFactors = FALSE)

# Generate graphs for each country
unique_countries <- unique(mics$Country)  # Get a list of unique countries

for (country in unique_countries) {
  # Filter data for the current country
  country_data <- mics %>%
    filter(Country == country)
  
  for (col in filter_columns) {
    # Filter out rows with "Total" or NA for the current column
    filtered_data <- country_data %>%
      filter(!.data[[col]] %in% c("Total", NA))
    
    # Skip if no valid data is available for the current column
    if (nrow(filtered_data) == 0) {
      next
    }
    
    std_data <- filtered_data %>%
      group_by(Country) %>%  # Only group by Country
      summarise(
        STD = sd(Value, na.rm = TRUE),               # Calculate STD for the current country
        .groups = "drop"
      ) %>%
      mutate(
        Category = col                              # Assign the current column name as Category
      )
    
    # Append the results to the STD results data frame
    std_results <- bind_rows(std_results, std_data)
    
    
    # Dynamically create the bar chart using the Value column
    plot <- ggplot(filtered_data, aes(x = .data[[col]], y = Value, fill = .data[[col]])) +
      geom_bar(stat = "identity", position = "dodge") +  # Use stat = "identity" to plot actual values
      labs(
        title = paste("Violent discipline - Any physical punishment for", country),
        x = col,  # Dynamic label based on the current column
        y = "Rate",  # Label for y-axis
        fill = col  # Dynamic legend title
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
    
    # Save the plot
    ggsave(
      filename = paste0(output_folder, "Violent_discipline_", country, "_", col, ".png"),
      plot = plot,
      width = 8, height = 6
    )
  }
}

std_results %<>%
  dplyr::select(-Column) %>%
  pivot_wider(names_from = Category, values_from = STD)
# Save the STD results to a CSV file in the output folder
write.csv(std_results, paste0(output_folder, "std_results.csv"), row.names = FALSE)

mics %>%
  group_by(Country, Sex, AgeGroup, WealthQuintile, IDPstatus, SettlementType) %>%
  summarize(Total = mean(Value))


# Assuming `filter_columns` is a vector of column names
filter_columns <- c("Sex", "AgeGroup", "WealthQuintile", "IDPstatus", "SettlementType")

# Initialize an empty list to store results
results_list <- list()

# Loop through each column in filter_columns
for (col in filter_columns) {
  # Process the data for the current column
  temp_result <- mics %>%
    filter(!is.na(!!sym(col))) %>%            # Filter non-NA values for the column
    group_by(Country, !!sym(col)) %>%        # Group by Country and the column
    summarise(Total = mean(Value, na.rm = TRUE), .groups = "drop") %>% # Calculate mean
    pivot_wider(names_from = !!sym(col), values_from = Total) %>% # Pivot to wide format
    mutate(Variable = col)                   # Add a column to track the variable name
  
  # Append the result to the list
  results_list[[col]] <- temp_result
}

# Combine all results into a single data frame
final_results <- bind_rows(results_list)

# View the final results
print(final_results)

# Optionally save to a CSV file
write.csv(final_results, paste0(output_folder, "country_comparison.csv"), row.names = FALSE)


# Initialize an empty list to store results
results_list <- list()

# Loop through each column in filter_columns
for (col in filter_columns) {
  # Process the data for the current column
  temp_result <- mics %>%
    filter(!is.na(!!sym(col))) %>%            # Filter non-NA values for the column
    group_by(!!sym(col)) %>%        # Group by Country and the column
    summarise(Total = mean(Value, na.rm = TRUE), .groups = "drop") %>% # Calculate mean
    pivot_wider(names_from = !!sym(col), values_from = Total) %>% # Pivot to wide format
    mutate(Variable = col)                   # Add a column to track the variable name
  
  # Append the result to the list
  results_list[[col]] <- temp_result
}

# Combine all results into a single data frame
final_results <- bind_rows(results_list)

# View the final results
print(final_results)

# Optionally save to a CSV file
write.csv(final_results, paste0(output_folder, "categories_comparison.csv"), row.names = FALSE)


mics %<>% filter(Sex == "Total")
data <- processing_comparison(mics, "Rate")
p <- static_map(data)
ggsave(filename = "analysis/cat2_MICS/static_map_with_auto_zoom_adapted_frame.png",
       plot = p, width = 12, height = 12)

