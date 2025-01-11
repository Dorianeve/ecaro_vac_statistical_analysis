filter_latest_consecutive_series_layertwo <- function(data, n = 5) {
  
  # Ensure Year is treated as a Date value if it's not already
  data <- data %>%
    mutate(Year = as.Date(Year))  # Convert Year to Date format if needed
  
  # Arrange by Country and Year to ensure chronological order
  data <- data %>%
    arrange(Country, Year)
  
  # Step 1: Create a helper column to identify consecutive series of non-missing values
  data <- data %>%
    group_by(Country) %>%
    mutate(
      is_missing = is.na(Value),
      
      # Define breaks in series based on missing values or large year gaps
      group_break = cumsum(is_missing | (c(0, diff(as.numeric(Year))) > 1)),
      
      # Create a unique series ID for each segment based on non-missing values
      series_id = cumsum(!is_missing & (c(0, diff(as.numeric(Year))) <= 1))
    )
  
  # Step 2: Group by Country and series_id to identify valid series
  series_counts <- data %>%
    group_by(Country, series_id) %>%
    summarize(
      Consecutive_Non_Missing = sum(!is.na(Value)),
      Latest_Year = max(Year),  # Capture the latest year within each series
      .groups = 'drop'
    )
  
  # Step 3: Merge the consecutive counts with the original data
  data <- data %>%
    left_join(series_counts, by = c("Country", "series_id"))
  
  # Step 4: Filter to keep only series with at least `n` consecutive non-NA values
  data <- data %>%
    filter(Consecutive_Non_Missing >= n)
  
  # Step 5: Identify the latest series ID based on the most recent year for each country
  latest_series_ids <- data %>%
    group_by(Country) %>%
    filter(Latest_Year == max(Latest_Year)) %>%
    summarize(latest_series_id = max(series_id), .groups = 'drop')
  
  # Step 6: Filter the data to keep only the latest series for each country
  data <- data %>%
    inner_join(latest_series_ids, by = c("Country", "series_id" = "latest_series_id")) %>%
    ungroup()
  
  # Remove temporary columns
  data <- data %>%
    dplyr::select(-group_break, -series_id, -is_missing, -Consecutive_Non_Missing, -Latest_Year)
  
  print("Final Filtered Data:")
  print(data)
  
  return(data)
}

  