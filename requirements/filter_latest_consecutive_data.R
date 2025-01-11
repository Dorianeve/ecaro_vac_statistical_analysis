# Function to get the latest series of at least 5 consecutive non-missing values
filter_latest_consecutive_series <- function(data, n = 5) {
  
  # Convert Year to Date format and extract the year
  data <- data %>%
    mutate(Year = year(as.Date(Year, format = "%Y-%m-%d")))
  
  # Group by country and filter to only keep those with at least 5 consecutive non-missing values
  filtered_data <- data %>%
    group_by(Country) %>%
    filter(has_consecutive_data(Value, n)) %>%  # Keep countries with at least 5 consecutive non-missing values
    arrange(Country, Year) %>%
    mutate(
      year_diff = c(NA, diff(Year)),
      series_id = cumsum(is.na(year_diff) | year_diff > 1)  # Identify consecutive series
    ) %>%
    group_by(Country, series_id) %>%
    filter(n() >= n) %>%  # Keep only series with at least n consecutive years
    ungroup() %>%
    group_by(Country) %>%
    filter(series_id == max(series_id)) %>%  # Keep the most recent series based on series_id
    ungroup() %>%
    dplyr::select(-year_diff, -series_id)  # Remove temporary columns
  
  return(filtered_data)
}
