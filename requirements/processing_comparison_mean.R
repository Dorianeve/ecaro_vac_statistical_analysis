processing_comparison_mean <- function(x, y) {
  data <- x %>%
    filter(StatisticalUnit == y) %>%
    mutate(ISO3 = trimws(ISO3)) %>%
    group_by(Country, Year, ISO3, Indicator) %>%
    summarise(Total = mean(Value, na.rm = TRUE))
  
  data <- data %>%
    group_by(Country, ISO3, Indicator) %>%
    top_n(1, Year) %>%
    ungroup()
  
}