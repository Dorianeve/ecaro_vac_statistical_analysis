# Sex should be filtered before
# X is dataset
# Y is statistical unit
# this preps the data for maps comparison

processing_comparison <- function(x, y) {
  data <- x %>%
    filter(StatisticalUnit == y) %>%
    mutate(ISO3 = trimws(ISO3)) %>%
    group_by(Country, Year, ISO3, Indicator) %>%
    summarise(Total = sum(Value))
  
  data <- data %>%
    group_by(Country, ISO3, Indicator) %>%
    top_n(1, Year) %>%
    ungroup()
  
}
