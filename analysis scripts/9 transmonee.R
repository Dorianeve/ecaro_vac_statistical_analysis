rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")
source("requirements/filter_latest_consecutive_data.R")


# Female / Male / Total
# Count
# 0 to 17 and aggregated 0-17

icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

cat9 <- icvac %>% filter(ICVACCategory == "Other acts of violence against a child not elsewhere classified")

# TRENDS: counts ----
# selection on consecutive 5 years at least
# trend of all the available latest data
# summing data M+F
# single lines with breaks to get country trend
# aggregated scatter to get country outliers

tm_cwd <- cat9 %>% filter(Indicator == "Number of child victims of violence with disabilities (0-17 years) registered by child/social welfare authorities during the year"
                          & Sex == "Total" & AgeGroup == "0-17") 
tm_edu <- cat9 %>% filter(grepl("education", Indicator, ignore.case = TRUE)
                & Sex == "Total" & AgeGroup == "0-17") 
tm_health <- cat9 %>% filter(grepl("healthcare", Indicator, ignore.case = TRUE)
                & Sex == "Total" & AgeGroup == "0-17") 
tm_police <- cat9 %>% filter(Indicator == "Number of child victims of crime (0-17 years) registered by the police during the year"
                & Sex == "Total" & AgeGroup == "0-17") 
tm_welfare <- cat9 %>% filter(Indicator == "Number of child victims of violence (0-17 years) registered by child/social welfare authorities during the year"
                & Sex == "Total" & AgeGroup == "0-17") 

# Define the list of dataframes for each indicator and corresponding names
tm <- list(tm_cwd = tm_cwd, tm_edu = tm_edu, tm_health = tm_health, tm_police = tm_police, tm_welfare = tm_welfare)

# Loop through each element in the `tm` list
for (indicator_name in names(tm)) {
  current_data <- tm[[indicator_name]]
  
  # Create a subfolder for each indicator
  dir.create(paste0("analysis/cat9_tm/trends/", indicator_name, "/"), recursive = TRUE, showWarnings = FALSE)
  
  # Filter countries with at least 5 consecutive non-NA data points
  tab1 <- current_data %>%
    group_by(Country, Year) %>%
    summarise(Value = sum(Value)) %>%
    ungroup()
  
  # Convert 'Year' to Date object
  tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"), format = "%Y-%m-%d")
  
  tab1 <- filter_latest_consecutive_series(tab1)
  
  tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))
  # Create a full sequence of years (2010-2023) for each country
  full_years <- expand.grid(
    Country = unique(tab1$Country),
    Year = seq(as.Date("2010-01-01"), as.Date("2023-01-01"), by = "year")
  )
  
  # Merge the full sequence of years with the original dataset
  tab1 <- full_years %>%
    left_join(tab1, by = c("Country", "Year")) %>%
    filter(!is.na(Value) & is.finite(Value))  # Remove missing values
  
  # Get the unique list of countries
  unique_countries <- unique(tab1$Country)
  
  # Loop through each country and save the time series plot
  for (country in unique_countries) {
    country_data <- tab1 %>% filter(Country == country)
    
    # Create a time series plot with a trendline and colorful line
    p <- p <- ggplot(country_data, aes(x = Year, y = Value)) +
      geom_line(aes(color = Country), size = 1.2, na.rm = FALSE) + # Remove na.rm = TRUE to show line breaks
      geom_point() + # Add points for visual clarity
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkred", size = 1) + # Trendline
      labs(title = paste0(indicator_name, " for ", country),
           x = "Year",
           y = "Value") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2023-01-01"))) +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        plot.title = element_text(hjust = 0.5)
      )
    
    # Save the plot as PNG in the corresponding indicator's folder
    ggsave(filename = paste0("analysis/cat9_tm/trends/", indicator_name, "/", country, "_time_series.png"),
           plot = p, width = 8, height = 6)
  }
}

## Aggregated -----
# Loop through each dataset in the list
for (indicator_name in names(tm)) {
  current_data <- tm[[indicator_name]]
  
  # Create a directory for each indicator if it does not exist
  dir.create(paste0("analysis/cat9_tm/trends/", indicator_name, "/"), recursive = TRUE, showWarnings = FALSE)
  
  
  # Filter countries with at least 5 consecutive non-NA data points
  tab1 <- current_data %>%
    group_by(Country, Year) %>%
    summarise(Value = sum(Value)) %>%
    ungroup()
  
  # Convert 'Year' to Date object
  tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"), format = "%Y-%m-%d")
  
  tab1 <- filter_latest_consecutive_series(tab1)
  
  tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))
  
  # Compute aggregated values (mean or sum across countries) for each year
  aggregated_data <- tab1 %>%
    group_by(Year) %>%
    summarise(AggregatedValue = mean(Value, na.rm = TRUE), .groups = "drop")  # Adjust to 'sum' if needed
  
  # Create a time series plot with aggregated trendline and error margin
  p <- ggplot(tab1, aes(x = Year, y = Value)) +
    geom_point(position = position_jitter(width = 30, height = 0)) + # Jitter points slightly for spacing
    geom_smooth(data = aggregated_data, aes(x = Year, y = AggregatedValue), 
                method = "lm", se = TRUE, linetype = "dashed", color = "darkred", fill = "lightpink", size = 1.5) + # Aggregated trendline with error margin
    labs(title = paste("Aggregated Time Series for", indicator_name),
         x = "Year",
         y = "Value") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2023-01-01"))) +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey90"),
      plot.title = element_text(hjust = 0.5)
    )
  
  # Save the aggregated plot as PNG in the corresponding indicator's folder
  ggsave(filename = paste0("analysis/cat9_tm/trends/", indicator_name, "/aggregated_time_series_with_error_margin.png"),
         plot = p, width = 10, height = 8)

}

## Scatter ----
# including also the non consecutive
# Loop through each dataset in the 'tm' list
for (indicator_name in names(tm)) {
  current_data <- tm[[indicator_name]]
  
  # Create a directory for each indicator if it does not exist
  dir.create(paste0("analysis/cat9_tm/trends/", indicator_name, "/"), recursive = TRUE, showWarnings = FALSE)
  
  # Filter countries with at least 5 consecutive non-NA data points
  tab1 <- current_data %>%
    group_by(Country, Year) %>%
    summarise(Value = sum(Value)) %>%
    ungroup()
  
  # Convert 'Year' to Date object
  tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"), format = "%Y-%m-%d")
  
  tab1 <- filter_latest_consecutive_series(tab1)
  
  tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))
  
  # Create a scatter plot with each country's trend
  p <- ggplot(tab1, aes(x = Year, y = Value, group = Country, color = Country)) +
    geom_point(position = position_jitter(width = 30, height = 3)) + # Jitter points to avoid overlap
    labs(title = paste0("Disaggregated Scatter Plot for ", indicator_name, " (All Countries)"),
         x = "Year",
         y = "Value") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2023-01-01"))) +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey90"),
      plot.title = element_text(hjust = 0.5)
    )
  
  # Save the plot as PNG
  ggsave(filename = paste0("analysis/cat9_tm/trends/", indicator_name, "/scatter.png"),
         plot = p, width = 10, height = 8)
}


# COMPARATIVE ----
## Latest map ----

# Loop through each dataset in the 'tm' list
for (indicator_name in names(tm)) {
  # Process data for comparison (using your custom processing function)
  data <- processing_comparison(tm[[indicator_name]], "Number")
  
  # Create a static map based on the processed data
  p <- static_map(data)
  
  # Create a directory for saving the maps
  dir.create(paste0("analysis/cat9_tm/map/", indicator_name), recursive = TRUE, showWarnings = FALSE)
  
  # Save the static map as PNG in the corresponding indicator's folder
  ggsave(filename = paste0("analysis/cat9_tm/map/", indicator_name, "/static_map_with_auto_zoom_adapted_frame.png"),
         plot = p, width = 12, height = 12)
}



# MODEL ----
## Variance Test ----

## Model ----
library(dplyr)
library(purrr)
library(broom)
library(MASS)

# List of dataframes
tm <- list(tm_cwd = tm_cwd, tm_edu = tm_edu, tm_health = tm_health, tm_police = tm_police, tm_welfare = tm_welfare)

# Function to calculate mean and variance for each country in the dataframe
calculate_variance <- function(df) {
  # Filter countries with at least 5 consecutive non-NA data points
  tab1 <- df %>%
    group_by(Country, Year) %>%
    summarise(Value = sum(Value)) %>%
    ungroup()
  
  # Convert 'Year' to Date object
  tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"), format = "%Y-%m-%d")
  
  tab1 <- filter_latest_consecutive_series(tab1)
  
  tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))
  tab1 %>%
    group_by(Country) %>%
    summarise(
      mean_value = mean(Value, na.rm = TRUE),
      variance_value = var(Value, na.rm = TRUE),
      overdispersion = variance_value > mean_value  # Flag if variance is greater than the mean
    )
}

# Apply the function to each dataframe in the list `tm`
variance_results <- tm %>%
  map(calculate_variance)

# View results for each dataframe
print(variance_results)

# there is overdispersion everywhere so Negatibe BInomial

# Function to fit and extract results from negative binomial models for each dataframe
run_nb_model <- function(df) {
  # Filter countries with at least 5 consecutive non-NA data points
  tab1 <- df %>%
    group_by(Country, Year) %>%
    summarise(Value = sum(Value)) %>%
    ungroup()
  
  # Convert 'Year' to Date object
  tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"), format = "%Y-%m-%d")
  
  tab1 <- filter_latest_consecutive_series(tab1)
  
  tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))
  tab1 %>%
    group_by(Country) %>%
    filter(!is.na(Value)) %>%  # Remove missing values
    nest() %>%  # Nest data by country
    mutate(
      model = map(data, ~ tryCatch(glm.nb(Value ~ Year, data = .x), error = function(e) NULL)),  # Handle errors
      result = map(model, ~ if (!is.null(.x)) tidy(.x) else NULL)  # Extract results if model is not NULL
    ) %>%
    unnest(cols = c(result)) %>%  # Unnest the result column
    filter(term == "Year") %>%  # Extract coefficient for Year
    mutate(Significant = ifelse(p.value < 0.05, "*", "")) %>%  # Add significance flag
    dplyr::select(-data, -model)  # Remove the nested columns
}

# Apply the function to each dataframe in the list `tm`
nb_results_list <- tm %>% 
  imap(~ run_nb_model(.x) %>% mutate(DataFrame = .y))  # Add DataFrame name as a column for reference

# Combine results into a single dataframe
nb_results_combined <- bind_rows(nb_results_list)

# View combined results
print(nb_results_combined)

# Save combined results to CSV
write.csv(nb_results_combined, "analysis/cat9_tm/trend_cases_nb.csv", row.names = FALSE)



## Gender ----
tm_cwd <- cat9 %>% filter(Indicator == "Number of child victims of violence with disabilities (0-17 years) registered by child/social welfare authorities during the year"
                          & Sex != "Total" & AgeGroup == "0-17") 
tm_edu <- cat9 %>% filter(grepl("education", Indicator, ignore.case = TRUE)
                          & Sex != "Total" & AgeGroup == "0-17") 
tm_health <- cat9 %>% filter(grepl("healthcare", Indicator, ignore.case = TRUE)
                             & Sex != "Total" & AgeGroup == "0-17") 
tm_police <- cat9 %>% filter(Indicator == "Number of child victims of crime (0-17 years) registered by the police during the year"
                             & Sex != "Total" & AgeGroup == "0-17") 
tm_welfare <- cat9 %>% filter(Indicator == "Number of child victims of violence (0-17 years) registered by child/social welfare authorities during the year"
                              & Sex != "Total" & AgeGroup == "0-17") 


tm <- list(tm_cwd = tm_cwd, tm_edu = tm_edu, tm_health = tm_health, tm_police = tm_police, tm_welfare = tm_welfare)
# Loop through each dataset in the `tm` list
for (indicator_name in names(tm)) {
  current_data <- tm[[indicator_name]]
  
  # Check the unique values in the Sex column
  print(paste("Indicator:", indicator_name))
  print(unique(current_data$Sex))
  
  # Filter to get the latest year for each country and sex
  data <- current_data %>%
    group_by(Country, ISO3, Indicator, Sex) %>%
    top_n(1, Year) %>%
    ungroup()
  
  # Check if there are multiple values in Sex for each Country
  print(table(data$Country, data$Sex))
  
  # Create the bar chart
  p <- ggplot(data, aes(x = Country, y = Value, fill = Sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste0("Sex disaggregation by Country (latest data) - ", indicator_name),
         x = "Country",
         y = "Total Victims") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Print and save the plot
  print(p)
  
  # Create directory for each indicator if it does not exist
  dir.create(paste0("analysis/cat9_tm/gender_totals/", indicator_name, "/"), recursive = TRUE, showWarnings = FALSE)
  
  # Save the plot in the corresponding directory
  ggsave(filename = paste0("analysis/cat9_tm/gender_totals/", indicator_name, "/gender_latest.png"),
         plot = p, width = 12, height = 12)
}


gender <- cat9 %>%
  filter(Sex != "Total" & AgeGroup == "0-17") %>%
  group_by(Country, Indicator, Sex) %>%
  top_n(1, Year) %>%
  ungroup() %>%
  group_by(Indicator, Sex) %>%
  summarise(Median = median(Value, na.rm = TRUE),
            Mean = mean(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from =  Sex, values_from = c(Median, Mean))



aggregated <- cat9 %>%
  filter(Sex == "Total" & AgeGroup == "0-17") %>%
  group_by(Country, Indicator) %>%
  filter(Year == max(Year)) %>%  # Select the latest year per Country and Indicator
  ungroup() %>%
  group_by(Indicator, Country) %>%
  summarise(Tot = sum(Value), .groups = "drop") %>%  # Sum values
  arrange(desc(Tot)) %>%  # Arrange by Tot in descending order
  group_by(Indicator) %>%
  slice_max(Tot, n = 3) %>%  # Get the top 3 rows per Indicator
  ungroup()

write.csv(aggregated, "analysis/cat9_tm/aggregated_totals_latest.csv")
write.csv(gender, "analysis/cat9_tm/gender.csv")

aggregated <- cat9 %>%
  filter(Sex == "Total" & AgeGroup == "0-17") %>%
  group_by(Indicator, Country) %>%
  summarise(Tot = sum(Value)) %>%
  slice_max(Tot, n = 3, with_ties = FALSE) %>%
  ungroup()

write.csv(aggregated, "analysis/cat9_tm/aggregated_totals_all.csv")

