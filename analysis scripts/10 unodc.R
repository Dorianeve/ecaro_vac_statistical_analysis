rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")
source("requirements/filter_latest_consecutive_data.R")
source("requirements/filter_latest_consecutive_series_layertwo.R")

icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")


# No age digagg (Only CHildren)
# Female / Male / total
# Multiple years

# Trends ----
# Rate
unodc <- icvac %>% filter(ICVACCategory == "Composite phoenomena" &
                         Indicator == "Human trafficking  -  Total - Children")

data <- unodc %>%
  filter(Sex == "Total" & StatisticalUnit == "Counts")

# Convert 'Year' to Date object
data$Year <- as.Date(paste0(data$Year, "-01-01"))

tab1 <- data %>%
  group_by(Country, Year) %>%
  summarise(Value = sum(Value)) %>%
  ungroup()

# Create a full sequence of years (2010-2023) for each country
tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))

tb <- tab1 %>%
  arrange(Year) %>%
  pivot_wider(names_from = Year, values_from = Value)

# Create a full sequence of years (2010-2023) for each country
tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))

full_years <- expand.grid(
  Country = unique(tab1$Country),
  Year = seq(as.Date("2010-01-01"), as.Date("2022-01-01"), by = "year")
)
# Merge the full sequence of years with the original dataset
tab1 <- full_years %>%
  left_join(tab1, by = c("Country", "Year"))

# using this to filter the countries with the time-series requirement: 
# latest ts with at least 5 datapoints
tb <- tab1 %>%
  arrange(Year) %>%
  pivot_wider(names_from = Year, values_from = Value)

list <- c("Albania", "Belarus",
          "Bosnia and Herzegovina",
          "France", "Republic of Moldova",
          "Serbia", "Slovakia",
          "Tajikistan", "Türkiye", "United Kingdom",
          "Uzbekistan")

tab1 <- tab1 %>%
  filter(Country %in% list)

tab1 <- tab1 %>%
  mutate(Value = case_when(
    (Year == "2010-01-01" |
       Year == "2011-01-01" |
       Year == "2012-01-01") & (Country == "Bosnia and Herzegovina") ~ NA,
    (Year == "2015-01-01" |
       Year == "2016-01-01") & (Country == "France") ~ NA,
    (Year == "2010-01-01" |
       Year == "2011-01-01" |
       Year == "2012-01-01" |
       Year == "2013-01-01" |
       Year == "2014-01-01") & (Country == "Serbia") ~ NA,
    (Year == "2010-01-01") & (Country == "Tajikistan") ~ NA,
    (Year == "2011-01-01") & (Country == "Türkiye") ~ NA,
    TRUE ~ Value
  ))

# Ensure the folder 'analysis/cat1_unodc' exists
dir.create("analysis/cat10_unodc/cases/", recursive = TRUE, showWarnings = FALSE)

# Loop through each country and save the time series plot
unique_countries <- unique(tab1$Country)

for (country in unique_countries) {
  # Filter data for the country
  country_data <- tab1 %>%
    filter(Country == country)
  
  # Create a time series plot with a trendline and colorful line
  p <- ggplot(country_data, aes(x = Year, y = Value)) +
    geom_line(aes(color = Country), na.rm = TRUE, size = 1.2) + # Colorful line for each country
    geom_point() + # Add points even where there are NAs
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkred", size = 1) + # Add trendline
    labs(title = paste("Victims of Human Trafficking - UNODC - for", country),
         x = "Year",
         y = "Value") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2023-01-01"))) +
    theme_minimal(base_size = 12) + # Base white background
    theme(
      panel.background = element_rect(fill = "white"), # Ensure background is white
      panel.grid.major = element_line(color = "grey90"), # Light grey grid lines
      plot.title = element_text(hjust = 0.5) # Center the title
    )
  
  # Save the plot as PNG in the folder 'analysis/cat1_unodc'
  ggsave(filename = paste0("analysis/cat10_unodc/cases/", country, "_time_series.png"),
         plot = p, width = 8, height = 6)
}

## Model ----
library(dplyr)
library(purrr)
library(broom)
library(MASS)

df <- tab1
# Calculate mean and variance for each country
variance_check <- df %>%
  group_by(Country) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    variance_value = var(Value, na.rm = TRUE),
    overdispersion = variance_value > mean_value  # Flag if variance is greater than the mean
  )

# View results
print(variance_check)

# Better model for overdispersion
# Fit Negative Binomial model for each country and extract significant results
nb_results <- df %>%
  group_by(Country) %>%
  filter(!is.na(Value)) %>%  # Remove missing values
  nest() %>%  # Nest data by country
  mutate(model = map(data, ~ glm.nb(Value ~ Year, data = .x)),  # Fit Negative Binomial model
         result = map(model, tidy)) %>%  # Summarize the model results
  unnest(cols = c(result)) %>%  # Unnest the result column
  filter(term == "Year") %>%  # Extract coefficient for Year
  mutate(Significant = ifelse(p.value < 0.05, "*", "")) %>%
  dplyr::select(-data,-model)
# View results with significance flag
print(nb_results)

write.csv(nb_results, "analysis/cat10_unodc/trends_nb_cases.csv")

# Poisson Regression
# Group data by country and apply the model for each country separately
trend_results <- df %>%
  group_by(Country) %>%
  filter(!is.na(Value)) %>%  # Remove missing values
  nest() %>%  # Nest the data by country
  mutate(model = map(data, ~ glm(Value ~ Year, family = poisson(link = "log"), data = .x)),
         result = map(model, tidy)) %>%
  unnest(result) %>%
  filter(term == "Year") %>%  # Extract coefficient for Year
  mutate(Significant = ifelse(p.value < 0.05, "*", "")) %>%  # Add significance flag
  dplyr::select(Country, term, estimate, std.error, statistic, p.value, Significant)  # Select relevant columns

# View results with significance flag
print(trend_results)


# Aggregated ----
# Compute aggregated values (mean or sum across countries) for each year
aggregated_data <- tab1 %>%
  group_by(Year) %>%
  summarise(AggregatedValue = mean(Value, na.rm = TRUE)) # Use mean or sum depending on your preference

# Create a time series plot with all countries' lines, aggregated trendline, and error margin
p <- ggplot(tab1, aes(x = Year, y = Value)) +
  geom_point(position = position_jitter(width = 30, height = 0)) + # Jitter points slightly for spacing
  geom_smooth(data = aggregated_data, aes(x = Year, y = AggregatedValue), 
              method = "lm", se = TRUE, linetype = "dashed", color = "darkred", fill = "lightpink", size = 1.5) + # Aggregated trendline with error margin
  labs(title = "Victims of Human Trafficking - UNODC (All Countries with Aggregated Trend and Error Margin)",
       x = "Year",
       y = "Value") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2008-01-01", "2023-01-01"))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0.5)
  )

# Save the plot as PNG
ggsave(filename = "analysis/cat10_unodc/cases/aggregated_time_series_with_error_margin.png",
       plot = p, width = 10, height = 8)

# DIsaggregate per country to get outliers
# Compute aggregated values (mean or sum across countries) for each year
aggregated_data <- tab1 %>%
  group_by(Country, Year) %>%
  summarise(AggregatedValue = mean(Value, na.rm = TRUE)) # Use mean or sum depending on your preference

# Create a time series plot with all countries' lines, aggregated trendline, and error margin
p <- ggplot(tab1, aes(x = Year, y = Value, group = Country, color = Country)) +
  geom_point(position = position_jitter(width = 30, height = 0)) + # Jitter points slightly for spacing
  labs(title = "Victims of Human Trafficking - UNODC (All Countries with Aggregated Trend and Error Margin)",
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
ggsave(filename = "analysis/cat10_unodc/cases/scatter.png",
       plot = p, width = 10, height = 8)

write.csv(tab1, "analysis/cat10_unodc/cases_trend.csv")

cumulative <- data %>%
  group_by(Country) %>%
  summarise(CumulativeTotal = sum(Value, na.rm = TRUE))

write.csv(tab1, "analysis/cat10_unodc/cumulative_totals.csv")

# COMPARATIVE ----
## Latest map ----

data <- processing_comparison(data, "Counts")
p <- static_map(data)
ggsave(filename = "analysis/cat10_unodc/cases/static_map_with_auto_zoom_adapted_frame.png",
       plot = p, width = 12, height = 12)

write.csv(data, "analysis/cat10_unodc/latest_cases.csv")

## Gender ----
unodc <- icvac %>% filter(ICVACCategory == "Composite phoenomena" &
                            Indicator == "Human trafficking  -  Total - Children")
data <- unodc %>%
  filter(Sex != "Total" & StatisticalUnit == "Counts")

stats_over_time <- data %>%
  group_by(Sex) %>%
  summarize(Median = median(Value, na.rm = TRUE),
            Mean = mean(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Sex, values_from = c(Median, Mean))
write.csv(stats_over_time, "analysis/cat10_unodc/gender_overtime.csv")

data <- data %>%
  group_by(Country, Year, ISO3, Indicator, Sex) %>%
  summarise(Total = sum(Value))

data <- data %>%
  filter(!is.na(Total)) %>%
  group_by(Country, ISO3, Indicator) %>%
  top_n(1, Year) %>%
  ungroup()

p <- ggplot(data, aes(x = Country, y = Total, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gender Totals by Country (latest data)",
       x = "Country",
       y = "Total Victims of Human trafficking") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

ggsave(filename = "analysis/cat10_unodc/cases/gender_latest.png",
       plot = p, width = 12, height = 12)

# clean env ----
rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")
source("requirements/filter_latest_consecutive_data.R")
source("requirements/filter_latest_consecutive_series_layertwo.R")

# RATE ----
icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

# Trends ----
# Rate
unodc <- icvac %>% filter(ICVACCategory == "Composite phoenomena" &
                            Indicator == "Human trafficking  -  Total - Children")

data <- unodc %>%
  filter(Sex == "Total" & StatisticalUnit == "Rate per 100,000 population")

# Convert 'Year' to Date object
data$Year <- as.Date(paste0(data$Year, "-01-01"))

tab1 <- data %>%
  group_by(Country, Year) %>%
  summarise(Value = sum(Value)) %>%
  ungroup()

# Create a full sequence of years (2010-2023) for each country
tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))

tb <- tab1 %>%
  arrange(Year) %>%
  pivot_wider(names_from = Year, values_from = Value)

# Create a full sequence of years (2010-2023) for each country
tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))

full_years <- expand.grid(
  Country = unique(tab1$Country),
  Year = seq(as.Date("2010-01-01"), as.Date("2022-01-01"), by = "year")
)
# Merge the full sequence of years with the original dataset
tab1 <- full_years %>%
  left_join(tab1, by = c("Country", "Year"))

# using this to filter the countries with the time-series requirement: 
# latest ts with at least 5 datapoints
tb <- tab1 %>%
  arrange(Year) %>%
  pivot_wider(names_from = Year, values_from = Value)

list <- c("Albania", "Belarus",
          "Bosnia and Herzegovina",
          "France", "Republic of Moldova",
          "Serbia", "Slovakia",
          "Tajikistan", "Türkiye", "United Kingdom",
          "Uzbekistan")

tab1 <- tab1 %>%
  filter(Country %in% list)

tab1 <- tab1 %>%
  mutate(Value = case_when(
    (Year == "2010-01-01" |
       Year == "2011-01-01" |
       Year == "2012-01-01") & (Country == "Bosnia and Herzegovina") ~ NA,
    (Year == "2015-01-01" |
       Year == "2016-01-01") & (Country == "France") ~ NA,
    (Year == "2010-01-01" |
       Year == "2011-01-01" |
       Year == "2012-01-01" |
       Year == "2013-01-01" |
       Year == "2014-01-01") & (Country == "Serbia") ~ NA,
    (Year == "2010-01-01") & (Country == "Tajikistan") ~ NA,
    (Year == "2011-01-01") & (Country == "Türkiye") ~ NA,
    TRUE ~ Value
  ))

# Ensure the folder 'analysis/cat1_unodc' exists
dir.create("analysis/cat10_unodc/rate/", recursive = TRUE, showWarnings = FALSE)

# Loop through each country and save the time series plot
unique_countries <- unique(tab1$Country)

for (country in unique_countries) {
  # Filter data for the country
  country_data <- tab1 %>%
    filter(Country == country)
  
  # Create a time series plot with a trendline and colorful line
  p <- ggplot(country_data, aes(x = Year, y = Value)) +
    geom_line(aes(color = Country), na.rm = TRUE, size = 1.2) + # Colorful line for each country
    geom_point() + # Add points even where there are NAs
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkred", size = 1) + # Add trendline
    labs(title = paste("Victims of Human Trafficking - UNODC - for", country),
         x = "Year",
         y = "Value") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2023-01-01"))) +
    theme_minimal(base_size = 12) + # Base white background
    theme(
      panel.background = element_rect(fill = "white"), # Ensure background is white
      panel.grid.major = element_line(color = "grey90"), # Light grey grid lines
      plot.title = element_text(hjust = 0.5) # Center the title
    )
  
  # Save the plot as PNG in the folder 'analysis/cat1_unodc'
  ggsave(filename = paste0("analysis/cat10_unodc/rate/", country, "_time_series.png"),
         plot = p, width = 8, height = 6)
}

## Model ----
library(dplyr)
library(purrr)
library(broom)
library(MASS)

df <- tab1
# Calculate mean and variance for each country
variance_check <- df %>%
  group_by(Country) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    variance_value = var(Value, na.rm = TRUE),
    overdispersion = variance_value > mean_value  # Flag if variance is greater than the mean
  )

# View results
print(variance_check)

# not too dispersed
poisson_results <- df %>%
  group_by(Country) %>%
  filter(!is.na(Value)) %>%  # Remove missing values
  nest() %>%  # Nest data by country
  mutate(
    model = map(data, ~ tryCatch(glm(Value ~ Year, family = poisson(link = "log"), data = .x), error = function(e) NULL)),  # Handle errors
    result = map(model, ~ if (!is.null(.x)) tidy(.x) else NULL)  # Extract results if model is not NULL
  ) %>%
  unnest(cols = c(result)) %>%  # Unnest the result column
  filter(term == "Year") %>%  # Extract coefficient for Year
  mutate(Significant = ifelse(p.value < 0.05, "*", "")) %>%  # Add significance flag
  dplyr::select(-data, -model)  # Remove the nested columns

# View the results
print(poisson_results)

write.csv(poisson_results, "analysis/cat10_unodc/trend_rate_poisson.csv", row.names = FALSE)


# Aggregated ----
# Compute aggregated values (mean or sum across countries) for each year
aggregated_data <- tab1 %>%
  group_by(Year) %>%
  summarise(AggregatedValue = mean(Value, na.rm = TRUE)) # Use mean or sum depending on your preference

# Create a time series plot with all countries' lines, aggregated trendline, and error margin
p <- ggplot(tab1, aes(x = Year, y = Value)) +
  geom_point(position = position_jitter(width = 30, height = 0)) + # Jitter points slightly for spacing
  geom_smooth(data = aggregated_data, aes(x = Year, y = AggregatedValue), 
              method = "lm", se = TRUE, linetype = "dashed", color = "darkred", fill = "lightpink", size = 1.5) + # Aggregated trendline with error margin
  labs(title = "Victims of Human Trafficking - UNODC (All Countries with Aggregated Trend and Error Margin)",
       x = "Year",
       y = "Value") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2008-01-01", "2023-01-01"))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0.5)
  )

# Save the plot as PNG
ggsave(filename = "analysis/cat10_unodc/rate/aggregated_time_series_with_error_margin.png",
       plot = p, width = 10, height = 8)

# DIsaggregate per country to get outliers
# Compute aggregated values (mean or sum across countries) for each year
aggregated_data <- tab1 %>%
  group_by(Country, Year) %>%
  summarise(AggregatedValue = mean(Value, na.rm = TRUE)) # Use mean or sum depending on your preference

# Create a time series plot with all countries' lines, aggregated trendline, and error margin
p <- ggplot(tab1, aes(x = Year, y = Value, group = Country, color = Country)) +
  geom_point(position = position_jitter(width = 30, height = 0)) + # Jitter points slightly for spacing
  labs(title = "Victims of Human Trafficking - UNODC (All Countries with Aggregated Trend and Error Margin)",
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
ggsave(filename = "analysis/cat10_unodc/rate/scatter.png",
       plot = p, width = 10, height = 8)

write.csv(tab1, "analysis/cat10_unodc/rate_trens.csv")

AverageRate <- data %>%
  group_by(Country) %>%
  summarise(AverageRate = mean(Value, na.rm = TRUE))

write.csv(AverageRate, "analysis/cat10_unodc/averate_rate.csv")

# COMPARATIVE ----
## Latest map ----

data <- processing_comparison(data, "Rate per 100,000 population")
p <- static_map(data)
ggsave(filename = "analysis/cat10_unodc/rate/static_map_with_auto_zoom_adapted_frame.png",
       plot = p, width = 12, height = 12)

write.csv(data, "analysis/cat10_unodc/latest_rate.csv")
## Gender ----
unodc <- icvac %>% filter(ICVACCategory == "Composite phoenomena" &
                            Indicator == "Human trafficking  -  Total - Children")
data <- unodc %>%
  filter(Sex != "Total" & StatisticalUnit == "Rate per 100,000 population")

stats_over_time <- data %>%
  group_by(Sex) %>%
  summarize(Median = median(Value, na.rm = TRUE),
            Mean = mean(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Sex, values_from = c(Median, Mean))
write.csv(stats_over_time, "analysis/cat10_unodc/gender_overtime_rate.csv")

data <- data %>%
  group_by(Country, Year, ISO3, Indicator, Sex) %>%
  summarise(Average = mean(Value))

data <- data %>%
  filter(!is.na(Average)) %>%
  group_by(Country, ISO3, Indicator) %>%
  top_n(1, Year) %>%
  ungroup()

abs <- data %>%
  pivot_wider(names_from = Sex, values_from = Average) %>%
  mutate(Diff = abs(Male - Female)) %>%  # Use abs() for absolute difference
  arrange(desc(Diff))      

write.csv(abs, "analysis/cat10_unodc/gender_differences_latest.csv")

p <- ggplot(data, aes(x = Country, y = Average, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sex disaggregation by Country (latest data) - Victims of Human trafficking",
       x = "Country",
       y = "Rate per 100,00 pop") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

ggsave(filename = "analysis/cat10_unodc/rate/gender_latest.png",
       plot = p, width = 12, height = 12)
