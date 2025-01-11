rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")
source("requirements/filter_latest_consecutive_data.R")

icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

# TRENDS: counts ----
# selection on consecutive 5 years at least
# trend of all the available latest data
# summing data M+F
# single lines with breaks to get country trend
# aggregated scatter to get country outliers

unodc <- icvac %>% filter(ICVACCategory == "Violent killing of a child" &
                          StatisticalUnit == "Counts" & RelationshipPerpetrator == "Total")

# Get yearly count
yearly <- unodc %>%
  group_by(Country, Year) %>%
  summarise(Total = sum(Value, na.rm = TRUE))

write.csv(yearly, "analysis/cat1_unodc/yearly_count.csv", row.names = FALSE)


# Data has Female and Male# Data has Female and Male
# We can simply sum to get the total
# There is one graph for M/F and one graph for total last year cases comparison 

## Country disaggregation ----
# Filter countries with at least 5 consecutive non-NA data points
# Apply the function for each country to filter and keep the most recent series

# Convert 'Year' to Date object
unodc$Year <- as.Date(paste0(unodc$Year, "-01-01"))

tab1 <- unodc %>%
  group_by(Country, Year) %>%
  summarise(Value = sum(Value)) %>%
  ungroup()

# Create a full sequence of years (2010-2023) for each country
tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))

tab1 <- filter_latest_consecutive_series(tab1)

tb <- tab1 %>%
  pivot_wider(names_from = Year, values_from = Value)
# Create a full sequence of years (2010-2023) for each country
tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))

full_years <- expand.grid(
  Country = unique(tab1$Country),
  Year = seq(as.Date("2010-01-01"), as.Date("2023-01-01"), by = "year")
)
# Merge the full sequence of years with the original dataset
tab1 <- full_years %>%
  left_join(tab1, by = c("Country", "Year"))

# Ensure the folder 'analysis/cat1_unodc' exists
dir.create("analysis/cat1_unodc/cases/", recursive = TRUE, showWarnings = FALSE)

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
    labs(title = paste("Victims of Intentional Homicide - UNODC - for", country),
         x = "Year",
         y = "NUmber") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2023-01-01"))) +
    theme_minimal(base_size = 12) + # Base white background
    theme(
      panel.background = element_rect(fill = "white"), # Ensure background is white
      panel.grid.major = element_line(color = "grey90"), # Light grey grid lines
      plot.title = element_text(hjust = 0.5) # Center the title
    )
  
  # Save the plot as PNG in the folder 'analysis/cat1_unodc'
  ggsave(filename = paste0("analysis/cat1_unodc/cases/", country, "_time_series.png"),
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
# better Negative BInombial as it is very dispersed
# Better model for overdispersion
# Fit Negative Binomial model for each country and extract significant results
nb_results <- df %>%
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

# View results
print(nb_results)

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

write.csv(nb_results, "analysis/cat1_unodc/trend_cases_nb.csv", row.names = FALSE)

## Aggregated ----
# Compute aggregated values (mean or sum across countries) for each year
aggregated_data <- tab1 %>%
  group_by(Year) %>%
  summarise(AggregatedValue = mean(Value, na.rm = TRUE)) # Use mean or sum depending on your preference

# Create a time series plot with all countries' lines, aggregated trendline, and error margin
p <- ggplot(tab1, aes(x = Year, y = Value)) +
  geom_point(position = position_jitter(width = 30, height = 0)) + # Jitter points slightly for spacing
  geom_smooth(data = aggregated_data, aes(x = Year, y = AggregatedValue), 
              method = "lm", se = TRUE, linetype = "dashed", color = "darkred", fill = "lightpink", size = 1.5) + # Aggregated trendline with error margin
  labs(title = "Victims of Intentional Homicide - UNODC (Mean of all countries per year and Error Margin)",
       x = "Year",
       y = "Number") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2023-01-01"))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0.5)
  )

# Save the plot as PNG
ggsave(filename = "analysis/cat1_unodc/cases/aggregated_time_series_with_error_margin.png",
       plot = p, width = 10, height = 8)

write.csv(aggregated_data, "analysis/cat1_unodc/aggregated_cases_time_series.csv", row.names = FALSE)

# DIsaggregate per country to get outliears
# Compute aggregated values (mean or sum across countries) for each year
aggregated_data <- tab1 %>%
  group_by(Country, Year) %>%
  summarise(AggregatedValue = mean(Value, na.rm = TRUE)) # Use mean or sum depending on your preference

# Create a time series plot with all countries' lines, aggregated trendline, and error margin
p <- ggplot(tab1, aes(x = Year, y = Value, group = Country, color = Country)) +
  geom_point(position = position_jitter(width = 30, height = 0)) + # Jitter points slightly for spacing
  labs(title = "Victims of Intentional Homicide - UNODC - data points",
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
ggsave(filename = "analysis/cat1_unodc/cases/scatter.png",
       plot = p, width = 10, height = 8)


write.csv(tab1, "analysis/cat1_unodc/cases_time_series.csv", row.names = FALSE)

# COMPARATIVE ----
## Latest map ----

cat1 <- icvac %>% filter(ICVACCategory == "Violent killing of a child" & RelationshipPerpetrator == "Total")
cat1_comparison <- processing_comparison(cat1, "Counts")
p <- static_map(cat1_comparison)
ggsave(filename = "analysis/cat1_unodc/cases/static_map_with_auto_zoom_adapted_frame.png",
       plot = p, width = 12, height = 12)

write.csv(cat1_comparison, "analysis/cat1_unodc/cases_latest_for_map.csv")

## Gender ----
tab1 <- icvac %>% filter(ICVACCategory == "Violent killing of a child" &
                           StatisticalUnit == "Counts" & RelationshipPerpetrator == "Total")

data <- tab1 %>%
  group_by(Country, Year, ISO3, Indicator, Sex) %>%
  summarise(Total = sum(Value))

data <- data %>%
  group_by(Country, ISO3, Indicator) %>%
  top_n(1, Year) %>%
  ungroup()

p <- ggplot(data, aes(x = Country, y = Total, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Victims of interntional homicide - Sex disaggregation - (age 0-19) (latest data)",
       x = "Country",
       y = "Number") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

ggsave(filename = "analysis/cat1_unodc/cases/gender_latest.png",
       plot = p, width = 12, height = 12)



# AgeGroup analysis ----
# Rate
# Only one year per country
# exclude aggregated category (same for DHS)
data <- icvac %>%
  filter(ICVACCategory == "Violent killing of a child" &
           StatisticalUnit == "Counts" & RelationshipPerpetrator == "Total")

sum_cum <- data %>%
  group_by(AgeGroup) %>%
  summarise(Total = sum(Value)) %>%
  arrange(desc(Total))
write.csv(sum_cum, "analysis/cat1_unodc/cumulative_age_groups.csv")

data <- icvac %>%
  filter(ICVACCategory == "Violent killing of a child" &
           StatisticalUnit == "Counts" & RelationshipPerpetrator == "Total") %>%
  group_by(Country, AgeGroup, Sex, Year) %>%
  summarise(Total = sum(Value), .groups = 'drop_last') %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>% 
  ungroup()

data %<>%
  group_by(Country,AgeGroup) %>%
  summarise(Total = sum(Total))

# Set the desired order of AgeGroup levels
age_levels <- c("0-9","10-14","15-17","18-19")

# Ensure that AgeGroup is a factor with the correct level order
data$AgeGroup <- factor(data$AgeGroup, levels = age_levels)

# Set consistent color palette for AgeGroup
# Set consistent color palette for AgeGroup with Vaporwave aesthetics
age_colors <- c(
  "0-9" = "#FF6AD5",   # Bright Pink
  "10-14" = "#FFC75F",   # Neon Yellow
  "15-17" = "#6A0572",   # Deep Purple
  "18-19" = "#00EAD3"   # Bright Cyan
)
# Get the list of unique countries
countries <- unique(data$Country)

# Create individual plots for each country and save them separately
for (country in countries) {
  
  # Filter data for the specific country
  cat2_age <- data %>%
    filter(Country == country)
  
  # Create the barchart with consistent x, y axis limits, and color palette
  p <- ggplot(cat2_age, aes(x = AgeGroup, y = Total, fill = AgeGroup)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = age_colors) +  # Use the same colors for AgeGroup
    labs(title = paste("UNODC - Victims of Intentional Homicide (0-19) - AgeGroups - for", country),
         x = "Age Group",
         y = "Number") +
    theme_minimal() +  # White background and consistent font
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
    scale_y_continuous(limits = c(0, max(data$Total, na.rm = TRUE)))  # Ensure same y scale
  
  # Save each plot as a PNG file
  ggsave(filename = paste0("analysis/cat1_unodc/analysis/barchart_", country, ".png"), plot = p, width = 8, height = 6, bg = "white")
  
  # Optionally print the plot in the console to visualize
  print(p)
}

std <- data %>%
  group_by(Country) %>%
  summarise(STD = sd(Total, na.rm = TRUE)) %>%
  arrange(desc(STD)) %>%
  print(n = Inf)

write.csv(std, "analysis/cat1_unodc/age_group_std.csv")



# TRENDS: rate ----
# selection on consecutive 5 years at least
# trend of all the available latest data
# summing data M+F
# single lines with breaks to get country trend
# aggregated scatter to get country outliers

unodc <- icvac %>% filter(ICVACCategory == "Violent killing of a child" &
                            StatisticalUnit == "Rate per 100,000 population" & 
                            RelationshipPerpetrator == "Total")

yearly <- unodc %>%
  group_by(Country, Year) %>%
  summarise(Total = mean(Value, na.rm = TRUE))

write.csv(yearly, "analysis/cat1_unodc/yearly_rate.csv", row.names = FALSE)
# Data has Female and Male
# We can simply sum to get the total
# There is one graph for M/F and one graph for total last year cases comparison 

## Country disaggregation ----
# Filter countries with at least 5 consecutive non-NA data points
# Apply the function for each country to filter and keep the most recent series

# Convert 'Year' to Date object
unodc$Year <- as.Date(paste0(unodc$Year, "-01-01"))

tab1 <- unodc %>%
  group_by(Country, Year) %>%
  summarise(Value = mean(Value, na.rm = TRUE)) %>%
  ungroup()

tab1 <- filter_latest_consecutive_series(tab1)

# Create a full sequence of years (2010-2023) for each country
tab1$Year <- as.Date(paste0(tab1$Year, "-01-01"))

full_years <- expand.grid(
  Country = unique(tab1$Country),
  Year = seq(as.Date("2010-01-01"), as.Date("2023-01-01"), by = "year")
)
# Merge the full sequence of years with the original dataset
tab1 <- full_years %>%
  left_join(tab1, by = c("Country", "Year"))

# Ensure the folder 'analysis/cat1_unodc' exists
dir.create("analysis/cat1_unodc/rate/", recursive = TRUE, showWarnings = FALSE)

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
    labs(title = paste("Rate of victims of Intentional Homicide per 100,000 pop (0-19) - UNODC - for", country),
         x = "Year",
         y = "Rate") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2023-01-01"))) +
    theme_minimal(base_size = 12) + # Base white background
    theme(
      panel.background = element_rect(fill = "white"), # Ensure background is white
      panel.grid.major = element_line(color = "grey90"), # Light grey grid lines
      plot.title = element_text(hjust = 0.5) # Center the title
    )
  
  # Save the plot as PNG in the folder 'analysis/cat1_unodc'
  ggsave(filename = paste0("analysis/cat1_unodc/rate/", country, "_time_series.png"),
         plot = p, width = 8, height = 6)
}

## Model ----

df <- tab1
# Calculate mean and variance for each country
variance_check <- df %>%
  group_by(Country) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    variance_value = var(Value, na.rm = TRUE),
    overdispersion = variance_value > mean_value  # Flag if variance is greater than the mean
  )

print(variance_check)
# not too high variace
# Assuming your data frame is named `df` and has columns: Country, Year, and Rate
# Fit a Poisson regression model with log-link function for each country and extract significant results
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

write.csv(poisson_results, "analysis/cat1_unodc/trend_rate_poisson.csv", row.names = FALSE)

# For continuous rate data (NOT TO USE HERE)
# Fit a linear regression model for each country and extract significant results
linear_results <- df %>%
  group_by(Country) %>%
  filter(!is.na(Value)) %>%  # Remove missing values
  nest() %>%  # Nest data by country
  mutate(
    model = map(data, ~ tryCatch(lm(Value ~ Year, data = .x), error = function(e) NULL)),  # Handle errors
    result = map(model, ~ if (!is.null(.x)) tidy(.x) else NULL)  # Extract results if model is not NULL
  ) %>%
  unnest(cols = c(result)) %>%  # Unnest the result column
  filter(term == "Year") %>%  # Extract coefficient for Year
  mutate(Significant = ifelse(p.value < 0.05, "*", "")) %>%  # Add significance flag
  dplyr::select(-data, -model)  # Remove the nested columns

# View the results
print(linear_results)

## Aggregated ----
# Compute aggregated values (mean or sum across countries) for each year
aggregated_data <- tab1 %>%
  group_by(Year) %>%
  summarise(AggregatedValue = mean(Value, na.rm = TRUE)) # Use mean or sum depending on your preference

# Create a time series plot with all countries' lines, aggregated trendline, and error margin
p <- ggplot(tab1, aes(x = Year, y = Value)) +
  geom_point(position = position_jitter(width = 30, height = 0)) + # Jitter points slightly for spacing
  geom_smooth(data = aggregated_data, aes(x = Year, y = AggregatedValue), 
              method = "lm", se = TRUE, linetype = "dashed", color = "darkred", fill = "lightpink", size = 1.5) + # Aggregated trendline with error margin
  labs(title = "Rate of victims of Intentional Homicide (0-19) 100,000 pop - UNODC (Mean of all countries and Error Margin)",
       x = "Year",
       y = "Rate") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2023-01-01"))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0.5)
  )

# Save the plot as PNG
ggsave(filename = "analysis/cat1_unodc/rate/aggregated_time_series_with_error_margin.png",
       plot = p, width = 10, height = 8)

# DIsaggregate per country to get outliears
# Compute aggregated values (mean or sum across countries) for each year
aggregated_data <- tab1 %>%
  group_by(Country, Year) %>%
  summarise(AggregatedValue = mean(Value, na.rm = TRUE)) # Use mean or sum depending on your preference

# Create a time series plot with all countries' lines, aggregated trendline, and error margin
p <- ggplot(tab1, aes(x = Year, y = Value, group = Country, color = Country)) +
  geom_point(position = position_jitter(width = 30, height = 0)) + # Jitter points slightly for spacing
  labs(title = "Victims of Intentional Homicide (0-19) - UNODC - datapoints",
       x = "Year",
       y = "Rate") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2023-01-01"))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0.5)
  )

# Save the plot as PNG
ggsave(filename = "analysis/cat1_unodc/rate/scatter.png",
       plot = p, width = 10, height = 8)

write.csv(tab1, "analysis/cat1_unodc/rate_time_series.csv", row.names = FALSE)

# COMPARATIVE ----
## Latest map ----

cat1 <- icvac %>% filter(ICVACCategory == "Violent killing of a child" & 
                           RelationshipPerpetrator == "Total" & RelationshipPerpetrator == "Total")
cat1_comparison <- processing_comparison_mean(cat1, "Rate per 100,000 population")
p <- static_map(cat1_comparison)
ggsave(filename = "analysis/cat1_unodc/rate/static_map_with_auto_zoom_adapted_frame.png",
       plot = p, width = 12, height = 12)
write.csv(cat1_comparison, "analysis/cat1_unodc/rate_latest_for_map.csv")


# PERPETRATOR ----
# Comparison latest data ----
df <- icvac %>% filter(ICVACCategory == "Violent killing of a child" & 
                         Sex == "Total" & 
                         RelationshipPerpetrator != "Total" & 
                         StatisticalUnit == "Counts")

df %<>% 
  group_by(Country) %>%                                  # Group by Country
  filter(Year == max(Year)) %>%                          # Keep rows with the latest Year
  ungroup()  
df

df %<>%
  group_by(Country, RelationshipPerpetrator) %>%
  summarise(Value = Value)
df

# Set consistent color palette for Relationship to perpetrator with Vaporwave aesthetics
colors <- c(
  "Intimate partner or family member" = "#FF6AD5",   # Bright Pink
  "Intimate partner or family member: Family member" = "#FFC75F",   # Neon Yellow
  "Intimate partner or family member: Intimate partner" = "#6A0572",   # Deep Purple
  "Other Perpetrator known to the victim" = "#00EAD3",   # Bright Cyan
  "Perpetrator to victim relationship unknown" = "#845EC2",   # Deep Purple
  "Perpetrator unknown to the victim" = "#C34A36"   # Bright Cyan
)
# Get the list of unique countries
countries <- unique(df$Country)

# Create individual plots for each country and save them separately
for (country in countries) {
  
  # Filter data for the specific country
  cat2_perp <- df %>%
    filter(Country == country)
  
  # Create the barchart with consistent x, y axis limits, and color palette
  p <- ggplot(cat2_perp, aes(x = RelationshipPerpetrator, y = Value, fill = RelationshipPerpetrator)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +  # Use the same colors for AgeGroup
    labs(title = paste("UNODC - Victims of Intentional Homicide for (0-19)", country),
         x = "Relationship to Perpetrator",
         y = "Number") +
    theme_minimal() +  # White background and consistent font
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
    scale_y_continuous(limits = c(0, max(df$Value, na.rm = TRUE)))  # Ensure same y scale
  
  # Save each plot as a PNG file
  ggsave(filename = paste0("analysis/cat1_unodc/analysis/perpetrators/barchart_", country, ".png"), plot = p, width = 8, height = 6, bg = "white")
  
  # Optionally print the plot in the console to visualize
  print(p)
}

std <- df %>%
  group_by(Country) %>%
  summarise(STD = sd(Value, na.rm = TRUE)) %>%
  arrange(desc(STD)) %>%
  print(n = Inf)

write.csv(std, "analysis/cat1_unodc/perpetrator_std.csv")


# COmputing the most cases per perpetrator ----
df <- icvac %>% filter(ICVACCategory == "Violent killing of a child" & 
                         Sex == "Total" & 
                         RelationshipPerpetrator != "Total" & 
                         StatisticalUnit == "Counts")

cumulative <- df %>%
  group_by(RelationshipPerpetrator) %>%
  summarise(Total = sum(Value)) %>%
  arrange(desc(Total))

df %<>% 
  group_by(Country) %>%                                  # Group by Country
  filter(Year == max(Year)) %>%                          # Keep rows with the latest Year
  ungroup()  

latest <- df %>%
  group_by(RelationshipPerpetrator) %>%
  summarise(Total = sum(Value)) %>%
  arrange(desc(Total))

write.csv(latest, "analysis/cat1_unodc/perpetrator_count_latest.csv")
write.csv(cumulative, "analysis/cat1_unodc/perpetrator_count_cumulative.csv")



