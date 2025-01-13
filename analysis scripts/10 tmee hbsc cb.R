rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")


# Age 11,13,15
# Sex Female, Male, Total
# Only 2018
icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

# Gender ----
# Rate
# Only one year per country
hbsc <- icvac %>% filter(ICVACCategory == "Composite phoenomena" &
                           Indicator == "Percentage of 11-, 13- and 15-year-old school children who report having been a victim of cyber-bullying")

data <- hbsc %>% filter(Sex != "Total" & WealthQuintile == "Total")

p <- ggplot(data, aes(x = Country, y = Value, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sex disaggregaton by Country (latest data) - Cyber-bullying",
       x = "Country",
       y = "Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),  # Increase x-axis text size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size
        axis.title.x = element_text(size = 14),  # Increase x-axis label size
        axis.title.y = element_text(size = 14),  # Increase y-axis label size
        legend.text = element_text(size = 14),  # Increase legend text size
        legend.title = element_text(size = 16),  # Increase legend title size
        plot.title = element_text(size = 24, face = "bold"))  # Increase plot title size

# Print and save the plot
print(p)

# Save the plot in the corresponding directory
ggsave(filename = "graphs/cat10_hbsc_cb/buillying_gender.png",
       plot = p, width = 30, height = 15)


# Comparative map ----
data <- hbsc %>% filter(Sex == "Total")

data <- processing_comparison(data, "Rate")
p <- static_map(data)
ggsave(filename = "graphs/cat10_hbsc_cb/bullying_map.png",
       plot = p, width = 24, height = 12)


# Comments ----
# Average of genders across countries exactly the same
# differences in countries
aggregation <- hbsc %>%
  filter(Sex != "Total") %>%
  group_by(Sex) %>%
  summarise(Average = mean(Value, na.rm = TRUE),
            Median = median(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Sex, values_from = c(Average, Median))

write.csv(aggregation, "graphs/cat10_hbsc_cb/gender_aggregation.csv")

# Calculate absolute and relative differences
gender_diff <- hbsc %>%
  filter(Sex %in% c("Female", "Male")) %>%  # Filter only Male and Female
  spread(Sex, Value) %>%  # Convert to wide format
  mutate(
    Absolute_Difference = abs(Female - Male),
    Relative_Difference = abs(Female - Male) / ((Female + Male) / 2) * 100
  ) %>%
  dplyr::select(Country, Female, Male, Absolute_Difference, Relative_Difference)

# Display results
gender_diff

write.csv(gender_diff, "graphs/cat10_hbsc_cb/gender_diff.csv")


totals <- hbsc %>%
  filter(Sex == "Total" & WealthQuintile == "Total") %>%
  arrange(desc(Value)) %>%
  dplyr::select(Country, Value)


write.csv(totals, "graphs/cat10_hbsc_cb/totals.csv")

## Wealth Quintiles ----

data <- hbsc %>% filter(WealthQuintile != "Total")

wq_comparison <- data %>%
  group_by(WealthQuintile) %>%
  summarize(Mean = mean(Value)) %>% arrange(desc(Mean))

wq_comparison_country <- data %>%
  group_by(Country) %>%
  summarize(STD = sd(Value)) %>% arrange(desc(STD))



write.csv(wq_comparison, "graphs/cat10_hbsc_cb/quintile_comparison.csv")
write.csv(wq_comparison_country, "graphs/cat10_hbsc_cb/quintile_comparison_country.csv")


# Plotting
# Ensure WealthQuintile is treated as a factor
data <- data %>%
  mutate(WealthQuintile = factor(WealthQuintile, levels = c("Lowest", "Middle", "Highest")))

# Define folder path
output_folder <- "graphs/cat10_hbsc_cb/"

# Create the folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Loop through each country
countries <- unique(data$Country)

for (country in countries) {
  # Filter data for the current country
  country_data <- data %>% filter(Country == country)
  
  # Create the bar chart
  plot <- ggplot(country_data, aes(x = WealthQuintile, y = Value, fill = WealthQuintile)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = paste("Differences in Wealth Quintile -", country),
      x = "Wealth Quintile",
      y = "Value",
      fill = "Wealth Quintile"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    ylim(0, max(data$Value, na.rm = TRUE)) # Fixed y-axis limit
  
  # Save the plot in the specified folder
  filename <- paste0(output_folder, "WealthQuintile_", country, ".png")
  ggsave(filename, plot, width = 8, height = 6)
}
