rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")

# 13-15 not disaggregated
# Female / Male / Total
# Only 2018

icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

# Gender ----
# Rate
# Only one year per country
hbsc <- icvac %>% filter(ICVACCategory == "Composite phoenomena" &
                             Indicator == "Percentage of students (13-15 years) who reported being bullied on 1 or more days in the past 30 days")

data <- hbsc %>% filter(Sex != "Total")

p <- ggplot(data, aes(x = Country, y = Value, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sex disaggregation by Country (latest data) - Bullying",
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
ggsave(filename = "analysis/cat10_hbsc/buillying_gender.png",
       plot = p, width = 30, height = 15)


# Comparative map ----
data <- hbsc %>% filter(Sex == "Total")

data <- processing_comparison(data, "Rate")
p <- static_map(data)
ggsave(filename = "analysis/cat10_hbsc/bullying_map.png",
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

write.csv(aggregation, "analysis/cat10_hbsc/gender_aggregation.csv")

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

write.csv(gender_diff, "analysis/cat10_hbsc/gender_diff.csv")


#  Lithuania, Latvia, Moldova, Ukraine > 50%
totals <- hbsc %>%
  filter(Sex == "Total") %>%
  arrange(desc(Value)) %>%
  dplyr::select(Country, Value)


write.csv(totals, "analysis/cat10_hbsc/totals.csv")
