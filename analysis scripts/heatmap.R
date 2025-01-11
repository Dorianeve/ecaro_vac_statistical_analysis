rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")
source("requirements/filter_latest_consecutive_data.R")


icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")


icvac <- icvac %>%
  mutate(ICVACCategory = case_when(
    ICVACCategory == "Composite phoenomena" ~ 10,
    ICVACCategory == "Other acts of violence against a child not elsewhere classified" ~ 9,
    ICVACCategory == "Physical violence against a child" ~ 2,
    ICVACCategory == "Sexual violence against a child" ~ 3,
    ICVACCategory == "Violent killing of a child" ~ 1,
    TRUE ~ NA_integer_  # Use NA_integer_ for missing values in an integer column
  ))
summary <- icvac %>%
  group_by(Country, ICVACCategory) %>%
  summarise(Presence = n()) %>%
  pivot_wider(names_from = ICVACCategory, values_from = Presence)


# Convert the summary data back to long format for ggplot
long_summary <- summary %>%
  pivot_longer(-Country, names_to = "ICVACCategory", values_to = "Presence")

# Create the heatmap using ggplot2
p <- ggplot(long_summary, aes(x = Country, y = ICVACCategory, fill = Presence)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "blue") +
  theme_minimal() +
  labs(
    title = "ICVACCategory Presence by Country",
    x = "ICVAC Category",
    y = "Country",
    fill = "Presence"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

ggsave(filename = "analysis/heatmap.png",
       plot = p, width = 24, height = 12)

