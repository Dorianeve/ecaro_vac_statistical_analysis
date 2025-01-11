rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")


icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

cat2 <- icvac %>% filter(Indicator == "Ever experienced physical violence since age 15")

# AgeGroup analysis ----
# Rate
# Only one year per country
# exclude aggregated category (same for DHS)
data <- cat2 %>% filter(AgeGroup != "15-49" & !is.na(AgeGroup))

age <- data %>%
  group_by(AgeGroup) %>%
  summarize(Mean = mean(Value)) %>%
  arrange(desc(Mean))
write.csv(age, "analysis/cat2_dhs/age_groups_mean.csv")

# Set the desired order of AgeGroup levels
age_levels <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")

# Ensure that AgeGroup is a factor with the correct level order
data$AgeGroup <- factor(data$AgeGroup, levels = age_levels)

# Set consistent color palette for AgeGroup
# Set consistent color palette for AgeGroup with Vaporwave aesthetics
age_colors <- c(
  "15-19" = "#FF6AD5",   # Bright Pink
  "20-24" = "#FF8B8B",   # Soft Coral
  "25-29" = "#FFC75F",   # Neon Yellow
  "30-34" = "#6A0572",   # Deep Purple
  "35-39" = "#00EAD3",   # Bright Cyan
  "40-44" = "#845EC2",   # Purple
  "45-49" = "#C34A36"    # Dark Red
)
# Get the list of unique countries
countries <- unique(data$Country)

# Create individual plots for each country and save them separately
for (country in countries) {
  
  # Filter data for the specific country
  cat2_age <- data %>%
    filter(Country == country)
  
  # Create the barchart with consistent x, y axis limits, and color palette
  p <- ggplot(cat2_age, aes(x = AgeGroup, y = Value, fill = AgeGroup)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = age_colors) +  # Use the same colors for AgeGroup
    labs(title = paste("DHS - AgeGroup disaggregation -  Women who ever experienced physical violence since age 15 in", country),
         x = "Age Group",
         y = "Rate") +
    theme_minimal() +  # White background and consistent font
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
    scale_y_continuous(limits = c(0, max(data$Value, na.rm = TRUE)))  # Ensure same y scale
  
  # Save each plot as a PNG file
  ggsave(filename = paste0("analysis/cat2_dhs/barchart_", country, ".png"), plot = p, width = 8, height = 6, bg = "white")
  
  # Optionally print the plot in the console to visualize
  print(p)
}

std <- data %>%
  group_by(Country) %>%
  summarise(STD = sd(Value, na.rm = TRUE)) %>%
  arrange(desc(STD)) %>%
  print(n = Inf)

write.csv(std, "analysis/cat2_dhs/age_group_std.csv")
# Aggregated map ----
# Rate
# 18-74 category
# Only one year per country
data <- cat2 %>% filter(AgeGroup == "15-19")

write.csv(data, "analysis/cat2_dhs/aggregated_rate.csv")

# no processing as we have already an aggregated number per latest year
data <- processing_comparison(data, "Rate")
p <- static_map(data)
ggsave(filename = "analysis/cat2_dhs/static_map_with_auto_zoom_adapted_frame.png",
       plot = p, width = 12, height = 12)

# Wealth quintile ----

cat2 <- icvac %>% filter(Indicator == "Ever experienced physical violence since age 15")

# Rate
# Only one year per country
# exclude aggregated category (same for DHS)
data <- cat2 %>% filter(!is.na(WealthQuintile))

# Set the desired order of AgeGroup levels
levels <- c("Lowest" , "Second" , "Middle",  "Fourth" , "Highest")

# Ensure that AgeGroup is a factor with the correct level order
data$WealthQuintile <- factor(data$WealthQuintile, levels = levels)

# Set consistent color palette for AgeGroup
# Set consistent color palette for AgeGroup with Vaporwave aesthetics
colors <- c(
  "Lowest" = "#FF6AD5",   # Bright Pink
  "Second" = "#FF8B8B",   # Soft Coral
  "Middle" = "#FFC75F",   # Neon Yellow
  "Fourth" = "#6A0572",   # Deep Purple
  "Highest" = "#00EAD3"   # Bright Cyan
)
# Get the list of unique countries
countries <- unique(data$Country)

# Create individual plots for each country and save them separately
for (country in countries) {
  
  # Filter data for the specific country
  cat2_wq <- data %>%
    filter(Country == country)
  
  # Create the barchart with consistent x, y axis limits, and color palette
  p <- ggplot(cat2_wq, aes(x = WealthQuintile, y = Value, fill = WealthQuintile)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +  # Use the same colors for AgeGroup
    labs(title = paste("DHS - Wealth Quintile disaggregation - Women who ever experienced physical violence since age 15 in", country),
         x = "Wealth Quintiles",
         y = "Rate") +
    theme_minimal() +  # White background and consistent font
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
    scale_y_continuous(limits = c(0, max(data$Value, na.rm = TRUE)))  # Ensure same y scale
  
  # Save each plot as a PNG file
  ggsave(filename = paste0("analysis/cat2_dhs/quintiles_", country, ".png"), plot = p, width = 8, height = 6, bg = "white")
  
  # Optionally print the plot in the console to visualize
  print(p)
}

std <- data %>%
  group_by(Country) %>%
  summarise(STD = sd(Value, na.rm = TRUE)) %>%
  arrange(desc(STD)) %>%
  print(n = Inf)

write.csv(std, "analysis/cat2_dhs/quintiles_std.csv")
