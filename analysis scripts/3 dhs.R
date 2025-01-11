rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")


# Rate
# Only Female
# AgeGroup "15-19","20-24","25-29","30-34","35-39","40-44","45-49"
# Using 15-19 for map

icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

cat3 <- icvac %>% filter(Indicator == "Women who ever experienced sexual violence")

# AgeGroup analysis ----
# Rate
# Only one year per country
# exclude aggregated category
data <- cat3 %>% filter(AgeGroup != "15-49" & !is.na(AgeGroup))

ag_comparison <- data %>%
  group_by(AgeGroup) %>%
  summarize(Mean = mean(Value)) %>%
  arrange(desc(Mean))

write.csv(ag_comparison, "analysis/cat3_dhs/age_group_comparison.csv")

# Set the desirMean# Set the desired order of AgeGroup levels
age_levels <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")

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

p <- ggplot(ag_comparison, aes(x = AgeGroup, y = Mean, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = age_colors) +  # Use the same colors for AgeGroup
  labs(title = paste("DHA - Women who ever experienced sexual violence - ALL Countries"),
       x = "Age Group",
       y = "Rate") +
  theme_minimal() +  # White background and consistent font
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
  scale_y_continuous(limits = c(0, max(data$Value, na.rm = TRUE)))  # Ensure same y scale

# Save each plot as a PNG file
ggsave(filename = paste0("analysis/cat3_dhs/barchart_AGE_ALL.png"), plot = p, width = 8, height = 6, bg = "white")



# Ensure that AgeGroup is a factor with the correct level order
data$AgeGroup <- factor(data$AgeGroup, levels = age_levels)


# Get the list of unique countries
countries <- unique(data$Country)

# Create individual plots for each country and save them separately
for (country in countries) {
  
  # Filter data for the specific country
  cat3_age <- data %>%
    filter(Country == country)
  
  # Create the barchart with consistent x, y axis limits, and color palette
  p <- ggplot(cat3_age, aes(x = AgeGroup, y = Value, fill = AgeGroup)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = age_colors) +  # Use the same colors for AgeGroup
    labs(title = paste("DHA - Women who ever experienced sexual violence in", country),
         x = "Age Group",
         y = "Rate") +
    theme_minimal() +  # White background and consistent font
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
    scale_y_continuous(limits = c(0, max(data$Value, na.rm = TRUE)))  # Ensure same y scale
  
  # Save each plot as a PNG file
  ggsave(filename = paste0("analysis/cat3_dhs/barchart_", country, ".png"), plot = p, width = 8, height = 6, bg = "white")
  
  # Optionally print the plot in the console to visualize
  print(p)
}

std <- data %>%
group_by(Country) %>%
  summarise(STD = sd(Value, na.rm = TRUE)) %>%
  arrange(desc(STD)) %>%
  print(n = Inf)

write.csv(std, "analysis/cat3_dhs/age_group_std.csv")

# Aggregated map ----
# Rate
# 18-74 category
# Only one year per country
data <- cat3 %>% filter(AgeGroup == "15-19")

write.csv(data, "analysis/cat3_dhs/aggregated_rate.csv")
# no processing as we have already an aggregated number per latest year
data <- processing_comparison(data, "Rate")
p <- static_map(data)
ggsave(filename = "analysis/cat3_dhs/static_map_with_auto_zoom_adapted_frame.png",
       plot = p, width = 12, height = 12)

# Wealth quintile ----

cat3 <- icvac %>% filter(Indicator == "Women who ever experienced sexual violence")

# Rate
# Only one year per country
# exclude aggregated category (same for DHS)
data <- cat3 %>% filter(!is.na(WealthQuintile))

wq_comparison <- data %>%
  group_by(WealthQuintile) %>%
  summarize(Mean = mean(Value,  na.rm = TRUE)) %>% arrange(desc(Mean))

write.csv(wq_comparison, "analysis/cat3_dhs/quintile_comparison.csv")

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
  cat3_wq <- data %>%
    filter(Country == country)
  
  # Create the barchart with consistent x, y axis limits, and color palette
  p <- ggplot(cat3_wq, aes(x = WealthQuintile, y = Value, fill = WealthQuintile)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +  # Use the same colors for AgeGroup
    labs(title = paste("DHS - Women who ever experienced sexual violence in", country),
         x = "Wealth Quintiles",
         y = "Rate") +
    theme_minimal() +  # White background and consistent font
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
    scale_y_continuous(limits = c(0, max(data$Value, na.rm = TRUE)))  # Ensure same y scale
  
  # Save each plot as a PNG file
  ggsave(filename = paste0("analysis/cat3_dhs/quintiles_", country, ".png"), plot = p, width = 8, height = 6, bg = "white")
  
  # Optionally print the plot in the console to visualize
  print(p)
}

std <- data %>%
  group_by(Country) %>%
  summarise(STD = sd(Value, na.rm = TRUE)) %>%
  arrange(desc(STD)) %>%
  print(n = Inf)

write.csv(std, "analysis/cat3_dhs/quintiles_std.csv")

wq_comparison$WealthQuintile <- factor(wq_comparison$WealthQuintile, levels = levels)
# Create the barchart with consistent x, y axis limits, and color palette
p <- ggplot(wq_comparison, aes(x = WealthQuintile, y = Mean, fill = WealthQuintile)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +  # Use the same colors for AgeGroup
  labs(title = paste("DHA - Women who ever experienced sexual violence - ALL Countries"),
       x = "WealthQuintile",
       y = "Rate") +
  theme_minimal() +  # White background and consistent font
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
  scale_y_continuous(limits = c(0, max(data$Value, na.rm = TRUE)))  # Ensure same y scale

# Save each plot as a PNG file
ggsave(filename = paste0("analysis/cat3_dhs/barchart_WealthQuintile_ALL.png"), plot = p, width = 8, height = 6, bg = "white")

# Optionally print the plot in the console to visualize
print(p)
