rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")

# Only F
# "18-29", "30-44", "45-64", "65-74", "18-74"
# 18-74 utilized as total
# No need to use processing mean


icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

# AgeDisaggregation ----
# Rate
# against 18-74 category
# Only one year per country
eurostat <- icvac %>% filter(ICVACCategory == "Sexual violence against a child" &
                               Indicator == "Women who have experienced sexual violence during childhood"
                             & is.na(RelationshipPerpetrator))


age <- eurostat %>%
  group_by(AgeGroup) %>%
  summarize(Total = mean(Value, na.rm = TRUE))

write.csv(age, "analysis/cat3_eurostat/age_group_mean.csv")


# Set the desired order of AgeGroup levels
age_levels <- c("18-29", "30-44", "45-64", "65-74", "18-74")

# Ensure that AgeGroup is a factor with the correct level order
eurostat$AgeGroup <- factor(eurostat$AgeGroup, levels = age_levels)

# Set consistent color palette for AgeGroup
age_colors <- c("18-29" = "#1f77b4", "30-44" = "#ff7f0e", "45-64" = "#2ca02c", "65-74" = "#d62728", "18-74" = "#9467bd")

# Get the list of unique countries
countries <- unique(eurostat$Country)

# Create individual plots for each country and save them separately
for (country in countries) {
  
  # Filter data for the specific country
  cat3_age <- eurostat %>%
    filter(Country == country)
  
  # Create the barchart with consistent x, y axis limits, and color palette
  p <- ggplot(cat3_age, aes(x = AgeGroup, y = Value, fill = AgeGroup)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = age_colors) +  # Use the same colors for AgeGroup
    labs(title = paste("Age Group - Women who have experienced sexual violence during childhood in", country),
         x = "Age Group",
         y = "Rate") +
    theme_minimal() +  # White background and consistent font
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
    scale_y_continuous(limits = c(0, max(eurostat$Value, na.rm = TRUE)))  # Ensure same y scale
  
  # Save each plot as a PNG file
  ggsave(filename = paste0("analysis/cat3_eurostat/barchart_", country, ".png"), plot = p, width = 8, height = 6, bg = "white")
  
  # Optionally print the plot in the console to visualize
  print(p)
}

# get std deviation excluding aggregated to get the dispersion and potential flags
tab3 <- eurostat %>% filter(AgeGroup != "18-74")

std <- tab3 %>%
  group_by(Country) %>%
  summarise(STD = sd(Value, na.rm = TRUE)) %>%
  arrange(desc(STD)) %>%
  print(n = Inf)

write.csv(std, "analysis/cat3_eurostat/age_group_std.csv")


mean <- eurostat %>%
  group_by(AgeGroup) %>%
  summarise(Mean = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(Mean))

write.csv(std, "analysis/cat3_eurostat/age_group_mean.csv")


p <- ggplot(mean, aes(x = AgeGroup, y = Mean, 
                        fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = age_colors) +  # Use the same colors for AgeGroup
  labs(title = paste("AgeGroup - Women who have experienced sexual violence during childhood - All countries", country),
       x = "AgeGroup",
       y = "Rate") +
  theme_minimal() +  # White background and consistent font
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
  scale_y_continuous(limits = c(0, max(eurostat$Value, na.rm = TRUE)))  # Ensure same y scale

# Save each plot as a PNG file
ggsave(filename = paste0("analysis/cat3_eurostat/barchart_AgeGroup_ALL.png"), plot = p, width = 8, height = 6, bg = "white")

# Optionally print the plot in the console to visualize
print(p)



# Aggregated map ----
# Rate
# 18-74 category
# Only one year per country
eurostat <- icvac %>% filter(ICVACCategory == "Sexual violence against a child" &
                               Indicator == "Women who have experienced sexual violence during childhood" &
                               AgeGroup == "18-74" &
                               is.na(RelationshipPerpetrator))

# no processing as we have already an aggregated number per latest year
cat3 <- processing_comparison(eurostat, "Rate")
p <- static_map(cat3)
ggsave(filename = "analysis/cat3_eurostat/static_map_with_auto_zoom_adapted_frame.png",
       plot = p, width = 12, height = 12)

write.csv(cat3, "analysis/cat3_eurostat/aggregated_rate.csv")



# Perpetrator ----
# Rate
# against 18-74 category
# Only one year per country
eurostat <- icvac %>% filter(ICVACCategory == "Sexual violence against a child" &
                               Indicator == "Women who have experienced sexual violence during childhood"
                             & !is.na(RelationshipPerpetrator))

# Set the desired order of AgeGroup levels
levels <- c("Perpetrators (F/M)", "Perpetrators (M)", 
                "Family member or relative (F/M)", 
                "Any known (F/M)", 
                "Other known (non-family or relative - F/M)",
                "Stranger (F/M)"
                )

# Ensure that AgeGroup is a factor with the correct level order
eurostat$AgeGroup <- factor(eurostat$RelationshipPerpetrator, levels = levels)

# Set consistent color palette for AgeGroup
colors <- c("Perpetrators (F/M)" = "#1f77b4", "Perpetrators (M)" = "#ff7f0e", 
                "Family member or relative (F/M)" = "#2ca02c", 
                "Any known (F/M)" = "#d62728", 
                "Other known (non-family or relative - F/M)" = "#9467bd",
                "Stranger (F/M)" = "#00EAD3"
                )

# Get the list of unique countries
countries <- unique(eurostat$Country)

# Create individual plots for each country and save them separately
for (country in countries) {
  
  # Filter data for the specific country
  cat3_perp <- eurostat %>%
    filter(Country == country)
  
  # Create the barchart with consistent x, y axis limits, and color palette
  p <- ggplot(cat3_perp, aes(x = RelationshipPerpetrator, y = Value, 
                             fill = RelationshipPerpetrator)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +  # Use the same colors for AgeGroup
    labs(title = paste("Perpetrator - Women who have experienced sexual violence during childhood in", country),
         x = "Perpetrator",
         y = "Rate") +
    theme_minimal() +  # White background and consistent font
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
    scale_y_continuous(limits = c(0, max(eurostat$Value, na.rm = TRUE)))  # Ensure same y scale
  
  # Save each plot as a PNG file
  ggsave(filename = paste0("analysis/cat3_eurostat/barchart_Perpetrator", country, ".png"), plot = p, width = 8, height = 6, bg = "white")
  
  # Optionally print the plot in the console to visualize
  print(p)
}

# get std deviation excluding aggregated to get the dispersion and potential flags
tab3 <- eurostat

std <- tab3 %>%
  group_by(Country) %>%
  summarise(STD = sd(Value, na.rm = TRUE)) %>%
  arrange(desc(STD)) %>%
  print(n = Inf)

write.csv(std, "analysis/cat3_eurostat/perpetrator_std.csv")

totals <- eurostat %>%
  group_by(RelationshipPerpetrator) %>%
  summarize(Total = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(Total))
totals


p <- ggplot(totals, aes(x = RelationshipPerpetrator, y = Total, 
                           fill = RelationshipPerpetrator)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +  # Use the same colors for AgeGroup
  labs(title = paste("Perpetrator - Women who have experienced sexual violence during childhood - All countries", country),
       x = "Perpetrator",
       y = "Rate") +
  theme_minimal() +  # White background and consistent font
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white", color = NA)) +  # Set white background
  scale_y_continuous(limits = c(0, max(eurostat$Value, na.rm = TRUE)))  # Ensure same y scale

# Save each plot as a PNG file
ggsave(filename = paste0("analysis/cat3_eurostat/barchart_Perpetrator_ALL.png"), plot = p, width = 8, height = 6, bg = "white")

# Optionally print the plot in the console to visualize
print(p)

write.csv(totals, "analysis/cat3_eurostat/perpetrator_totals.csv")
