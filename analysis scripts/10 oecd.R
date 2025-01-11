rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")

# Female / Male
# 15-19,20-24
# One data point
# Use of mean for map and aggregated

icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

df <- icvac %>% filter(Indicator == "Prevalence child marriage")

folder <- "analysis/cat10_oecd/"

# Mean per country and sex
gender <- df %>%
  group_by(Country, Sex) %>%
  summarise(Mean = mean(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from =  Sex, values_from = Mean) %>%
  mutate(Diff = Female - Male)

write.csv(gender, paste0(folder, "gender.csv"))

# Plot
gender_long <- gender %>%
  pivot_longer(cols = c(Female, Male), names_to = "Sex", values_to = "Value")

# Create bar chart
p <- ggplot(gender_long, aes(x = Country, y = Value, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Prevalence of Child Marriage - Sex Disaggregation",
    x = "Country",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = paste0(folder, "child_marriage_sex_disaggregation.png"), # File name
  plot = p,                                           # The plot object
  width = 10,                                         # Width in inches
  height = 6,                                         # Height in inches
  dpi = 300                                           # Resolution in dots per inch
)

age <- df %>%
  group_by(Country, AgeGroup) %>%
  summarise(Mean = mean(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from =  AgeGroup, values_from = Mean) %>%
  mutate(Diff = `15-19` -  `20-24`)
write.csv(age, paste0(folder, "age.csv"))

# Plot
age_long <- age %>%
  pivot_longer(cols = c( `15-19` , `20-24`), names_to = "AgeGroup", values_to = "Value")

# Create bar chart
p <- ggplot(age_long, aes(x = Country, y = Value, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Prevalence of Child Marriage - AgeGroup Disaggregation",
    x = "Country",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = paste0(folder, "child_marriage_agegroup_disaggregation.png"), # File name
  plot = p,                                           # The plot object
  width = 10,                                         # Width in inches
  height = 6,                                         # Height in inches
  dpi = 300                                           # Resolution in dots per inch
)


age_15_19 <- df %>%
  filter(AgeGroup == "15-19") %>%
  group_by(Country) %>%
  summarise(Mean = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(Mean))
write.csv(age_15_19, paste0(folder, "age_15_16.csv"))

age_20_24 <- df %>%
  filter(AgeGroup == "20-24") %>%
  group_by(Country) %>%
  summarise(Mean = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(Mean))
write.csv(age_20_24, paste0(folder, "age_20_24.csv"))

fem <- df %>%
  filter(Sex == "Female") %>%
  group_by(Country) %>%
  summarise(Mean = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(Mean))
write.csv(fem, paste0(folder, "fem.csv"))

male <- df %>%
  filter(Sex == "Male") %>%
  group_by(Country) %>%
  summarise(Mean = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(Mean))
write.csv(male, paste0(folder, "male.csv"))

total <- df %>%
  group_by(Country) %>%
  summarise(Mean = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(Mean))
write.csv(total, paste0(folder, "total.csv"))


total <- df %>%
  group_by(AgeGroup) %>%
  summarise(Mean = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(Mean))
write.csv(total, paste0(folder, "age.csv"))

map <- df %>%
  group_by(ISO3, Indicator) %>%
  summarise(Total = mean(Value, na.rm = TRUE)) 

m <- static_map(map)

# Save the map
ggsave(
  filename =  paste0(folder, "map_visualization.png"), # File name
  plot = m,                           # The map object
  width = 12,                         # Width in inches
  height = 8,                         # Height in inches
  dpi = 300                           # Resolution in dots per inch
)
