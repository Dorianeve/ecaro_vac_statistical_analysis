rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")

# Folder path
folder <- "analysis/cat11_MICS/"

#  15-19 (disaggregated)
# Male / Female 
# Quintiles / Displacement / Settlement
# Roma taken out

icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

mics <- icvac %>% filter(Indicator == "Attitude towards domestic violence")

df <- mics %<>%
  filter(AgeGroup == "15-49")

df %<>%
  mutate(Value = case_when(
    Comments == "Not available/applicable" ~ NA,
    TRUE ~ Value
  ))

df %<>%
  filter(!grepl("roma", Source, ignore.case = TRUE) &
           !is.na(Year)) %>%  # Exclude rows with "roma" in Source
  group_by(Country) %>%                                  # Group by Country
  filter(Year == max(Year)) %>%                          # Keep rows with the latest Year
  ungroup()     

# Totals 15-49 ----
total_mean <- df %>%
  group_by(Country, Year) %>%
  summarise(AverageMF = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(AverageMF))
total_mean

map <- df %>%
  group_by(ISO3, Year, Indicator) %>%
  summarise(Total = mean(Value, na.rm = TRUE))
map

m <- static_map(map)

ggsave(
  filename =  paste0(folder, "map_visualization.png"), # File name
  plot = m,                           # The map object
  width = 12,                         # Width in inches
  height = 8,                         # Height in inches
  dpi = 300                           # Resolution in dots per inch
)


# Gender 15-49 ----
sex_mean <- df %>%
  group_by(Country, Sex) %>%
  summarise(Average = mean(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Sex, values_from = Average) %>%
  mutate(SexDifference = Female - Male) %>%
  arrange(desc(abs(SexDifference)))
sex_mean


# Plot
gender_long <- sex_mean %>%
  pivot_longer(cols = c(Female, Male), names_to = "Sex", values_to = "Value")

# Create bar chart
p <- ggplot(gender_long, aes(x = Country, y = Value, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Attitude towards domestic violence - Sex Disaggregation",
    x = "Country",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = paste0(folder, "domestic_violence_sex_disaggregation.png"), # File name
  plot = p,                                           # The plot object
  width = 10,                                         # Width in inches
  height = 6,                                         # Height in inches
  dpi = 300                                           # Resolution in dots per inch
)

# AgeGroups
mics <- icvac %>% filter(Indicator == "Attitude towards domestic violence")

df <- mics %<>%
  filter(!is.na(AgeGroup) & AgeGroup != "15-49")

df %<>%
  mutate(Value = case_when(
    Comments == "Not available/applicable" ~ NA,
    TRUE ~ Value
  ))

df %<>%
  filter(!grepl("roma", Source, ignore.case = TRUE) &
           !is.na(Year)) %>%  # Exclude rows with "roma" in Source
  group_by(Country) %>%                                  # Group by Country
  filter(Year == max(Year)) %>%                          # Keep rows with the latest Year
  ungroup()   

age_group_country <- df %>%
  group_by(Country, AgeGroup) %>%
  summarise(Average = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = AgeGroup, values_from = Average) %>%
  rowwise() %>% # Apply row-wise operations
  mutate(Max = max(c_across(where(is.numeric)), na.rm = TRUE),
         STD = sd(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup() # Ungroup after rowwise operations
age_group_country

age_long <- age_group_country %>%
  pivot_longer(cols = c( `15-19` , `20-24`,
                         `25-29`, `30-34`, `35-39`, 
                         `40-44`, `45-49`), names_to = "AgeGroup", values_to = "Value")

# Create bar chart
p <- ggplot(age_long, aes(x = Country, y = Value, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Attitude towards domestic violence - AgeGroup Disaggregation",
    x = "Country",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = paste0(folder, "domestic_violence_agegroup_disaggregation.png"), # File name
  plot = p,                                           # The plot object
  width = 10,                                         # Width in inches
  height = 6,                                         # Height in inches
  dpi = 300                                           # Resolution in dots per inch
)



age_group <- df %>%
  group_by(AgeGroup) %>%
  summarise(Average = mean(Value, na.rm = TRUE), .groups = "drop")
age_group

# Other variables ----
mics <- icvac %>% filter(Indicator == "Attitude towards domestic violence")
df <- mics %<>%
  filter(is.na(AgeGroup))

df %<>%
  mutate(Value = case_when(
    Comments == "Not available/applicable" ~ NA,
    TRUE ~ Value
  ))

df %<>%
  filter(!grepl("roma", Source, ignore.case = TRUE) &
           !is.na(Year)) %>%  # Exclude rows with "roma" in Source
  group_by(Country) %>%                                  # Group by Country
  filter(Year == max(Year)) %>%                          # Keep rows with the latest Year
  ungroup()     


df %<>%
  mutate(WealthQuintile = ifelse(WealthQuintile == "Poorest (Q1)", "Lowest", WealthQuintile),
         WealthQuintile = ifelse(WealthQuintile == "Richest (Q5)", "Highest", WealthQuintile)) 

groups <- df %>%
  group_by(WealthQuintile, IDPstatus, SettlementType) %>%
  summarise(Average = mean(Value, na.rm = TRUE))
groups

# List of objects
obj <- list(
  age_group = age_group,
  age_group_country = age_group_country,
  total_mean = total_mean,
  sex_mean = sex_mean,
  groups = groups
)


# Loop through the list
for (name in names(obj)) {
  write.csv(obj[[name]], paste0(folder, name, ".csv"), row.names = FALSE)
}
