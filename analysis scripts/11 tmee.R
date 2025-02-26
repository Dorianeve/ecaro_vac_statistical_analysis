rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")

folder <- "analysis/cat11_tm/"

# Female / Male / Total
# Multiple years
# Percentage

icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

df <- icvac %>% filter(Indicator == "Percentage of population that feel safe walking alone around the area they live")

# Gender Totals ----
df %<>% filter(Sex != "Total")

df %<>%
  group_by(Country, Sex) %>%                # Group by Country
  slice_max(Year, with_ties = FALSE)   # Select the row with the latest Year per group

df %<>%
  group_by(Country, Sex) %>%
  summarise(Value = mean(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Sex, values_from = Value) %>%
  mutate(Diff = Female - Male)

gender_long <- df %>%
  pivot_longer(cols = c(Female, Male), names_to = "Sex", values_to = "Value")

# Create bar chart
p <- ggplot(gender_long, aes(x = Country, y = Value, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percentage of population that feel safe walking alone around the area they live - Sex Disaggregation",
    x = "Country",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = paste0(folder, "safety_sex_disaggregation.png"), # File name
  plot = p,                                           # The plot object
  width = 10,                                         # Width in inches
  height = 6,                                         # Height in inches
  dpi = 300                                           # Resolution in dots per inch
)




write.csv(df, paste0(folder, "gender_totals_latest.csv"))

df <- icvac %>% filter(Indicator == "Percentage of population that feel safe walking alone around the area they live")

# Gender Totals over time ----
df %<>% filter(Sex != "Total")

df %<>%
  group_by(Country, Sex) %>%                # Group by Country
  summarize(MeanOverTime = mean(Value, na.rm = TRUE))  %>%
  pivot_wider(names_from = Sex, values_from = MeanOverTime) %>%
  mutate(Diff = Female - Male)# Select the row with the latest Year per group


folder <- "analysis/cat11_tm/"

write.csv(df, paste0(folder, "gender_totals_overtime.csv"))

# Totals -----
df <- icvac %>% filter(Indicator == "Percentage of population that feel safe walking alone around the area they live")


df %<>% filter(Sex == "Total")

map <- df %<>%
  group_by(ISO3) %>%                # Group by Country
  slice_max(Year, with_ties = FALSE)  

map %<>%
  group_by(ISO3, Indicator) %>%
  summarize(Total = mean(Value))

m <- static_map(map)

ggsave(
  filename = paste0(folder, "safety_map.png"), # File name
  plot = m,                                           # The plot object
  width = 10,                                         # Width in inches
  height = 6,                                         # Height in inches
  dpi = 300                                           # Resolution in dots per inch
)

df %<>%
  group_by(Country) %>%                # Group by Country
  slice_max(Year, with_ties = FALSE)   # Select the row with the latest Year per group

df %<>%
  select(Country, ISO3, Year, Value)

folder <- "analysis/cat11_tm/"

write.csv(df, paste0(folder, "country_totals_latest.csv"))

df <- icvac %>% filter(Indicator == "Percentage of population that feel safe walking alone around the area they live")

# Totals over time ----
df %<>% filter(Sex == "Total")

df %<>%
  group_by(Country) %>%                # Group by Country
  summarize(MeanOverTime = mean(Value, na.rm = TRUE)) # Select the row with the latest Year per group

df %<>%
  select(Country, MeanOverTime)

folder <- "analysis/cat11_tm/"

write.csv(df, paste0(folder, "country_totals_overtime.csv"))
