rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")

# Female
# 15-19
# Diffentent than UNECE cat2, as this focused on minors,
# UNECE instead focuses on 15+
icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

df <- icvac %>% filter(Indicator == "Percentage of ever-partnered girls aged 15–19 years who have experienced physical and/or sexual violence by a current or former intimate partner during the last twelve months.")

data <- processing_comparison(df, "Rate")
p <- static_map(data)
ggsave(filename = "analysis/cat2_unicef/static_map_with_auto_zoom_adapted_frame.png",
       plot = p, width = 12, height = 12)

df %<>%
  select(Country, ISO3, Value, Year) %>%
  arrange(desc(Value))



folder<- "analysis/cat2_unicef/"

write.csv(df, paste0(folder,"countries.csv"))
