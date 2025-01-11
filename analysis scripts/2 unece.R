rm(list = ls())
source("requirements/libraries.R")
source("requirements/load_requirements.R")
source("requirements/static_map.R")
source("requirements/processing_comparison.R")
source("requirements/has_consecutive_data.R")

# Female
# 15+

icvac <- read.csv("data/output/icvac.csv", encoding = "UTF-8")

cat2 <- icvac %>% filter(Indicator == "Women & girls subjected to physical and/or sexual violence")

data <- cat2 %>%
  arrange(desc(Value)) %>%
  dplyr::select(Country, ISO3, Value)

data <- processing_comparison(cat2, "Rate")
p <- static_map(data)
ggsave(filename = "analysis/cat2_UNECE/static_map_with_auto_zoom_adapted_frame.png",
       plot = p, width = 12, height = 12)

write.csv(data, "graphs/cat2_UNECE/rate.csv" )
print(p)
