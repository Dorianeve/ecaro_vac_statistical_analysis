# Do not clean environment as we need to load for cleaning
source("requirements/libraries.R")
source("requirements/dynamic_map.R")
# wb <- createWorkbook()
# file_path <- "data/output/2024_ICVAC_ECARO.xlsx"

UNODC_VICTIM_HOMICIDE <- folder1_data_cts_intentional_homicidex
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")

UNODC_VICTIM_HOMICIDE <- UNODC_VICTIM_HOMICIDE %>%
  rename(ISO3 = Iso3_code)

UNODC_VICTIM_HOMICIDE %<>%
  mutate(Country = case_when(
    grepl("United Kingdom", Country) ~ "United Kingdom",
    TRUE ~ Country
  ))

gbr_counts_aggregated <- UNODC_VICTIM_HOMICIDE %>%
  filter(`Unit of measurement` == "Counts", ISO3 %in% c("GBR_E_W","GBR_NI","GBR_S")) %>%
  group_by(Dimension, Region, Subregion,Indicator,Category, Sex, Age, Year,
           `Unit of measurement`) %>%
  summarise(VALUE = sum(VALUE, na.rm = TRUE),     
            Source = str_c(Source, collapse = "; "),  # Concatenate Source text
            .groups = "drop") %>%
  mutate(ISO3 = "GBR")  # Add GBR as the ISO3 code

# Aggregate rates for GBR
# gbr_rates_aggregated <- UNODC_VICTIM_HOMICIDE %>%
#   filter(`Unit of measurement` != "Counts", ISO3 %in% c("GBR_E_W","GBR_NI","GBR_S")) %>%
#   group_by(Dimension, Region, Subregion,Indicator,Category, Sex, Age, Year,
#            `Unit of measurement`) %>%
#   summarise(VALUE = mean(VALUE, na.rm = TRUE),     
#             Source = str_c(Source, collapse = "; "),  # Concatenate Source text
#             .groups = "drop") %>%
#   mutate(ISO3 = "GBR")  # Add GBR as the ISO3 code

# Combine GBR data with the original dataset
UNODC_VICTIM_HOMICIDE <- UNODC_VICTIM_HOMICIDE %>%
  filter(!ISO3 %in% c("GBR_E_W","GBR_NI","GBR_S")) %>%
  bind_rows(gbr_counts_aggregated)

UNODC_VICTIM_HOMICIDE %<>%
  filter(ISO3 %in% iso$ISO3)

UNODC_VICTIM_HOMICIDE %<>%
         filter(Indicator == "Victims of intentional homicide", 
                Dimension == "by relationship to perpetrator" |
         (Age == "0-9" |
           Age == "10 -14" |
           Age == "15 -17" |        
           Age == "18-19"))

UNODC_VICTIM_HOMICIDE %<>%
  mutate(Age = case_when(
    Age == "10 -14" ~ "10-14",
      Age == "15 -17" ~ "15-17",
      TRUE ~ Age))


UNODC_VICTIM_HOMICIDE %<>%
  mutate(Value = as.numeric(VALUE))

UNODC_VICTIM_HOMICIDE %<>%
  dplyr::select(ISO3, Country, Indicator, Sex, Age, Year, `Unit of measurement`, Dimension, Category, Value, Source) %>%
  mutate(
    ICVACCategory = "Violent killing of a child",
    Dataset = "UNODC-VICTIM-HOMICIDE",
    Dimension = Category  # Correct column name
  )

UNODC_VICTIM_HOMICIDE %<>%
  mutate(Age = trimws(Age)) %>%
  rename(AgeGroup = Age,
         StatisticalUnit = `Unit of measurement`,
         RelationshipPerpetrator = Dimension) %>%
  dplyr::select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit, Value, RelationshipPerpetrator, Source, Dataset, ICVACCategory)
  
UNODC_VICTIM_HOMICIDE %<>%
  mutate(Country = case_when(
    grepl("GBR", ISO3) ~ "United Kingdom",
    TRUE ~ Country
  ))

# Save in folder ----
write.csv(UNODC_VICTIM_HOMICIDE, "data/output/disaggregated/icvac_1.csv", row.names = FALSE)

