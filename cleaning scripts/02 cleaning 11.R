# Do not clean environment as we need to load for cleaning
source("requirements/libraries.R")
# wb <- createWorkbook()
# file_path <- "data/output/2024_ICVAC_ECARO.xlsx"

# DHS Child discipline ----
df <- folder11_fusion_TRANSMONEE_ECARO_1.0_.PT_VC_SNS_WALN....
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% select(CountryName, ISO3) %>% rename(Country = CountryName)

df %<>%
  mutate(
    ISO3 = substr(`REF_AREA:Geographic area`, 1, 3),  # Extract first 3 characters
    Country = sub(".*: ", "", `REF_AREA:Geographic area`)  # Extract everything after ": "
  )

df %<>%
  mutate(Indicator = "Percentage of population that feel safe walking alone around the area they live",
         Sex = substr(`SEX:Sex`, 1, 3),
         AgeGroup = NA)

df %<>%
  mutate(Sex = case_when(
    grepl("T", Sex, ignore.case = TRUE)  ~  "Total",
    grepl("F", Sex, ignore.case = TRUE)  ~  "Female",
    grepl("M", Sex, ignore.case = TRUE)  ~  "Male",
    TRUE ~ Sex
  ))

df %<>%
  mutate(Dataset = "TMEE-UNICEF-SAFETY",
         Year = `TIME_PERIOD:Time period`,
         ICVACCategory = "Environment",
         Reliability = NA,
         Value = as.numeric(`OBS_VALUE:Observation value`),
         StatisticalUnit = "Rate",
         Comments = `OBS_FOOTNOTE:Observation footnote`,
         Source = `DATA_SOURCE:Data Source`) %>%
  select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit,
         Source, Dataset, ICVACCategory, Comments, Reliability)

write.csv(df, "data/output/disaggregated/icvac_11.csv", row.names = FALSE)
