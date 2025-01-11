# Do not clean environment as we need to load for cleaning
source("requirements/libraries.R")
# wb <- createWorkbook()
# file_path <- "data/output/2024_ICVAC_ECARO.xlsx"

DHS_PSY_VIOLENCE <- folder4_STATcompilerExport2024917_16214x
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% select(CountryName, ISO3) %>% rename(Country = CountryName)

DHS_PSY_VIOLENCE <- DHS_PSY_VIOLENCE[1:7,]

DHS_PSY_VIOLENCE %<>%
  left_join(iso, by = "Country")

DHS_PSY_VIOLENCE %<>%
  mutate(ISO3 = case_when(
    Country == "Moldova" ~ "MDA",
    Country == "Kyrgyz Republic" ~ "KGZ",
    TRUE ~ ISO3
  ))

# Select correct columns

# DHS_PHYSICAL_VIOLENCE %<>%
#   select(Country, ISO3, Survey,`Total 15-49...3`, `Total 15-49...18`, `Total 15-49...19`, 
#          `Total 15-49...27`, `Total 15-49...35`,
#          `15-19...4`, `15-19...11`, `15-19...20`, `15-19...28`,
#          `15-19...36`)

# DHS_PHYSICAL_VIOLENCE %<>%
#   rename(TOTALsince_15_49 = `Total 15-49...3`,
#          since_15_19 = `15-19...4`,
#          TOTALmonths12often_15_49 = `Total 15-49...18`,
#          months12often_15_19 = `15-19...11`,
#          TOTALmonths12sometimes_15_49 = `Total 15-49...19`,
#          months12sometimes_15_19 = `15-19...20`,
#          TOTALmonths12oftentimes_15_49 = `Total 15-49...27`,
#          months12oftentimes_15_19 = `15-19...28`,
#          womenever_15_49 = `Total 15-49...35`,
#          womenever_15_19 = `15-19...36`)

DHS_PHYSICAL_VIOLENCE %<>%
  rename(
    TOTALsince_15_49 = `Total 15-49...3`,
    since_15_19 = `15-19...4`,
    since_20_24 = `20-24...5`,
    since_25_29 = `25-29...6`,
    since_30_34 = `30-34...7`,
    since_35_39 = `35-39...8`,
    since_40_44 = `40-44...9`,
    since_45_49 = `45-49...10`,
    months12often_15_19 = `15-19...11`,
    months12often_20_24 = `20-24...12`,
    months12often_25_29 = `25-29...13`,
    months12often_30_34 = `30-34...14`,
    months12often_35_39 = `35-39...15`,
    months12often_40_44 = `40-44...16`,
    months12often_45_49 = `45-49...17`,
    TOTALmonths12often_15_49 = `Total 15-49...18`,
    TOTALmonths12sometimes_15_49 = `Total 15-49...19`,
    months12sometimes_15_19 = `15-19...20`,
    months12sometimes_20_24 = `20-24...21`,
    months12sometimes_25_29 = `25-29...22`,
    months12sometimes_30_34 = `30-34...23`,
    months12sometimes_35_39 = `35-39...24`,
    months12sometimes_40_44 = `40-44...25`,
    months12sometimes_45_49 = `45-49...26`,
    TOTALmonths12oftentimes_15_49 = `Total 15-49...27`,
    months12oftentimes_15_19 = `15-19...28`,
    months12oftentimes_20_24 = `20-24...29`,
    months12oftentimes_25_29 = `25-29...30`,
    months12oftentimes_30_34 = `30-34...31`,
    months12oftentimes_35_39 = `35-39...32`,
    months12oftentimes_40_44 = `40-44...33`,
    months12oftentimes_45_49 = `45-49...34`,
    womenever_15_49 = `Total 15-49...35`,
    womenever_15_19 = `15-19...36`,
    womenever_20_24 = `20-24...37`,
    womenever_25_29 = `25-29...38`,
    womenever_30_34 = `30-34...39`,
    womenever_35_39 = `35-39...40`,
    womenever_40_44 = `40-44...41`,
    womenever_45_49 = `45-49...42`,
    ISO3 = ISO3
  )

DHS_PHYSICAL_VIOLENCE %<>%
  pivot_longer(
    cols = -c(Country, Survey, ISO3),  # Include all columns except Country, Survey, and ISO3
    names_to = "Survey_Metric",         # This will be the new column for metric names
    values_to = "Value"                 # This will be the new column for values
  )

DHS_PHYSICAL_VIOLENCE %<>%
  mutate(AgeGroup = case_when(
    grepl("15_49", Survey_Metric, ignore.case = TRUE) ~ "15-49",
    grepl("15_19", Survey_Metric, ignore.case = TRUE) ~ "15-19",
    grepl("20_24", Survey_Metric, ignore.case = TRUE) ~ "20-24",
    grepl("25_29", Survey_Metric, ignore.case = TRUE) ~ "25-29",
    grepl("30_34", Survey_Metric, ignore.case = TRUE) ~ "30-34",
    grepl("35_39", Survey_Metric, ignore.case = TRUE) ~ "35-39",
    grepl("40_44", Survey_Metric, ignore.case = TRUE) ~ "40-44",
    grepl("45_49", Survey_Metric, ignore.case = TRUE) ~ "45-49",
    TRUE ~ NA,
  ))

DHS_PHYSICAL_VIOLENCE %<>%
  mutate(Sex = "Female")

DHS_PHYSICAL_VIOLENCE %<>%
  mutate(Indicator = case_when(
    grepl("since", Survey_Metric, ignore.case = TRUE) ~ "Ever experienced physical violence since age 15",
    grepl("often_", Survey_Metric, ignore.case = TRUE) ~ "Percentage of women who have experienced physical violence in the past 12 months often",
    grepl("sometimes_", Survey_Metric, ignore.case = TRUE) ~ "Percentage of women who have experienced physical violence in the past 12 months sometimes",
    grepl("oftentimes_", Survey_Metric, ignore.case = TRUE) ~ "Percentage of women who have experienced physical violence in the past 12 months often or sometimes",
    grepl("womenever_", Survey_Metric, ignore.case = TRUE) ~ "Percentage of women who ever experienced sexual violence",
    TRUE ~ NA,
  ))

DHS_PHYSICAL_VIOLENCE %<>%
  mutate(Year = case_when(
    grepl("2005", Survey, ignore.case = TRUE) ~ 2005,
    grepl("2006", Survey, ignore.case = TRUE) ~ 2006,
    grepl("2007", Survey, ignore.case = TRUE) ~ 2007,
    grepl("2012", Survey, ignore.case = TRUE) ~ 2012,
    grepl("16", Survey, ignore.case = TRUE) ~ 2016,
    grepl("2017", Survey, ignore.case = TRUE) ~ 2017,
    TRUE ~ NA,
  ))

DHS_PHYSICAL_VIOLENCE %<>%
  rename(Source = Survey) %>%
  select(Country, ISO3, Year, Sex, AgeGroup, Indicator, Value, Source)

DHS_PHYSICAL_VIOLENCE %<>%
  group_by(Country) %>%
  top_n(1, Year) %>%
  ungroup()

DHS_PHYSICAL_VIOLENCE %<>%
  mutate(StatisticalUnit = "Rate")

DHS_PHYSICAL_VIOLENCE %<>%
  select(Country, ISO3, Year, Sex, AgeGroup, Indicator, StatisticalUnit, Value, Source) %>%
  mutate(ICVACCategory = "Physical violence against a child",
         Dataset = "DHS_PHYSICAL_VIOLENCE")
