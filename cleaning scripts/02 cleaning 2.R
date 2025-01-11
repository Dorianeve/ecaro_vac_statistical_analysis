# Do not clean environment as we need to load for cleaning
source("requirements/libraries.R")
# wb <- createWorkbook()
# file_path <- "data/output/2024_ICVAC_ECARO.xlsx"

DHS_PHYSICAL_VIOLENCE <- folder2_STATcompilerExport2024917_152648x
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% 
  rename(Country = CountryName) %>%
  select(Country, ISO3)

DHS_PHYSICAL_VIOLENCE <- DHS_PHYSICAL_VIOLENCE[1:7,]

DHS_PHYSICAL_VIOLENCE %<>%
  left_join(iso, by = "Country")

DHS_PHYSICAL_VIOLENCE %<>%
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

# Quintile ----
DHS_PHYSICAL_VIOLENCE_quint <- folder2_STATcompilerExport2024111_163615x
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% 
  rename(Country = CountryName) %>%
  select(Country, ISO3)

DHS_PHYSICAL_VIOLENCE_quint <- DHS_PHYSICAL_VIOLENCE_quint[1:7,]

DHS_PHYSICAL_VIOLENCE_quint %<>%
  left_join(iso, by = "Country")

DHS_PHYSICAL_VIOLENCE_quint %<>%
  mutate(ISO3 = case_when(
    Country == "Moldova" ~ "MDA",
    Country == "Kyrgyz Republic" ~ "KGZ",
    TRUE ~ ISO3
  ))


DHS_PHYSICAL_VIOLENCE_quint %<>%
  rename(
    TOTAL_15_49 = `Total 15-49...3`,
    Lowest = Lowest...4,
    Second = Second...5,
    Middle = Middle...6,
    Fourth = Fourth...7,
    Highest = Highest...8,
    ISO3 = ISO3)

DHS_PHYSICAL_VIOLENCE_quint %<>%
  select(Country, Survey, ISO3, Lowest, Second, Middle, Fourth, Highest)


DHS_PHYSICAL_VIOLENCE_quint %<>%
  pivot_longer(
    cols = -c(Country, Survey, ISO3),  # Include all columns except Country, Survey, and ISO3
    names_to = "Survey_Metric",         # This will be the new column for metric names
    values_to = "Value"                 # This will be the new column for values
  )

DHS_PHYSICAL_VIOLENCE_quint %<>%
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


DHS_PHYSICAL_VIOLENCE_quint %<>%
  mutate(Sex = "Female") %>%
  rename(WealthQuintile = Survey_Metric)

DHS_PHYSICAL_VIOLENCE_quint %<>%
  mutate(Indicator = "Ever experienced physical violence since age 15")

DHS_PHYSICAL_VIOLENCE_quint %<>%
  mutate(Year = case_when(
    grepl("2005", Survey, ignore.case = TRUE) ~ 2005,
    grepl("2006", Survey, ignore.case = TRUE) ~ 2006,
    grepl("2007", Survey, ignore.case = TRUE) ~ 2007,
    grepl("2012", Survey, ignore.case = TRUE) ~ 2012,
    grepl("16", Survey, ignore.case = TRUE) ~ 2016,
    grepl("2017", Survey, ignore.case = TRUE) ~ 2017,
    TRUE ~ NA,
  ))

DHS_PHYSICAL_VIOLENCE_quint %<>%
  rename(Source = Survey) %>%
  select(Country, ISO3, Year, Sex, AgeGroup, WealthQuintile, Indicator, Value, Source)

DHS_PHYSICAL_VIOLENCE_quint %<>%
  group_by(Country) %>%
  top_n(1, Year) %>%
  ungroup()

DHS_PHYSICAL_VIOLENCE_quint %<>%
  mutate(StatisticalUnit = "Rate")

DHS_PHYSICAL_VIOLENCE_quint %<>%
  select(Country, ISO3, Year, Sex, AgeGroup, WealthQuintile, Indicator, StatisticalUnit, Value, Source) %>%
  mutate(ICVACCategory = "Physical violence against a child",
         Dataset = "DHS_PHYSICAL_VIOLENCE")

# merge DHS
DHS_PHYSICAL_VIOLENCE %<>%
  mutate(WealthQuintile = NA)

DHS_PHYSICAL_VIOLENCE <- rbind(DHS_PHYSICAL_VIOLENCE, DHS_PHYSICAL_VIOLENCE_quint)

# addWorksheet(wb, "ICVAC2")
# writeData(wb, sheet = "ICVAC2", DHS_PHYSICAL_VIOLENCE, colNames = TRUE, rowNames = FALSE)
# saveWorkbook(wb, file_path , overwrite = TRUE)

# Save in folder ----
#write.csv(DHS_PHYSICAL_VIOLENCE, "data/output/disaggregated/icvac_2.csv", row.names = FALSE)


## Analysis ----
### Comparison ----
data <- DHS_PHYSICAL_VIOLENCE

data %<>%
  filter(Indicator == "Ever experienced physical violence since age 15"  &
           AgeGroup == "15-19")
# dynamic_map(data)

### Comparison women ----
data <- DHS_PHYSICAL_VIOLENCE

data %<>%
  filter(Indicator == "Percentage of women who ever experienced sexual violence" &
           AgeGroup == "15-19")

# dynamic_map(data)


# Women physical and sexual violence UNECE ----


UNECE_SDG5.2.1 <- folder2_c0001024_20240916.182251

# Apply separate() to each column that has comma-separated values
for (col in names(UNECE_SDG5.2.1)) {
  if (any(grepl(",", UNECE_SDG5.2.1[[col]]))) {
    UNECE_SDG5.2.1 <- separate(UNECE_SDG5.2.1, col, 
                               into = paste0(col, "_", seq_len(max(stringr::str_count(UNECE_SDG5.2.1[[col]], ",") + 1))),
                               sep = ",", fill = "right")
  }
}


names(UNECE_SDG5.2.1) <- UNECE_SDG5.2.1[1,]

UNECE_SDG5.2.1 <- UNECE_SDG5.2.1[-1,]

UNECE_SDG5.2.1 %<>%
  left_join(iso, by = "Country")

UNECE_SDG5.2.1 %<>%
  mutate(ISO3 = case_when(
    Country == "Turkiye" ~ "TUR",
    Country == "United Kingdom" ~ "GBR",
    TRUE ~ ISO3
  ))

UNECE_SDG5.2.1 %<>%
  filter(ISO3 %in% iso$ISO3 & !is.na(ISO3))

UNECE_SDG5.2.1 %<>%
  select(Country, ISO3, Indicator, `2018`) %>%
  pivot_longer(cols = `2018`,
               names_to = "Year",
               values_to = "Value")

UNECE_SDG5.2.1 %<>%
  mutate(Indicator = "Women & girls subjected to physical and/or sexual violence",
         SDG = "5.2.1",
         ICVACCategory = "Physical violence against a child",
         Dataset = "UNECE_SDG5.2.1",
         Source = "UNECE",
         StatisticalUnit = "Rate")

UNECE_SDG5.2.1 %<>%
  mutate(Value = as.numeric(Value))

# dynamic_map(UNECE_SDG5.2.1)

DHS_PHYSICAL_VIOLENCE %<>%
  mutate(SDG = NA)

UNECE_SDG5.2.1 %<>%
  mutate(Sex = "Female",
         AgeGroup = "15+",
         WealthQuintile = NA)

ind_2 <- rbind(DHS_PHYSICAL_VIOLENCE, UNECE_SDG5.2.1)

ind_2 %<>% select(-SDG)



write.csv(ind_2, "data/output/disaggregated/icvac_2.csv", row.names = FALSE)
