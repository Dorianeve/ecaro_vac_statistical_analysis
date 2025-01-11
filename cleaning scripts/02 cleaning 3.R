# Do not clean environment as we need to load for cleaning
source("requirements/libraries.R")
# wb <- createWorkbook()
# file_path <- "data/output/2024_ICVAC_ECARO.xlsx"

EUROSTAT_GBV_CH <- folder3_estat_gbv_ch_age
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% 
  rename(Country = CountryName) %>%
  select(Country, ISO2, ISO3)

# Apply separate() to each column that has comma-separated values
for (col in names(EUROSTAT_GBV_CH)) {
  if (any(grepl(",", EUROSTAT_GBV_CH[[col]]))) {
    EUROSTAT_GBV_CH <- separate(EUROSTAT_GBV_CH, col, 
                               into = paste0(col, "_", seq_len(max(stringr::str_count(EUROSTAT_GBV_CH[[col]], ",") + 1))),
                               sep = ",", fill = "right")
  }
}

EUROSTAT_GBV_CH %<>% 
  rename(AgeGroup =`freq,age,unit,geo\\TIME_PERIOD_2`,
         ISO2 = `freq,age,unit,geo\\TIME_PERIOD_4`,
         Value = `2021`) %>%
  mutate(StatisticalUnit = "Rate",
         Year = "2021",
         Indicator = "Women who have experienced sexual violence during childhood",
         Dataset = "EUROSTAT_GBV_CH",
         ICVACCategory = "Sexual violence against a child",
         Source = "EU-GBV 2021")

EUROSTAT_GBV_CH %<>%
  left_join(iso, by = "ISO2")

EUROSTAT_GBV_CH %<>%
  select(Country, ISO3, ISO2, Year, AgeGroup, Indicator, StatisticalUnit, Value, Source,
         ICVACCategory, Dataset)

EUROSTAT_GBV_CH %<>%
  mutate(ISO3 = case_when(
    ISO2 == "EL" ~ "GRC",
    ISO2 == "XK" ~ "XKX",
    TRUE ~ ISO3
  ))


EUROSTAT_GBV_CH %<>%
  mutate(Country = case_when(
    ISO3 == "GRC" ~ "Greece",
    ISO3 == "XKX" ~ "Kosovo",
    TRUE ~ Country
  ))

EUROSTAT_GBV_CH$AgeGroup <- gsub("^Y", "", EUROSTAT_GBV_CH$AgeGroup)

EUROSTAT_GBV_CH %<>%
  mutate(Reliability = ifelse(grepl("u", Value, ignore.case = TRUE), "Low", NA))

# Remove all characters that are not numbers or decimal point
EUROSTAT_GBV_CH$Value <- gsub("[^0-9.]", "", EUROSTAT_GBV_CH$Value)

# Convert the cleaned strings to numeric
EUROSTAT_GBV_CH$Value <- as.numeric(EUROSTAT_GBV_CH$Value)

EUROSTAT_GBV_CH %<>%
  select(-ISO2)

subset <- EUROSTAT_GBV_CH %>%
  filter(AgeGroup == "18-29")

# EUROSTAT Perpetrator
df <- folder3_estat_gbv_ch_perp

# Apply separate() to each column that has comma-separated values
for (col in names(df)) {
  if (any(grepl(",", df[[col]]))) {
    df <- separate(df, col, 
                                into = paste0(col, "_", seq_len(max(stringr::str_count(df[[col]], ",") + 1))),
                                sep = ",", fill = "right")
  }
}

names(df)


df %<>% 
  rename(RelationshipPerpetrator =`freq,perp,unit,geo\\TIME_PERIOD_2`,
         ISO2 = `freq,perp,unit,geo\\TIME_PERIOD_4`,
         Value = `2021`) %>%
  mutate(StatisticalUnit = "Rate",
         Year = "2021",
         Indicator = "Women who have experienced sexual violence during childhood",
         Dataset = "EUROSTAT_GBV_CH",
         ICVACCategory = "Sexual violence against a child",
         Source = "EU-GBV 2021")


df %<>%
  mutate(RelationshipPerpetrator = case_when(
    RelationshipPerpetrator == "FAM_REL" ~ "Family member or relative (F/M)",
    RelationshipPerpetrator == "STRNG" ~ "Stranger (F/M)",
    RelationshipPerpetrator == "PERP" ~ "Perpetrators (F/M)",
    RelationshipPerpetrator == "PERP_M" ~ "Perpetrators (M)",
    RelationshipPerpetrator == "KNW" ~ "Any known (F/M)",
    RelationshipPerpetrator == "KNW_OTH" ~ "Other known (non-family or relative - F/M)",
    TRUE ~ RelationshipPerpetrator
  ))


df %<>%
  left_join(iso, by = "ISO2")

df %<>%
  select(Country, ISO3, ISO2, Year, RelationshipPerpetrator, Indicator, StatisticalUnit, Value, Source,
         ICVACCategory, Dataset)

df %<>%
  mutate(ISO3 = case_when(
    ISO2 == "EL" ~ "GRC",
    ISO2 == "XK" ~ "XKX",
    TRUE ~ ISO3
  ))


df %<>%
  mutate(Country = case_when(
    ISO3 == "GRC" ~ "Greece",
    ISO3 == "XKX" ~ "Kosovo",
    TRUE ~ Country
  ))


df %<>%
  mutate(Reliability = ifelse(grepl("u", Value, ignore.case = TRUE), "Low", NA))

# Remove all characters that are not numbers or decimal point
df$Value <- gsub("[^0-9.]", "", df$Value)

# Convert the cleaned strings to numeric
df$Value <- as.numeric(df$Value)

df %<>%
  select(-ISO2)

# Map ----
# dynamic_map(subset)

# Processing DHS data
DHS_SEXUAL_VIOLENCE <- folder3_STATcompilerExport2024917_15321x

iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% 
  rename(Country = CountryName) %>%
  select(Country, ISO3)

DHS_SEXUAL_VIOLENCE <- DHS_SEXUAL_VIOLENCE[1:7,]

DHS_SEXUAL_VIOLENCE %<>%
  left_join(iso, by = "Country")

DHS_SEXUAL_VIOLENCE %<>%
  mutate(ISO3 = case_when(
    Country == "Moldova" ~ "MDA",
    Country == "Kyrgyz Republic" ~ "KGZ",
    TRUE ~ ISO3
  ))

# Select correct columns

# DHS_SEXUAL_VIOLENCE %<>%
#   select(Country, ISO3, Survey,`Total 15-49...3`, `Total 15-49...18`,
#          `15-19...4`, `15-19...11`)

DHS_SEXUAL_VIOLENCE <- DHS_SEXUAL_VIOLENCE %>%
  rename(TOTALever_15_49 = `Total 15-49...3`,
        ever_15_19 = `15-19...4`,
        ever_20_24 = `20-24...5`,
        ever_25_29 = `25-29...6`,
        ever_30_34 = `30-34...7`,
        ever_35_39 = `35-39...8`,
        ever_40_44 = `40-44...9`,
        ever_45_49 = `45-49...10`,
        months12_15_19 = `15-19...11`,
        months12_20_24 = `20-24...12`,
        months12_25_29 = `25-29...13`,
        months12_30_34 = `30-34...14`,
        months12_35_39 = `35-39...15`,
        months12_40_44 = `40-44...16`,
        months12_45_49 = `45-49...17`,
        TOTALmonths12_15_49 = `Total 15-49...18`)

DHS_SEXUAL_VIOLENCE %<>%
  pivot_longer(
    cols = c(TOTALever_15_49, ever_15_19, ever_15_19, ever_20_24, ever_25_29,
             ever_30_34, ever_35_39, ever_40_44, ever_45_49,
             TOTALmonths12_15_49, months12_15_19, months12_20_24,
             months12_20_24, months12_25_29, months12_30_34, months12_35_39,
             months12_40_44, months12_45_49),
    names_to = "Survey_Metric",   # This will be the new column for metric names
    values_to = "Value"           # This will be the new column for values
  )

DHS_SEXUAL_VIOLENCE %<>%
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

DHS_SEXUAL_VIOLENCE %<>%
  mutate(Sex = "Female")

DHS_SEXUAL_VIOLENCE %<>%
  mutate(Indicator = case_when(
    grepl("ever", Survey_Metric, ignore.case = TRUE) ~ "Women who ever experienced sexual violence",
    grepl("months", Survey_Metric, ignore.case = TRUE) ~ "Women who experienced sexual violence in past 12 months",
    TRUE ~ NA,
  ))

DHS_SEXUAL_VIOLENCE %<>%
  mutate(Year = case_when(
    grepl("2005", Survey, ignore.case = TRUE) ~ 2005,
    grepl("2006", Survey, ignore.case = TRUE) ~ 2006,
    grepl("2007", Survey, ignore.case = TRUE) ~ 2007,
    grepl("2012", Survey, ignore.case = TRUE) ~ 2012,
    grepl("16", Survey, ignore.case = TRUE) ~ 2016,
    grepl("2017", Survey, ignore.case = TRUE) ~ 2017,
    TRUE ~ NA,
  ))

DHS_SEXUAL_VIOLENCE %<>%
  rename(Source = Survey) %>%
  select(Country, ISO3, Year, Sex, AgeGroup, Indicator, Value, Source)

DHS_SEXUAL_VIOLENCE %<>%
  group_by(Country) %>%
  top_n(1, Year) %>%
  ungroup()

DHS_SEXUAL_VIOLENCE %<>%
  mutate(StatisticalUnit = "Rate")

DHS_SEXUAL_VIOLENCE %<>%
  select(Country, ISO3, Year, Sex, AgeGroup, Indicator, StatisticalUnit, Value, Source) %>%
  mutate(ICVACCategory = "Sexual violence against a child",
         Dataset = "DHS_SEXUAL_VIOLENCE")

DHS_SEXUAL_VIOLENCE_quint <- folder3_STATcompilerExport2024111_163737x
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% 
  rename(Country = CountryName) %>%
  select(Country, ISO3)

DHS_SEXUAL_VIOLENCE_quint <- DHS_SEXUAL_VIOLENCE_quint[1:7,]

DHS_SEXUAL_VIOLENCE_quint %<>%
  left_join(iso, by = "Country")

DHS_SEXUAL_VIOLENCE_quint %<>%
  mutate(ISO3 = case_when(
    Country == "Moldova" ~ "MDA",
    Country == "Kyrgyz Republic" ~ "KGZ",
    TRUE ~ ISO3
  ))


DHS_SEXUAL_VIOLENCE_quint %<>%
  rename(
    Lowest = Lowest...28,
    Second = Second...29,
    Middle = Middle...30,
    Fourth = Fourth...31,
    Highest = Highest...32,
    ISO3 = ISO3)

DHS_SEXUAL_VIOLENCE_quint %<>%
  select(Country, Survey, ISO3, Lowest, Second, Middle, Fourth, Highest)


DHS_SEXUAL_VIOLENCE_quint %<>%
  pivot_longer(
    cols = -c(Country, Survey, ISO3),  # Include all columns except Country, Survey, and ISO3
    names_to = "Survey_Metric",         # This will be the new column for metric names
    values_to = "Value"                 # This will be the new column for values
  )

DHS_SEXUAL_VIOLENCE_quint %<>%
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


DHS_SEXUAL_VIOLENCE_quint %<>%
  mutate(Sex = "Female") %>%
  rename(WealthQuintile = Survey_Metric)

DHS_SEXUAL_VIOLENCE_quint %<>%
  mutate(Indicator = "Women who ever experienced sexual violence")

DHS_SEXUAL_VIOLENCE_quint %<>%
  mutate(Year = case_when(
    grepl("2005", Survey, ignore.case = TRUE) ~ 2005,
    grepl("2006", Survey, ignore.case = TRUE) ~ 2006,
    grepl("2007", Survey, ignore.case = TRUE) ~ 2007,
    grepl("2012", Survey, ignore.case = TRUE) ~ 2012,
    grepl("16", Survey, ignore.case = TRUE) ~ 2016,
    grepl("2017", Survey, ignore.case = TRUE) ~ 2017,
    TRUE ~ NA,
  ))

DHS_SEXUAL_VIOLENCE_quint %<>%
  rename(Source = Survey) %>%
  select(Country, ISO3, Year, Sex, AgeGroup, WealthQuintile, Indicator, Value, Source)

DHS_SEXUAL_VIOLENCE_quint %<>%
  group_by(Country) %>%
  top_n(1, Year) %>%
  ungroup()

DHS_SEXUAL_VIOLENCE_quint %<>%
  mutate(StatisticalUnit = "Rate")

DHS_SEXUAL_VIOLENCE_quint %<>%
  select(Country, ISO3, Year, Sex, AgeGroup, WealthQuintile, Indicator, StatisticalUnit, Value, Source) %>%
  mutate(ICVACCategory = "Sexual violence against a child",
         Dataset = "DHS_SEXUAL_VIOLENCE")

# merge DHS
DHS_SEXUAL_VIOLENCE %<>%
  mutate(WealthQuintile = NA)

DHS_SEXUAL_VIOLENCE <- rbind(DHS_SEXUAL_VIOLENCE, DHS_SEXUAL_VIOLENCE_quint)

# subset <- DHS_SEXUAL_VIOLENCE %>%
#   filter(Indicator == "Women who ever experienced sexual violence" & AgeGroup == "15-19")
# dynamic_map(subset)

# Merge the two datasets ----
DHS_SEXUAL_VIOLENCE %<>%
  mutate(Reliability = "")

EUROSTAT_GBV_CH %<>%
  mutate(Sex = "Female",
         WealthQuintile = NA)

icvac3 <- rbind(EUROSTAT_GBV_CH, DHS_SEXUAL_VIOLENCE)

icvac3 %<>% 
  mutate(RelationshipPerpetrator = NA)

df %<>%
  mutate(AgeGroup = NA,
         WealthQuintile = NA,
         Sex = "Female")

icvac3 <- rbind(icvac3, df)

write.csv(icvac3, "data/output/disaggregated/icvac_3.csv", row.names = FALSE)
