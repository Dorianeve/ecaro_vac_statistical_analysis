# Do not clean environment as we need to load for cleaning
source("requirements/libraries.R")
# wb <- createWorkbook()
# file_path <- "data/output/2024_ICVAC_ECARO.xlsx"

# ATTITUDES TOWARDS WIFE BEATING ----
# MICS 4 ----
df <- folder11_MICS4...ECA.Equity.for.Children.Dashboard...CVx

table(df$Indicator)

# Isolate psychological indicators ----
df %<>%
  filter(grepl("domestic violence", Indicator, ignore.case = TRUE))

# Filter out disaggregations ----
df %<>%
  filter(!grepl("sub-national", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("ethnicity", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("language", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("functional", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("by Type of settlement", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("wealth by", AggregationArea, ignore.case = TRUE))

# Work on structure ----
df %>%
  filter(is.na(Year))


df %<>%
  mutate(AgeGroup = case_when(
    AggregationArea == "Age" ~ Category,
    AggregationArea == "Total" ~ "15-49",
    TRUE ~ NA
  ))

df %<>%
  mutate(AgeGroup = case_when(
    grepl("^15-19\\b", AgeGroup, ignore.case = TRUE) ~ "15-19",
    grepl("^20-24\\b", AgeGroup, ignore.case = TRUE) ~ "20-24",
    grepl("^25-29\\b", AgeGroup, ignore.case = TRUE) ~ "25-29",
    grepl("^30-34\\b", AgeGroup, ignore.case = TRUE) ~ "30-34",
    grepl("^35-39\\b", AgeGroup, ignore.case = TRUE) ~ "35-39",
    grepl("^40-44\\b", AgeGroup, ignore.case = TRUE) ~ "40-44",
    grepl("^45-49\\b", AgeGroup, ignore.case = TRUE) ~ "45-49",
    TRUE ~ AgeGroup)
  )

df %<>%
  mutate(WealthQuintile = case_when(
    AggregationArea == "Wealth Index Quintile" ~ Category,
    TRUE ~ NA
  ))


df %<>%
  mutate(WealthQuintile = case_when(
    WealthQuintile == "Poorest (Q1)" ~ "Lowest",
    WealthQuintile == "Richest (Q5)" ~ "Highest",
    TRUE ~ NA
  ))



df %<>%
  mutate(IDPstatus = case_when(
    AggregationArea == "IDP status" ~ Category,
    TRUE ~ NA
  ))

df %<>%
  mutate(SettlementType = case_when(
    AggregationArea == "Type of settlement" ~ Category,
    TRUE ~ NA
  ))

df %<>%
  mutate(Sex = case_when(
    grepl("women", Indicator, ignore.case = TRUE) ~ "Female",
    grepl("men", Indicator, ignore.case = TRUE) ~ "Male",
    TRUE ~ NA
  ))

df %<>%
  filter(AggregationArea != "Gender")

# ISO merging ----
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% select(CountryName, ISO3) %>% rename(Country = CountryName)

df %<>%
  left_join(iso, by = "Country")

df %<>%
  mutate(ISO3 = case_when(
    Country == "BiH" ~ "BIH",
    TRUE ~ ISO3
  ))

df %<>%
  mutate(Country = case_when(
    Country == "BiH" ~ "Bosnia and Herzegovina",
    TRUE ~ Country
  ))

# Unify structure ----
df %<>%
  mutate(Source = paste0(Survey, " ", Year))

df %<>%
  mutate(Source = gsub("Roma settlements", "MICS Roma settlements", Source))

# Usign latest year ----
df %<>%
  mutate(Year = gsub("2011-12", "2012", Year))

df %<>%
  mutate(Value = as.numeric(Value),
         StatisticalUnit = "Rate",
         Dataset = "MICS-ATTITUDE-DOMESTIC",
         ICVACCategory = "Environment") %>%
  rename(Comments = Footnote,
         Reliability = `CV classification` )

df %<>%
  select(Country, ISO3, Indicator, Year,
         Value, Sex, AgeGroup, StatisticalUnit, Source,
         Dataset, ICVACCategory, WealthQuintile, IDPstatus, SettlementType,
         Comments, Reliability)

mics4 <- df

# MICS 5 ----
df <- folder11_MICS5...ECA.Equity.for.Children.Dashboard...CVx

table(df$Country, df$Year)

# Isolate psychological indicators ----
df %<>%
  filter(grepl("domestic violence", Indicator, ignore.case = TRUE))

# Filter out disaggregations ----
df %<>%
  filter(!grepl("sub-national", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("ethnicity", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("language", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("functional", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("by Type of settlement", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("wealth by", AggregationArea, ignore.case = TRUE))

# Work on structure ----
# Work on structure ----
df %>%
  filter(is.na(Year)) %>%
  distinct(Country)

df %<>%
  mutate(AgeGroup = case_when(
    AggregationArea == "Age" ~ Category,
    AggregationArea == "Total" ~ "15-49",
    TRUE ~ NA
  ))

df %<>%
  mutate(AgeGroup = case_when(
    grepl("^15-19\\b", AgeGroup, ignore.case = TRUE) ~ "15-19",
    grepl("^20-24\\b", AgeGroup, ignore.case = TRUE) ~ "20-24",
    grepl("^25-29\\b", AgeGroup, ignore.case = TRUE) ~ "25-29",
    grepl("^30-34\\b", AgeGroup, ignore.case = TRUE) ~ "30-34",
    grepl("^35-39\\b", AgeGroup, ignore.case = TRUE) ~ "35-39",
    grepl("^40-44\\b", AgeGroup, ignore.case = TRUE) ~ "40-44",
    grepl("^45-49\\b", AgeGroup, ignore.case = TRUE) ~ "45-49",
    TRUE ~ AgeGroup)
  )

df %<>%
  mutate(WealthQuintile = case_when(
    AggregationArea == "Wealth Index Quintile" ~ Category,
    TRUE ~ NA
  ))


df %<>%
  mutate(WealthQuintile = case_when(
    WealthQuintile == "Poorest (Q1)" ~ "Lowest",
    WealthQuintile == "Richest (Q5)" ~ "Highest",
    TRUE ~ NA
  ))

df %<>%
  mutate(WealthQuintile = case_when(
    AggregationArea == "Wealth Index Quintile" ~ Category,
    TRUE ~ NA
  ))


df %<>%
  mutate(IDPstatus = case_when(
    AggregationArea == "IDP status" ~ Category,
    TRUE ~ NA
  ))

df %<>%
  mutate(SettlementType = case_when(
    AggregationArea == "Type of settlement" ~ Category,
    TRUE ~ NA
  ))

df %<>%
  mutate(Sex = case_when(
    grepl("women", Indicator, ignore.case = TRUE) ~ "Female",
    grepl("men", Indicator, ignore.case = TRUE) ~ "Male",
    TRUE ~ NA
  ))

df %<>%
  filter(AggregationArea != "Gender")

# ISO merging ----
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% select(CountryName, ISO3) %>% rename(Country = CountryName)

df %<>%
  left_join(iso, by = "Country")

# Unify structure ----

df %<>%
  mutate(Year = case_when(
    Country == "Kosovo" ~ 2014,
    Country == "Turkmenistan" ~ 2016,
    TRUE ~ Year
  ))

df %>%
  filter(is.na(Year)) %>%
  distinct(Country)


df %<>%
  mutate(Source = paste0(Survey, " ", Year))

df %<>%
  mutate(Source = gsub("Roma settlements", "MICS Roma settlements", Source))


df %<>%
  mutate(Value = as.numeric(Value),
         StatisticalUnit = "Rate",
         Dataset = "MICS-ATTITUDE-DOMESTIC",
         ICVACCategory = "Environment") %>%
  rename(Comments = Footnote,
         Reliability = `CV classification` )

df %<>%
  select(Country, ISO3, Indicator, Year,
         Value, Sex, AgeGroup, StatisticalUnit, Source,
         Dataset, ICVACCategory, WealthQuintile, IDPstatus, SettlementType,
         Comments, Reliability)

mics5 <- df


# MICS6 ----

df <- folder11_MICS6...ECA.Equity.for.Children.Dashboard...CVx

table(df$Country, df$Year)

# Isolate domestic violence indicators ----

df %<>%
  filter(grepl("domestic violence", Indicator, ignore.case = TRUE))

# Filter out disaggregations ----
df %<>%
  filter(!grepl("sub-national", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("ethnicity", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("language", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("functional", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("by Type of settlement", AggregationArea, ignore.case = TRUE))
df %<>%
  filter(!grepl("wealth by", AggregationArea, ignore.case = TRUE))

# Work on structure ----
df %>%
  filter(is.na(Year)) %>%
  distinct(Country)


df %<>%
  mutate(AgeGroup = case_when(
    AggregationArea == "Age" ~ Category,
    AggregationArea == "Total" ~ "15-49",
    TRUE ~ NA
  ))

df %<>%
  mutate(AgeGroup = case_when(
    grepl("^15-19\\b", AgeGroup, ignore.case = TRUE) ~ "15-19",
    grepl("^20-24\\b", AgeGroup, ignore.case = TRUE) ~ "20-24",
    grepl("^25-29\\b", AgeGroup, ignore.case = TRUE) ~ "25-29",
    grepl("^30-34\\b", AgeGroup, ignore.case = TRUE) ~ "30-34",
    grepl("^35-39\\b", AgeGroup, ignore.case = TRUE) ~ "35-39",
    grepl("^40-44\\b", AgeGroup, ignore.case = TRUE) ~ "40-44",
    grepl("^45-49\\b", AgeGroup, ignore.case = TRUE) ~ "45-49",
    TRUE ~ AgeGroup)
  )

df %<>%
  mutate(WealthQuintile = case_when(
    AggregationArea == "Wealth Index Quintile" ~ Category,
    TRUE ~ NA
  ))


df %<>%
  mutate(WealthQuintile = case_when(
    WealthQuintile == "Poorest (Q1)" ~ "Lowest",
    WealthQuintile == "Richest (Q5)" ~ "Highest",
    TRUE ~ NA
  ))

df %<>%
  mutate(WealthQuintile = case_when(
    AggregationArea == "Wealth Index Quintile" ~ Category,
    TRUE ~ NA
  ))


df %<>%
  mutate(IDPstatus = case_when(
    AggregationArea == "IDP status" ~ Category,
    TRUE ~ NA
  ))

df %<>%
  mutate(SettlementType = case_when(
    AggregationArea == "Type of settlement" ~ Category,
    TRUE ~ NA
  ))

df %<>%
  mutate(Sex = case_when(
    grepl("women", Indicator, ignore.case = TRUE) ~ "Female",
    grepl("men", Indicator, ignore.case = TRUE) ~ "Male",
    TRUE ~ NA
  ))

df %<>%
  filter(AggregationArea != "Gender")
# ISO merging ----
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% select(CountryName, ISO3) %>% rename(Country = CountryName)

df %<>%
  left_join(iso, by = "Country")

df %<>%
  mutate(ISO3 = case_when(
    Country == "Macedonia" ~ "MKD",
    TRUE ~ ISO3
  ))

df %<>%
  mutate(Country = case_when(
    Country == "Macedonia" ~ "North Macedonia",
    TRUE ~ Country
  ))

# Unify structure ----
df %<>%
  mutate(Year = case_when(
    Country == "North Macedonia" ~ 2019,
    Country == "Uzbekistan" ~ 2022,
    Country == "Kosovo" ~ 2019,
    TRUE ~ Year
  ))


df %>%
  filter(is.na(Year)) %>%
  distinct(Country)


df %<>%
  mutate(Source = paste0(Survey, " ", Year))

df %<>%
  mutate(Source = gsub("Roma settlements", "MICS Roma settlements", Source))

df %<>%
  mutate(Value = as.numeric(Value),
         StatisticalUnit = "Rate",
         Dataset = "MICS-ATTITUDE-DOMESTIC",
         ICVACCategory = "Environment") %>%
  rename(Comments = Footnote,
         Reliability = `CV classification` )

df %<>%
  select(Country, ISO3, Indicator, Year,
         Value, Sex, AgeGroup, StatisticalUnit, Source,
         Dataset, ICVACCategory, WealthQuintile, IDPstatus, SettlementType,
         Comments, Reliability)

mics6 <- df

df <- rbind(mics4, mics5, mics6)

df %<>%
  mutate(Indicator = "Attitude towards domestic violence",
         Value = ifelse(Value == 998, NA, Value))

df %>%
  filter(is.na(Year)) %>%
  distinct(Country)


write.csv(df, "data/output/disaggregated/icvac_11_mics.csv", row.names = FALSE)
