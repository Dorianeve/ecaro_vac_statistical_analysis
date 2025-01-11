# Do not clean environment as we need to load for cleaning
source("requirements/libraries.R")
# wb <- createWorkbook()
# file_path <- "data/output/2024_ICVAC_ECARO.xlsx"

# DHS Child discipline ----
DHS_CHLD_DISCIPLINE <- folder10_STATcompilerExport2024917_161040x
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% select(CountryName, ISO3) %>% rename(Country = CountryName)

DHS_CHLD_DISCIPLINE <- DHS_CHLD_DISCIPLINE[1:108,]

DHS_CHLD_DISCIPLINE %<>%
  rename(Country = `Country Name`) %>%
  left_join(iso, by = "Country")

DHS_CHLD_DISCIPLINE %<>%
  select(-`Country Code`) %>%
  filter(Indicator == "Children 2-14 who experienced any violent discipline method")

DHS_CHLD_DISCIPLINE %<>%
  mutate(AgeGroup = case_when(
    `Characteristic Label` == "2-4" ~ "2-4",
    `Characteristic Label` == "5-9" ~ "5-9",
    `Characteristic Label` == "10-14" ~ "10-14",
    TRUE ~ NA
  ))

DHS_CHLD_DISCIPLINE %<>%
  mutate(Sex = case_when(
    `Characteristic Label` == "Female" ~ "Female",
    `Characteristic Label` == "Male" ~ "Male",
    `Characteristic Label` == "Total" ~ "Total",
    TRUE ~ NA
  ))

DHS_CHLD_DISCIPLINE %<>%
  rename(Source = `Survey Name`,
         Year = `Survey Year`) %>%
  mutate(StatisticalUnit = "Rate",
         Dataset = "DHA-CHLD-DISCIPLINE",
         ICVACCategory = "Composite phoenomena",
         Comments = NA,
         Reliability = NA,
         Sex = ifelse(is.na(Sex), "Total", Sex),
         AgeGroup = ifelse(is.na(AgeGroup), "2-14", AgeGroup)) %>%
  select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit,
         Source, Dataset, ICVACCategory, Comments, Reliability)

# UNICEF Child discipline ----
UNICEF_CHLD_DISCIPLINE <- folder10_XLS_Violent.discipline.database_Jun.2024x

UNICEF_CHLD_DISCIPLINE %<>%
  mutate(Country = ...1,
         Total = `GLOBAL DATABASES`,
         FlagTotal = ...3,
         Male = ...4,
         FlagMale = ...5,
         Female = ...6,
         FlagFemale = ...7,
         Source = ...8)

UNICEF_CHLD_DISCIPLINE <- UNICEF_CHLD_DISCIPLINE[10:211,]

UNICEF_CHLD_DISCIPLINE %<>%
  select(Country, Total, FlagTotal,
         Female, FlagFemale,
         Male, FlagMale,
         Source)

UNICEF_CHLD_DISCIPLINE %<>%
  mutate(across(-Source, ~ gsub("-", NA, .)))

UNICEF_CHLD_DISCIPLINE %<>%
  left_join(iso, by = "Country")

UNICEF_CHLD_DISCIPLINE %<>%
  mutate(ISO3 = case_when(
    Country == "United Kingdom" ~ "GBR",
    TRUE ~ ISO3
  ))

UNICEF_CHLD_DISCIPLINE %<>%
  filter(!is.na(ISO3))

UNICEF_CHLD_DISCIPLINE %<>%
  mutate(Comments = case_when(
    FlagTotal == "x" | FlagFemale == "x" | FlagMale == "x" ~ 
      "Data refer to years or periods other than those specified in the column heading. Such data are not included in the calculation of regional and global averages.",
    FlagTotal == "y" | FlagFemale == "y" | FlagMale == "y" ~ 
      "Data differ from the standard definition or refer to only part of a country. If they fall within the noted reference period, such data are included in the calculation of regional and global averages.",
    FlagTotal == "x,y" | FlagFemale == "x,y" | FlagMale == "x,y" ~ 
      "Data refer to years or periods other than those specified in the column heading. Such data are not included in the calculation of regional and global averages. | Data differ from the standard definition or refer to only part of a country. If they fall within the noted reference period, such data are included in the calculation of regional and global averages.",
    TRUE ~ NA
  ))

UNICEF_CHLD_DISCIPLINE %<>%
  select(-c(FlagTotal, FlagFemale, FlagMale))

UNICEF_CHLD_DISCIPLINE %<>%
  pivot_longer(cols = c(Total, Female, Male),
               names_to = "Sex")

UNICEF_CHLD_DISCIPLINE %<>%
  mutate(AgeGroup = "1-14",
         Indicator = "Percentage of children 1–14 years old who experience any violent discipline (psychological aggression and/or physical punishment) in the past month.",
         Dataset = "UNICEF-CHLD-DISCIPLINE",
         Year = 2024,
         ICVACCategory = "Composite phoenomena",
         Reliability = NA,
         Value = as.numeric(value),
         StatisticalUnit = "Rate") %>%
  select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit,
         Source, Dataset, ICVACCategory, Comments, Reliability)

# TRANSMONEE Child labour ----
TMEE_CHLD_LABOUR_5_17 <- folder10_fusion_TRANSMONEE_ECARO_1.0_.PT_CHLD_5.17_LBR_ECON....

TMEE_CHLD_LABOUR_5_17 %<>%
  mutate(
    ISO3 = substr(`REF_AREA:Geographic area`, 1, 3),  # Extract first 3 characters
    Country = sub(".*: ", "", `REF_AREA:Geographic area`)  # Extract everything after ": "
  )

TMEE_CHLD_LABOUR_5_17 %<>%
  mutate(Indicator = "Percentage of children (5-17 years) engaged in child labour (economic activities)",
         Sex = substr(`SEX:Sex`, 1, 3),
         AgeGroup = "5-17")

TMEE_CHLD_LABOUR_5_17 %<>%
  mutate(Sex = case_when(
    grepl("T", Sex, ignore.case = TRUE)  ~  "Total",
    grepl("F", Sex, ignore.case = TRUE)  ~  "Female",
    grepl("M", Sex, ignore.case = TRUE)  ~  "Male",
    TRUE ~ Sex
  ))

TMEE_CHLD_LABOUR_5_17 %<>%
  mutate(Dataset = "TMEE_CHLD_LABOUR_5_17",
         Year = `TIME_PERIOD:Time period`,
         ICVACCategory = "Composite phoenomena",
         Reliability = NA,
         Value = as.numeric(`OBS_VALUE:Observation value`),
         StatisticalUnit = "Rate",
         Comments = `OBS_FOOTNOTE:Observation footnote`,
         Source = `DATA_SOURCE:Data Source`) %>%
  select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit,
         Source, Dataset, ICVACCategory, Comments, Reliability)


# UNODC Human Trafficking ----
UNODC_SDG_16.2.2 <- folder10_sdg_datasetx

UNODC_SDG_16.2.2 %<>%
  filter(Indicator_label_short == "16.2.2 'Human trafficking'" &
           grepl("children", Disaggregation, ignore.case = TRUE))

UNODC_SDG_16.2.2 %<>%
  mutate(ISO3 = Iso3_code,
         Indicator = "Human trafficking",
         StatisticalUnit = Unit_of_measurement,
         AgeGroup = NA,
         Dataset = "UNODC_SDG_16.2.2",
         ICVACCategory = "Composite phoenomena",
         Comments = ifelse(Value == "<5", NA, Value),
         Value = as.numeric(Value),
         Reliability = NA) %>%
  select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit,
         Source, Dataset, ICVACCategory, Comments, Reliability, Disaggregation)

UNODC_SDG_16.2.2 %<>%
  mutate(Indicator = paste(Indicator, " - ", Disaggregation)) %>%
  select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit,
         Source, Dataset, ICVACCategory, Comments, Reliability)

UNODC_SDG_16.2.2 %<>%
  filter(ISO3 %in% iso$ISO3)

# TRANSMONEE HBCS bullying ----
TMEE_HBSC_BULLYING <- folder10_fusion_TRANSMONEE_ECARO_1.0_.PT_ST_13.15_BUL_30.DYS....

TMEE_HBSC_BULLYING %<>%
  mutate(
    ISO3 = substr(`REF_AREA:Geographic area`, 1, 3),  # Extract first 3 characters
    Country = sub(".*: ", "", `REF_AREA:Geographic area`)  # Extract everything after ": "
  )

TMEE_HBSC_BULLYING %<>%
  mutate(Indicator = "Percentage of students (13-15 years) who reported being bullied on 1 or more days in the past 30 days",
         Sex = substr(`SEX:Sex`, 1, 3),
         AgeGroup = "13-15")

TMEE_HBSC_BULLYING %<>%
  mutate(Sex = case_when(
    grepl("T", Sex, ignore.case = TRUE)  ~  "Total",
    grepl("F", Sex, ignore.case = TRUE)  ~  "Female",
    grepl("M", Sex, ignore.case = TRUE)  ~  "Male",
    TRUE ~ Sex
  ))

TMEE_HBSC_BULLYING %<>%
  mutate(Dataset = "TMEE_HBSC_BULLYING",
         Year = `TIME_PERIOD:Time period`,
         ICVACCategory = "Composite phoenomena",
         Reliability = NA,
         Value = as.numeric(`OBS_VALUE:Observation value`),
         StatisticalUnit = "Rate",
         Comments = `OBS_FOOTNOTE:Observation footnote`,
         Source = `DATA_SOURCE:Data Source`) %>%
  select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit,
         Source, Dataset, ICVACCategory, Comments, Reliability)


# TRANSMONEE HBCS Cyber-bullying ----
TMEE_HBCS_CYBER <- folder10_fusion_TRANSMONEE_ECARO_1.0_.PP_CHLD_VICTIM_CB....

TMEE_HBCS_CYBER %<>%
  mutate(
    ISO3 = substr(`REF_AREA:Geographic area`, 1, 3),  # Extract first 3 characters
    Country = sub(".*: ", "", `REF_AREA:Geographic area`),
    WealthQuintile = substring(TMEE_HBCS_CYBER$`WEALTH_QUINTILE:Wealth Quintile`, 5)# Extract everything after ": "
  )

TMEE_HBCS_CYBER %<>%
  mutate(Indicator = "Percentage of 11-, 13- and 15-year-old school children who report having been a victim of cyber-bullying",
         Sex = substr(`SEX:Sex`, 1, 3),
         AgeGroup = "11,13,15")

TMEE_HBCS_CYBER %<>%
  mutate(Sex = case_when(
    grepl("T", Sex, ignore.case = TRUE)  ~  "Total",
    grepl("F", Sex, ignore.case = TRUE)  ~  "Female",
    grepl("M", Sex, ignore.case = TRUE)  ~  "Male",
    TRUE ~ Sex
  ))

TMEE_HBCS_CYBER %<>%
  mutate(Dataset = "TMEE_HBCS_CYBER",
         Year = `TIME_PERIOD:Time period`,
         ICVACCategory = "Composite phoenomena",
         Reliability = NA,
         Value = as.numeric(`OBS_VALUE:Observation value`),
         StatisticalUnit = "Rate",
         Comments = `OBS_FOOTNOTE:Observation footnote`,
         Source = `DATA_SOURCE:Data Source`) %>%
  select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit,
         Source, WealthQuintile, Dataset, ICVACCategory, Comments, Reliability)



# UNICEF Intimate partner violence ----

UNICEF_INTIMATE_PARTNER <- folder10_XLS_IPV.database_July.2024x


UNICEF_INTIMATE_PARTNER %<>%
  mutate(Country = ...1,
         Total = `GLOBAL DATABASES`,
         FlagTotal = ...3,
         Source = ...4)

UNICEF_INTIMATE_PARTNER <- UNICEF_INTIMATE_PARTNER[10:211,]

UNICEF_INTIMATE_PARTNER %<>%
  select(Country, Total, FlagTotal,
         Source)

UNICEF_INTIMATE_PARTNER %<>%
  left_join(iso, by = "Country")

UNICEF_INTIMATE_PARTNER %<>%
  mutate(ISO3 = case_when(
    Country == "United Kingdom" ~ "GBR",
    TRUE ~ ISO3
  ))

UNICEF_INTIMATE_PARTNER %<>%
  filter(!is.na(ISO3))

UNICEF_INTIMATE_PARTNER %<>%
  mutate(Comments = case_when(
    FlagTotal == "x" ~ 
      "Data refer to years or periods other than those specified in the column heading. Such data are not included in the calculation of regional and global averages.",
    FlagTotal == "y" ~ 
      "Data differ from the standard definition or refer to only part of a country. If they fall within the noted reference period, such data are included in the calculation of regional and global averages.",
    FlagTotal == "x,y" ~ 
      "Data refer to years or periods other than those specified in the column heading. Such data are not included in the calculation of regional and global averages. | Data differ from the standard definition or refer to only part of a country. If they fall within the noted reference period, such data are included in the calculation of regional and global averages.",
    FlagTotal == "p" ~
      "Based on small denominators (typically 25-49 unweighted cases). No data based on fewer than 25 unweighted cases are displayed.",
    TRUE ~ NA
  ))

UNICEF_INTIMATE_PARTNER %<>%
  select(-c(FlagTotal))


UNICEF_INTIMATE_PARTNER %<>%
  mutate(AgeGroup = "15-19",
         Indicator = "Percentage of ever-partnered girls aged 15–19 years who have experienced physical and/or sexual violence by a current or former intimate partner during the last twelve months.",
         Dataset = "UNICEF_INTIMATE_PARTNER",
         Sex = "Female",
         Year = str_extract(Source, "\\d{4}"),
         ICVACCategory = "Composite phoenomena",
         Reliability = NA,
         Value = as.numeric(Total),
         StatisticalUnit = "Rate") %>%
  select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit,
         Source, Dataset, ICVACCategory, Comments, Reliability)

# OECD Child Marriage ----
OECD_CHLD_MARRIAGE <- folder10_OECD.DEV.NPG.DSD_GID.DF_GID_2023..all

OECD_CHLD_MARRIAGE %<>%
  filter(Measure == "Prevalence child marriage")

OECD_CHLD_MARRIAGE %<>%
  rename(ISO3 = REF_AREA)

OECD_CHLD_MARRIAGE %<>%
  left_join(iso, by = "ISO3")

OECD_CHLD_MARRIAGE %<>%
  filter(!is.na(Country))

OECD_CHLD_MARRIAGE %<>%
  rename(StatisticalUnit = `Unit of measure`,
         AgeGroup = AGE,
         Indicator = Measure) %>%
  mutate(Comments = paste0("SDG Indicator - ", `SDG indicator`)) %>%
  mutate(Comments = ifelse(Comments == "SDG Indicator - NA", NA, Comments))

OECD_CHLD_MARRIAGE %<>%
  rename(Year = TIME_PERIOD,
         Value = OBS_VALUE) %>%
  mutate(Source = paste0("OECD - ", STRUCTURE_NAME),
         Dataset = "OECD-CHLD-MARRIAGE",
         ICVACCategory = "Composite phoenomena",
         Reliability = NA)

OECD_CHLD_MARRIAGE %<>%  
select(Country, ISO3, Indicator, Year, Value, Sex, AgeGroup, StatisticalUnit,
         Source, Dataset, ICVACCategory, Comments, Reliability)

OECD_CHLD_MARRIAGE %<>%
  mutate(AgeGroup = case_when(
    AgeGroup == "Y15T19" ~ "15-19",
    AgeGroup == "Y20T24" ~ "20-24",
    TRUE ~ NA
  ))

# Prep and save ----
# DHS excluded as it is part of UNICEF
# Define a function to ensure the WealthQuintile column exists
add_wealth_quintile <- function(df) {
  if (!"WealthQuintile" %in% colnames(df)) {
    df <- df %>% mutate(WealthQuintile = NA) # Add column with NA values
  }
  return(df)
}

# Apply the function to each data frame
OECD_CHLD_MARRIAGE <- add_wealth_quintile(OECD_CHLD_MARRIAGE)
UNICEF_CHLD_DISCIPLINE <- add_wealth_quintile(UNICEF_CHLD_DISCIPLINE)
TMEE_CHLD_LABOUR_5_17 <- add_wealth_quintile(TMEE_CHLD_LABOUR_5_17)
TMEE_HBSC_BULLYING <- add_wealth_quintile(TMEE_HBSC_BULLYING)
UNODC_SDG_16.2.2 <- add_wealth_quintile(UNODC_SDG_16.2.2)
TMEE_HBCS_CYBER <- add_wealth_quintile(TMEE_HBCS_CYBER)
UNICEF_INTIMATE_PARTNER <- add_wealth_quintile(UNICEF_INTIMATE_PARTNER)

# Combine the data frames
icvac10 <- rbind(
  UNICEF_CHLD_DISCIPLINE, 
  TMEE_CHLD_LABOUR_5_17, 
  TMEE_HBSC_BULLYING, 
  UNODC_SDG_16.2.2, 
  TMEE_HBCS_CYBER,
  UNICEF_INTIMATE_PARTNER,
  OECD_CHLD_MARRIAGE
)

write.csv(icvac10, "data/output/disaggregated/icvac_10.csv", row.names = FALSE)
