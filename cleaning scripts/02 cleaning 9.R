# Do not clean environment as we need to load for cleaning
source("requirements/libraries.R")
# wb <- createWorkbook()
# file_path <- "data/output/2024_ICVAC_ECARO.xlsx"

# TM Welfare ----
TMEE_PT_CHLD_VIOLENCE_WELFARE <- folder9_fusion_TRANSMONEE_ECARO_1.0_.PT_CHLD_VIOLENCE_WELFARE....
iso <- read.csv("data/utilities/iso_codes.csv", encoding = "UTF-8")
iso %<>% rename(Country = CountryName)
iso %<>% dplyr::select(Country,ISO3, ISO2)

TMEE_PT_CHLD_VIOLENCE_WELFARE %<>%
  mutate(
    ISO3 = substr(`REF_AREA:Geographic area`, 1, 3),  # Extract first 3 characters
    Country = sub(".*: ", "", `REF_AREA:Geographic area`)  # Extract everything after ": "
  )

TMEE_PT_CHLD_VIOLENCE_WELFARE %<>%
  mutate(Indicator = "Number of child victims of violence (0-17 years) registered by child/social welfare authorities during the year",
         Sex = substr(`SEX:Sex`, 1, 3),
         AgeGroup = substr(`AGE:Age`, 1,3))

TMEE_PT_CHLD_VIOLENCE_WELFARE %<>%
  mutate(Sex = case_when(
    grepl("T", Sex, ignore.case = TRUE)  ~  "Total",
    grepl("F", Sex, ignore.case = TRUE)  ~  "Female",
    grepl("M", Sex, ignore.case = TRUE)  ~  "Male",
    TRUE ~ Sex
  ))

TMEE_PT_CHLD_VIOLENCE_WELFARE %<>%
  mutate(AgeGroup = case_when(
    grepl("T", AgeGroup, ignore.case = TRUE)  ~  "0-17",
    grepl("Y0:", AgeGroup, ignore.case = TRUE)  ~  "0",
    grepl("Y01", AgeGroup, ignore.case = TRUE)  ~  "1",
    grepl("Y02", AgeGroup, ignore.case = TRUE)  ~  "2",
    grepl("Y03", AgeGroup, ignore.case = TRUE)  ~  "3",
    grepl("Y04", AgeGroup, ignore.case = TRUE)  ~  "4",
    grepl("Y05", AgeGroup, ignore.case = TRUE)  ~  "5",
    grepl("Y06", AgeGroup, ignore.case = TRUE)  ~  "6",
    grepl("Y07", AgeGroup, ignore.case = TRUE)  ~  "7",
    grepl("Y08", AgeGroup, ignore.case = TRUE)  ~  "8",
    grepl("Y09", AgeGroup, ignore.case = TRUE)  ~  "9",
    grepl("Y10", AgeGroup, ignore.case = TRUE)  ~  "10",
    grepl("Y11", AgeGroup, ignore.case = TRUE)  ~  "11",
    grepl("Y12", AgeGroup, ignore.case = TRUE)  ~  "12",
    grepl("Y13", AgeGroup, ignore.case = TRUE)  ~  "13",
    grepl("Y14", AgeGroup, ignore.case = TRUE)  ~  "14",
    grepl("Y15", AgeGroup, ignore.case = TRUE)  ~  "15",
    grepl("Y16", AgeGroup, ignore.case = TRUE)  ~  "16",
    grepl("Y17", AgeGroup, ignore.case = TRUE)  ~  "17",
    TRUE ~ AgeGroup
  ))

TMEE_PT_CHLD_VIOLENCE_WELFARE %<>%
  mutate(Dataset = "TMEE_PT_CHLD_VIOLENCE_WELFARE",
         StatisticalUnit = "Number",
         Value = `OBS_VALUE:Observation value`,
         Source = `DATA_SOURCE:Data Source`,
         Comments = `OBS_FOOTNOTE:Observation footnote`,
         ICVACCategory = "Other acts of violence against a child not elsewhere classified",
         Year = `TIME_PERIOD:Time period`) %>%
  dplyr::select(Country, ISO3, Year, Sex, AgeGroup, Indicator, StatisticalUnit,
         Value, Source,
         ICVACCategory, Dataset, Comments)

TMEE_PT_CHLD_VIOLENCE_WELFARE %>%
  filter(AgeGroup == "0-17" & Sex == "Total") %>%
  group_by(Country, Year) %>%
  summarise(Total = sum(Value)) %>%
  pivot_wider(names_from = Year, values_from = Total)

# Analysis prep
icvac9_aggregated_welfare <- TMEE_PT_CHLD_VIOLENCE_WELFARE %>%
  filter(AgeGroup == "0-17" & Sex == "Total") %>%
  group_by(Country, ISO3,  Year) %>%
  summarize(Value = sum(Value))

icvac9_disagg_welfare <- TMEE_PT_CHLD_VIOLENCE_WELFARE %>%
  filter(AgeGroup == "0-17" & Sex != "Total") %>%
  group_by(Country, ISO3,  Year, Sex) %>%
  summarize(Value = sum(Value))

icvac9_trends_welfare <- icvac9_aggregated_welfare %>%
  group_by(Country, ISO3) %>% 
  filter(n() > 5)

icvac9_comparison_welfare <- icvac9_aggregated_welfare %>%
  group_by(Country) %>%
  top_n(1, Year) %>%
  ungroup()




# TM CWD welfare ----

TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE <- folder9_fusion_TRANSMONEE_ECARO_1.0_.PT_CHLD_DISAB_VIOLENCE_WELFARE....


iso %<>% dplyr::select(Country,ISO3, ISO2)

TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE %<>%
  mutate(
    ISO3 = substr(`REF_AREA:Geographic area`, 1, 3),  # Extract first 3 characters
    Country = sub(".*: ", "", `REF_AREA:Geographic area`)  # Extract everything after ": "
  )

TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE %<>%
  mutate(Indicator = "Number of child victims of violence with disabilities (0-17 years) registered by child/social welfare authorities during the year",
         Sex = substr(`SEX:Sex`, 1, 3),
         AgeGroup = substr(`AGE:Age`, 1,3))

TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE %<>%
  mutate(Sex = case_when(
    grepl("T", Sex, ignore.case = TRUE)  ~  "Total",
    grepl("F", Sex, ignore.case = TRUE)  ~  "Female",
    grepl("M", Sex, ignore.case = TRUE)  ~  "Male",
    TRUE ~ Sex
  ))

TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE %<>%
  mutate(AgeGroup = case_when(
    grepl("T", AgeGroup, ignore.case = TRUE)  ~  "0-17",
    grepl("Y0:", AgeGroup, ignore.case = TRUE)  ~  "0",
    grepl("Y01", AgeGroup, ignore.case = TRUE)  ~  "1",
    grepl("Y02", AgeGroup, ignore.case = TRUE)  ~  "2",
    grepl("Y03", AgeGroup, ignore.case = TRUE)  ~  "3",
    grepl("Y04", AgeGroup, ignore.case = TRUE)  ~  "4",
    grepl("Y05", AgeGroup, ignore.case = TRUE)  ~  "5",
    grepl("Y06", AgeGroup, ignore.case = TRUE)  ~  "6",
    grepl("Y07", AgeGroup, ignore.case = TRUE)  ~  "7",
    grepl("Y08", AgeGroup, ignore.case = TRUE)  ~  "8",
    grepl("Y09", AgeGroup, ignore.case = TRUE)  ~  "9",
    grepl("Y10", AgeGroup, ignore.case = TRUE)  ~  "10",
    grepl("Y11", AgeGroup, ignore.case = TRUE)  ~  "11",
    grepl("Y12", AgeGroup, ignore.case = TRUE)  ~  "12",
    grepl("Y13", AgeGroup, ignore.case = TRUE)  ~  "13",
    grepl("Y14", AgeGroup, ignore.case = TRUE)  ~  "14",
    grepl("Y15", AgeGroup, ignore.case = TRUE)  ~  "15",
    grepl("Y16", AgeGroup, ignore.case = TRUE)  ~  "16",
    grepl("Y17", AgeGroup, ignore.case = TRUE)  ~  "17",
    TRUE ~ AgeGroup
  ))

TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE %<>%
  mutate(Dataset = "TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE",
         StatisticalUnit = "Number",
         Value = `OBS_VALUE:Observation value`,
         Source = `DATA_SOURCE:Data Source`,
         Comments = `OBS_FOOTNOTE:Observation footnote`,
         ICVACCategory = "Other acts of violence against a child not elsewhere classified",
         Year = `TIME_PERIOD:Time period`) %>%
  dplyr::select(Country, ISO3, Year, Sex, AgeGroup, Indicator, StatisticalUnit,
         Value, Source,
         ICVACCategory, Dataset, Comments)

TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE %>%
  filter(AgeGroup == "0-17" & Sex == "Total") %>%
  group_by(Country, Year) %>%
  summarise(Total = sum(Value)) %>%
  pivot_wider(names_from = Year, values_from = Total)

# Analysis prep
icvac9_aggregated_cwdwelfare <- TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE %>%
  filter(AgeGroup == "0-17" & Sex == "Total") %>%
  group_by(Country, ISO3,  Year) %>%
  summarize(Value = sum(Value))

icvac9_disagg_cwdwelfare <- TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE %>%
  filter(AgeGroup == "0-17" & Sex != "Total") %>%
  group_by(Country, ISO3,  Year, Sex) %>%
  summarize(Value = sum(Value))

icvac9_trends_cwdwelfare <- icvac9_aggregated_cwdwelfare %>%
  group_by(Country, ISO3) %>% 
  filter(n() > 5)

icvac9_comparison_cwdwelfare <- icvac9_aggregated_cwdwelfare %>%
  group_by(Country) %>%
  top_n(1, Year) %>%
  ungroup()


# TM Crime ----
TMEE_JJ_CHLD_VICTIM_CRIME <- folder9_fusion_TRANSMONEE_ECARO_1.0_.JJ_CHLD_VICTIM_CRIME....


iso %<>% dplyr::select(Country,ISO3, ISO2)

TMEE_JJ_CHLD_VICTIM_CRIME %<>%
  mutate(
    ISO3 = substr(`REF_AREA:Geographic area`, 1, 3),  # Extract first 3 characters
    Country = sub(".*: ", "", `REF_AREA:Geographic area`)  # Extract everything after ": "
  )

TMEE_JJ_CHLD_VICTIM_CRIME %<>%
  mutate(Indicator = "Number of child victims of crime (0-17 years) registered by the police during the year",
         Sex = substr(`SEX:Sex`, 1, 3),
         AgeGroup = substr(`AGE:Age`, 1,3))

TMEE_JJ_CHLD_VICTIM_CRIME %<>%
  mutate(Sex = case_when(
    grepl("T", Sex, ignore.case = TRUE)  ~  "Total",
    grepl("F", Sex, ignore.case = TRUE)  ~  "Female",
    grepl("M", Sex, ignore.case = TRUE)  ~  "Male",
    TRUE ~ Sex
  ))

TMEE_JJ_CHLD_VICTIM_CRIME %<>%
  mutate(AgeGroup = case_when(
    grepl("T", AgeGroup, ignore.case = TRUE)  ~  "0-17",
    grepl("Y0:", AgeGroup, ignore.case = TRUE)  ~  "0",
    grepl("Y01", AgeGroup, ignore.case = TRUE)  ~  "1",
    grepl("Y02", AgeGroup, ignore.case = TRUE)  ~  "2",
    grepl("Y03", AgeGroup, ignore.case = TRUE)  ~  "3",
    grepl("Y04", AgeGroup, ignore.case = TRUE)  ~  "4",
    grepl("Y05", AgeGroup, ignore.case = TRUE)  ~  "5",
    grepl("Y06", AgeGroup, ignore.case = TRUE)  ~  "6",
    grepl("Y07", AgeGroup, ignore.case = TRUE)  ~  "7",
    grepl("Y08", AgeGroup, ignore.case = TRUE)  ~  "8",
    grepl("Y09", AgeGroup, ignore.case = TRUE)  ~  "9",
    grepl("Y10", AgeGroup, ignore.case = TRUE)  ~  "10",
    grepl("Y11", AgeGroup, ignore.case = TRUE)  ~  "11",
    grepl("Y12", AgeGroup, ignore.case = TRUE)  ~  "12",
    grepl("Y13", AgeGroup, ignore.case = TRUE)  ~  "13",
    grepl("Y14", AgeGroup, ignore.case = TRUE)  ~  "14",
    grepl("Y15", AgeGroup, ignore.case = TRUE)  ~  "15",
    grepl("Y16", AgeGroup, ignore.case = TRUE)  ~  "16",
    grepl("Y17", AgeGroup, ignore.case = TRUE)  ~  "17",
    TRUE ~ AgeGroup
  ))

TMEE_JJ_CHLD_VICTIM_CRIME %<>%
  mutate(Dataset = "TMEE_JJ_CHLD_VICTIM_CRIME",
         StatisticalUnit = "Number",
         Value = `OBS_VALUE:Observation value`,
         Source = `DATA_SOURCE:Data Source`,
         Comments = `OBS_FOOTNOTE:Observation footnote`,
         ICVACCategory = "Other acts of violence against a child not elsewhere classified",
         Year = `TIME_PERIOD:Time period`) %>%
  dplyr::select(Country, ISO3, Year, Sex, AgeGroup, Indicator, StatisticalUnit,
         Value, Source,
         ICVACCategory, Dataset, Comments)

TMEE_JJ_CHLD_VICTIM_CRIME %>%
  filter(AgeGroup == "0-17" & Sex == "Total") %>%
  group_by(Country, Year) %>%
  summarise(Total = sum(Value)) %>%
  pivot_wider(names_from = Year, values_from = Total)

# Analysis prep
icvac9_aggregated_crime <- TMEE_JJ_CHLD_VICTIM_CRIME %>%
  filter(AgeGroup == "0-17" & Sex == "Total") %>%
  group_by(Country, ISO3,  Year) %>%
  summarize(Value = sum(Value))

icvac9_disagg_crime <- TMEE_JJ_CHLD_VICTIM_CRIME %>%
  filter(AgeGroup == "0-17" & Sex != "Total") %>%
  group_by(Country, ISO3,  Year, Sex) %>%
  summarize(Value = sum(Value))

icvac9_trends_crime <- icvac9_aggregated_crime %>%
  group_by(Country, ISO3) %>% 
  filter(n() > 5)

icvac9_comparison_crime <- icvac9_aggregated_crime %>%
  group_by(Country) %>%
  top_n(1, Year) %>%
  ungroup()



# TM Education ----
TMEE_PT_CHLD_VIOLENCE_EDUCATION<- folder9_fusion_TRANSMONEE_ECARO_1.0_.PT_CHLD_VIOLENCE_EDUCATION....


iso %<>% dplyr::select(Country,ISO3, ISO2)

TMEE_PT_CHLD_VIOLENCE_EDUCATION%<>%
  mutate(
    ISO3 = substr(`REF_AREA:Geographic area`, 1, 3),  # Extract first 3 characters
    Country = sub(".*: ", "", `REF_AREA:Geographic area`)  # Extract everything after ": "
  )

TMEE_PT_CHLD_VIOLENCE_EDUCATION%<>%
  mutate(Indicator = " Number of child victims of violence (0-17 years) registered by education authorities during the year",
         Sex = substr(`SEX:Sex`, 1, 3),
         AgeGroup = substr(`AGE:Age`, 1,3))

TMEE_PT_CHLD_VIOLENCE_EDUCATION%<>%
  mutate(Sex = case_when(
    grepl("T", Sex, ignore.case = TRUE)  ~  "Total",
    grepl("F", Sex, ignore.case = TRUE)  ~  "Female",
    grepl("M", Sex, ignore.case = TRUE)  ~  "Male",
    TRUE ~ Sex
  ))

TMEE_PT_CHLD_VIOLENCE_EDUCATION%<>%
  mutate(AgeGroup = case_when(
    grepl("T", AgeGroup, ignore.case = TRUE)  ~  "0-17",
    grepl("Y0:", AgeGroup, ignore.case = TRUE)  ~  "0",
    grepl("Y01", AgeGroup, ignore.case = TRUE)  ~  "1",
    grepl("Y02", AgeGroup, ignore.case = TRUE)  ~  "2",
    grepl("Y03", AgeGroup, ignore.case = TRUE)  ~  "3",
    grepl("Y04", AgeGroup, ignore.case = TRUE)  ~  "4",
    grepl("Y05", AgeGroup, ignore.case = TRUE)  ~  "5",
    grepl("Y06", AgeGroup, ignore.case = TRUE)  ~  "6",
    grepl("Y07", AgeGroup, ignore.case = TRUE)  ~  "7",
    grepl("Y08", AgeGroup, ignore.case = TRUE)  ~  "8",
    grepl("Y09", AgeGroup, ignore.case = TRUE)  ~  "9",
    grepl("Y10", AgeGroup, ignore.case = TRUE)  ~  "10",
    grepl("Y11", AgeGroup, ignore.case = TRUE)  ~  "11",
    grepl("Y12", AgeGroup, ignore.case = TRUE)  ~  "12",
    grepl("Y13", AgeGroup, ignore.case = TRUE)  ~  "13",
    grepl("Y14", AgeGroup, ignore.case = TRUE)  ~  "14",
    grepl("Y15", AgeGroup, ignore.case = TRUE)  ~  "15",
    grepl("Y16", AgeGroup, ignore.case = TRUE)  ~  "16",
    grepl("Y17", AgeGroup, ignore.case = TRUE)  ~  "17",
    TRUE ~ AgeGroup
  ))

TMEE_PT_CHLD_VIOLENCE_EDUCATION%<>%
  mutate(Dataset = "TMEE_JJ_CHLD_VICTIM_CRIME",
         StatisticalUnit = "Number",
         Value = `OBS_VALUE:Observation value`,
         Source = `DATA_SOURCE:Data Source`,
         Comments = `OBS_FOOTNOTE:Observation footnote`,
         ICVACCategory = "Other acts of violence against a child not elsewhere classified",
         Year = `TIME_PERIOD:Time period`) %>%
  dplyr::select(Country, ISO3, Year, Sex, AgeGroup, Indicator, StatisticalUnit,
         Value, Source,
         ICVACCategory, Dataset, Comments)

TMEE_PT_CHLD_VIOLENCE_EDUCATION%>%
  filter(AgeGroup == "0-17" & Sex == "Total") %>%
  group_by(Country, Year) %>%
  summarise(Total = sum(Value)) %>%
  pivot_wider(names_from = Year, values_from = Total)

# Analysis prep
icvac9_aggregated_edu <- TMEE_PT_CHLD_VIOLENCE_EDUCATION%>%
  filter(AgeGroup == "0-17" & Sex == "Total") %>%
  group_by(Country, ISO3,  Year) %>%
  summarize(Value = sum(Value))

icvac9_disagg_edu <- TMEE_PT_CHLD_VIOLENCE_EDUCATION%>%
  filter(AgeGroup == "0-17" & Sex != "Total") %>%
  group_by(Country, ISO3,  Year, Sex) %>%
  summarize(Value = sum(Value))

icvac9_trends_edu <- icvac9_aggregated_edu %>%
  group_by(Country, ISO3) %>% 
  filter(n() > 5)

icvac9_comparison_edu <- icvac9_aggregated_edu %>%
  group_by(Country) %>%
  top_n(1, Year) %>%
  ungroup()




# TM Health ----
TMEE_PT_CHLD_VIOLENCE_HEALTHCARE<- folder9_fusion_TRANSMONEE_ECARO_1.0_.PT_CHLD_VIOLENCE_HEALTHCARE....


iso %<>% dplyr::select(Country,ISO3, ISO2)

TMEE_PT_CHLD_VIOLENCE_HEALTHCARE%<>%
  mutate(
    ISO3 = substr(`REF_AREA:Geographic area`, 1, 3),  # Extract first 3 characters
    Country = sub(".*: ", "", `REF_AREA:Geographic area`)  # Extract everything after ": "
  )

TMEE_PT_CHLD_VIOLENCE_HEALTHCARE%<>%
  mutate(Indicator = " Number of child victims of violence (0-17 years) registered by healthcare authorities during the year",
         Sex = substr(`SEX:Sex`, 1, 3),
         AgeGroup = substr(`AGE:Age`, 1,3))

TMEE_PT_CHLD_VIOLENCE_HEALTHCARE%<>%
  mutate(Sex = case_when(
    grepl("T", Sex, ignore.case = TRUE)  ~  "Total",
    grepl("F", Sex, ignore.case = TRUE)  ~  "Female",
    grepl("M", Sex, ignore.case = TRUE)  ~  "Male",
    TRUE ~ Sex
  ))

TMEE_PT_CHLD_VIOLENCE_HEALTHCARE%<>%
  mutate(AgeGroup = case_when(
    grepl("T", AgeGroup, ignore.case = TRUE)  ~  "0-17",
    grepl("Y0:", AgeGroup, ignore.case = TRUE)  ~  "0",
    grepl("Y01", AgeGroup, ignore.case = TRUE)  ~  "1",
    grepl("Y02", AgeGroup, ignore.case = TRUE)  ~  "2",
    grepl("Y03", AgeGroup, ignore.case = TRUE)  ~  "3",
    grepl("Y04", AgeGroup, ignore.case = TRUE)  ~  "4",
    grepl("Y05", AgeGroup, ignore.case = TRUE)  ~  "5",
    grepl("Y06", AgeGroup, ignore.case = TRUE)  ~  "6",
    grepl("Y07", AgeGroup, ignore.case = TRUE)  ~  "7",
    grepl("Y08", AgeGroup, ignore.case = TRUE)  ~  "8",
    grepl("Y09", AgeGroup, ignore.case = TRUE)  ~  "9",
    grepl("Y10", AgeGroup, ignore.case = TRUE)  ~  "10",
    grepl("Y11", AgeGroup, ignore.case = TRUE)  ~  "11",
    grepl("Y12", AgeGroup, ignore.case = TRUE)  ~  "12",
    grepl("Y13", AgeGroup, ignore.case = TRUE)  ~  "13",
    grepl("Y14", AgeGroup, ignore.case = TRUE)  ~  "14",
    grepl("Y15", AgeGroup, ignore.case = TRUE)  ~  "15",
    grepl("Y16", AgeGroup, ignore.case = TRUE)  ~  "16",
    grepl("Y17", AgeGroup, ignore.case = TRUE)  ~  "17",
    TRUE ~ AgeGroup
  ))

TMEE_PT_CHLD_VIOLENCE_HEALTHCARE%<>%
  mutate(Dataset = "TMEE_JJ_CHLD_VICTIM_CRIME",
         StatisticalUnit = "Number",
         Value = `OBS_VALUE:Observation value`,
         Source = `DATA_SOURCE:Data Source`,
         Comments = `OBS_FOOTNOTE:Observation footnote`,
         ICVACCategory = "Other acts of violence against a child not elsewhere classified",
         Year = `TIME_PERIOD:Time period`) %>%
  dplyr::select(Country, ISO3, Year, Sex, AgeGroup, Indicator, StatisticalUnit,
         Value, Source,
         ICVACCategory, Dataset, Comments)

TMEE_PT_CHLD_VIOLENCE_HEALTHCARE%>%
  filter(AgeGroup == "0-17" & Sex == "Total") %>%
  group_by(Country, Year) %>%
  summarise(Total = sum(Value)) %>%
  pivot_wider(names_from = Year, values_from = Total)

# Analysis prep
icvac9_aggregated_health <- TMEE_PT_CHLD_VIOLENCE_HEALTHCARE%>%
  filter(AgeGroup == "0-17" & Sex == "Total") %>%
  group_by(Country, ISO3,  Year) %>%
  summarize(Value = sum(Value))

icvac9_disagg_health <- TMEE_PT_CHLD_VIOLENCE_HEALTHCARE%>%
  filter(AgeGroup == "0-17" & Sex != "Total") %>%
  group_by(Country, ISO3,  Year, Sex) %>%
  summarize(Value = sum(Value))

icvac9_trends_health <- icvac9_aggregated_health %>%
  group_by(Country, ISO3) %>% 
  filter(n() > 5)

icvac9_comparison_health <- icvac9_aggregated_health %>%
  group_by(Country) %>%
  top_n(1, Year) %>%
  ungroup()



# Binding and saving ----
icvac9 <- rbind(TMEE_JJ_CHLD_VICTIM_CRIME, TMEE_PT_CHILD_DISAB_VIOLENCE_WELFARE,
                TMEE_PT_CHLD_VIOLENCE_EDUCATION, TMEE_PT_CHLD_VIOLENCE_HEALTHCARE,
                TMEE_PT_CHLD_VIOLENCE_WELFARE)

write.csv(icvac9, "data/output/disaggregated/icvac_9.csv", row.names = FALSE)
