source("requirements/libraries.R")
rm(list = ls())

# Import ----
icvac1 <- read.csv("data/output/disaggregated/icvac_1.csv", encoding = "UTF-8")
icvac2 <- read.csv("data/output/disaggregated/icvac_2.csv", encoding = "UTF-8")
icvac2_mics <- read.csv("data/output/disaggregated/icvac_2_mics.csv", encoding = "UTF-8")
icvac3 <- read.csv("data/output/disaggregated/icvac_3.csv", encoding = "UTF-8")
icvac4 <- read.csv("data/output/disaggregated/icvac_4.csv", encoding = "UTF-8")
icvac9 <- read.csv("data/output/disaggregated/icvac_9.csv", encoding = "UTF-8")
icvac10 <- read.csv("data/output/disaggregated/icvac_10.csv", encoding = "UTF-8")
icvac10_mics <- read.csv("data/output/disaggregated/icvac_11_mics.csv", encoding = "UTF-8")
icvac11 <- read.csv("data/output/disaggregated/icvac_11.csv", encoding = "UTF-8")

cols <- c("Country","ISO3","Year","Sex","AgeGroup","WealthQuintile","Indicator","StatisticalUnit", 
          "Value","Source","ICVACCategory","Dataset", "WealthQuintile",
          "IDPstatus", "SettlementType", "RelationshipPerpetrator", "Comments", "Reliability") 

# Get all objects in the environment whose names contain "icvac"
icvac_objects <- ls(pattern = "icvac")

# Loop through each object
for (obj_name in icvac_objects) {
  # Get the object
  obj <- get(obj_name)
  
  # Check if it is a data.frame
  if (is.data.frame(obj)) {
    # Add any missing columns with NA
    missing_cols <- setdiff(cols, colnames(obj))
    if (length(missing_cols) > 0) {
      obj[missing_cols] <- NA
    }
    
    # Check if "Comments" or "Reliability" columns exist, and replace "" with NA
    if ("Comments" %in% colnames(obj)) {
      obj$Comments[obj$Comments == ""] <- NA
    }
    
    if ("Reliability" %in% colnames(obj)) {
      obj$Reliability[obj$Reliability == ""] <- NA
    }
    
    # Assign the modified object back to its original name
    assign(obj_name, obj)
  }
}

# Bind all in the same dataframe ---
# Initialize an empty list to store all icvac objects
icvac_list <- list()

# Loop through each object and add it to the list
for (obj_name in icvac_objects) {
  # Get the object
  obj <- get(obj_name)
  
  # Check if it is a data.frame, and if so, add it to the list
  if (is.data.frame(obj)) {
    icvac_list[[obj_name]] <- obj
  }
}
# Combine all data frames in the list into a single data frame using rbind
icvac <- do.call(rbind, icvac_list)

# Save output
write.csv(icvac, "data/output/icvac.csv", row.names = FALSE)

icvac %>%
  filter(is.na(Year) & !is.na(Value)) %>%
  distinct(Country, Indicator)

