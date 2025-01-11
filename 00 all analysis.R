# all statistical analysis

# Extracting -----
source("01 data extraction.R")

# Cleaning and consolidating ----
script_files <- list.files(path = "cleaning scripts", pattern = "\\.R$", full.names = TRUE)
# Source each script
for (script in script_files) {
  message("Running script: ", script)
  source(script)
}

# Analysing ----
script_files <- list.files(path = "analysis scripts", pattern = "\\.R$", full.names = TRUE)
# Source each script
for (script in script_files) {
  message("Running script: ", script)
  source(script)
}

rm(list = ls())