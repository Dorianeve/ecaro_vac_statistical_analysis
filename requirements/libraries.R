# List of required packages
required_packages <- c(
  "dotenv", "httr", "tibble", "tidyverse", 
  "dplyr", "ggplot2", "readr", "DT", 
  "plotly", "viridis", "readr", "progress",
  "countrycode", "writexl", "lubridate", "openxlsx",
  "stringr", "scales", "readxl", "ggrepel",
  "jsonlite", "stringi", "urltools", "foreign",
  "fs", "purrr", "magrittr", "leaflet", "sf",
  "rnaturalearth", "highcharter", "shiny", "dygraphs", "xts", "shiny.i18n"
)

# foreign for spss reading
# fs for file system operations
# Function to check, install if necessary, and load packages
load_libraries <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Load all required packages
load_libraries(required_packages)
