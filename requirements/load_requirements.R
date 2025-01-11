# This script defines a function to load all R scripts from the 'requirements' folder.

load_requirements <- function(folder = "requirements") {
  # List all the R script files in the folder
  files <- list.files(path = folder, pattern = "\\.R$", full.names = TRUE)
  
  # Source each file
  for (file in files) {
    source(file)
  }
}