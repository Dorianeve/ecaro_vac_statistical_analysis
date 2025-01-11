source("requirements/libraries.R")
rm(list = ls())

# Automated extraction from each folder and subfolder ----

# List of folders (folder names to loop through)
folder_names <- c(1, 2, 3, 4, 5, 9, 10, 11)

# Base directory
base_dir <- "data/input"

# Function to dynamically load CSV, TSV, and Excel files
load_data_files <- function(base_dir, folder_names) {
  # Loop through each folder name
  for (folder in folder_names) {
    # Construct the full folder path
    dir_path <- file.path(base_dir, as.character(folder))
    # Check if the directory exists
    if (dir_exists(dir_path)) {
      # Get list of all files recursively in the directory
      files <- dir_ls(dir_path, recurse = TRUE, type = "file", glob = "*.csv|*.tsv|*.xls|*.xlsx")
      # Loop through each file
      for (file_path in files) {
        # Extract file extension to determine the type
        file_ext <- tools::file_ext(file_path)
        # Create a valid object name based on folder and file name
        # E.g., "folder1_file.csv" becomes "folder1_file"
        file_name <- str_replace_all(basename(file_path), "\\.csv|\\.tsv|\\.xls|\\.xlsx", "")
        object_name <- paste0("folder", folder, "_", file_name)
        object_name <- make.names(object_name)  # Ensure it's a valid R object name
        if (file_ext == "csv") {
          # Load CSV file with UTF-8 encoding
          assign(object_name, read_csv(file_path, locale = locale(encoding = "UTF-8")), envir = .GlobalEnv)
        } else if (file_ext == "tsv") {
          # Load TSV file with UTF-8 encoding
          assign(object_name, read_tsv(file_path, locale = locale(encoding = "UTF-8")), envir = .GlobalEnv)
        } else if (file_ext %in% c("xls", "xlsx")) {
          # Read the Excel file and find the header row dynamically
          # Read first 5 rows to identify the header (assumption: headers are often in the first few rows)
          temp_data <- read_excel(file_path, sheet = 1, n_max = 5, col_names = FALSE)
          # Find the first row that contains non-NA values, assuming this is the header
          header_row <- which.max(rowSums(!is.na(temp_data)))
          # Load the Excel file with the correct header row (no locale for Excel)
          assign(object_name, read_excel(file_path, sheet = 1, skip = header_row - 1), envir = .GlobalEnv)
        }
      }
    } else {
      message(paste("Directory does not exist:", dir_path))
    }
  }
}

# Call the function to load all files in the specified folders
load_data_files(base_dir, folder_names)

