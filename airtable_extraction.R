library(httr)
library(jsonlite)
library(plyr)
library(dplyr)
library(purrr)

# Set your Airtable PAT (personal access token)
pat <- "patqCZM9fmCLkqfwA.fc3c7adf4d431584e4bba65ab3ccdc9980caf6dbb1b0f891681c0ceb26e4217c"

# Base ID and Table Name (replace with your values)
base_id <- "appPzaOmtJXIwyfjb"
table_name <- "tblmQ4pkBjyi2RAfU"

# Construct the URL for the API request
url <- paste0("https://api.airtable.com/v0/", base_id, "/", table_name)

# Make a GET request to Airtable with proper uthentication
response <- GET(
  url,
  add_headers(Authorization = paste("Bearer", pat))  # Include the PAT in the Authorization header
)

# Check for any errors in the response
if (status_code(response) == 200) {
  # Parse the JSON response
  content <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # Display the records
  records <- content$records
  print(records)
} else {
  # Display the error message if the request fails
  print(content(response, "text"))
}

indicators <- records

indicators <- indicators %>% 
  select(`fields._icvac category`, `fields.ICVAC code`, fields.ID)

indicators <- indicators %>%
  mutate(ICVACCategory = map_chr(`fields._icvac category`, ~ paste(.x, collapse = ",")),
         ICVACCode = map_chr(`fields.ICVAC code`, ~ paste(.x, collapse = ",")),
         ID = fields.ID)

indicators <- indicators %>%
  select(ICVACCategory, ICVACCode, ID)

write.csv(indicators, "data/utilities/data_codes.csv", row.names = FALSE)

# Extract ISO codes ----

# Set your Airtable PAT (personal access token)
pat <- "patqCZM9fmCLkqfwA.fc3c7adf4d431584e4bba65ab3ccdc9980caf6dbb1b0f891681c0ceb26e4217c"

# Base ID and Table Name (replace with your values)
base_id <- "appPzaOmtJXIwyfjb"
table_name <- "tblvFqWhGIQcROac0"

# Construct the URL for the API request
url <- paste0("https://api.airtable.com/v0/", base_id, "/", table_name)

# Make a GET request to Airtable with proper uthentication
response <- GET(
  url,
  add_headers(Authorization = paste("Bearer", pat))  # Include the PAT in the Authorization header
)

# Check for any errors in the response
if (status_code(response) == 200) {
  # Parse the JSON response
  content <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # Display the records
  records <- content$records
  print(records)
} else {
  # Display the error message if the request fails
  print(content(response, "text"))
}

iso <- records

iso %<>%
  mutate(ISO3 = `fields.ISO 3166-1 Alpha 3-Codes`,
         ISO2 = `fields.ISO 3166-1 Alpha 2-Codes`,
         CountryName = fields.ID,
         UNICEFRegion = fields.UNICEF) %>%
  select(CountryName, UNICEFRegion, ISO3, ISO2)

write.csv(iso, "data/utilities/iso_codes.csv", row.names = FALSE)


# Extract Records codes ----

# Set your Airtable PAT (personal access token)
pat <- "patqCZM9fmCLkqfwA.fc3c7adf4d431584e4bba65ab3ccdc9980caf6dbb1b0f891681c0ceb26e4217c"

# Base ID and Table Name (replace with your values)
base_id <- "appPzaOmtJXIwyfjb"
table_name <- "tblvSwh66gDX374MS"
view_name <- "Checked"

# Construct the URL for the API request
url <- paste0("https://api.airtable.com/v0/", base_id, "/", table_name)

# Function to get all records and handle varying record structures
get_all_records <- function(base_id, table_name, api_key, view_name) {
  offset <- NULL  # Initialize the offset for pagination
  result <- list()  # List to hold all the results
  
  repeat {
    # Construct the request URL
    url <- paste0("https://api.airtable.com/v0/", base_id, "/", table_name)
    
    # Define the query parameters
    query_params <- list(
      view = view_name,
      offset = offset  # Include offset if not null
    )
    
    # Make the GET request to Airtable API with the correct Authorization header
    response <- GET(
      url,
      query = query_params,
      add_headers(Authorization = paste("Bearer", api_key))  # Correct Bearer token auth
    )
    
    # Check if the response was successful
    if (status_code(response) != 200) {
      stop("Error: Failed to fetch data. Status code: ", status_code(response))
    }
    
    # Parse the JSON response
    response_content <- fromJSON(content(response, "text"), flatten = TRUE)
    
    # Append the records to the result list
    result <- append(result, response_content$records)
    
    # Check if there is an offset for the next set of records
    if (!is.null(response_content$offset)) {
      offset <- response_content$offset  # Set the new offset for the next page
    } else {
      break  # No more pages, break the loop
    }
  }
  
  # Handling varying number of columns across records
  # Find all possible column names
  all_fields <- unique(unlist(lapply(result, names)))
  
  # Standardize all records to have the same columns
  standardized_records <- lapply(result, function(record) {
    record[setdiff(all_fields, names(record))] <- NA  # Add missing columns as NA
    return(as.data.frame(record, stringsAsFactors = FALSE))
  })
  
  # Combine all standardized records into a single dataframe
  result_df <- bind_rows(standardized_records)
  
  return(result_df)
}
all_records_df <- get_all_records(base_id, table_name, pat, view_name)


# Display all the records
print(all_records)



iso %<>%
  mutate(ISO3 = `fields.ISO 3166-1 Alpha 3-Codes`,
         ISO2 = `fields.ISO 3166-1 Alpha 2-Codes`,
         CountryName = fields.ID,
         UNICEFRegion = fields.UNICEF) %>%
  select(CountryName, UNICEFRegion, ISO3, ISO2)

write.csv(iso, "data/utilities/iso_codes.csv", row.names = FALSE)
