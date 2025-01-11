# Define a function to detect countries with at least 5 consecutive non-missing values
has_consecutive_data <- function(values, n = 5) {
  non_na_run <- rle(!is.na(values))  # Get the run lengths of non-NA values
  any(non_na_run$values == TRUE & non_na_run$lengths >= n)  # Check if any run is of length >= n
}