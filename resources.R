### Inputs ----
library(tidyverse)
library(data.table)
library(janitor)
options(scipen=999)


### Cleaning Functions ----


# takes a numeric column, removes non-numeric characters, then returns as a string column
clean_numeric_string <- function(column) {
  
  column_numeric <- convert_to_numeric(column)
  
  column_string <- as.character(column_numeric, FALSE)
  column_string <- replace_na(column_string, "No Information")
  
  return(column_string)
}

# takes a numeric column, removes non-numeric characters, then returns a numeric column
# can also automatically replace NAs with 0 for being able to add columns together
# but note that those columns will likely then need to be processed as strings separately
convert_to_numeric <- function(column, fill_na_0 = FALSE) {
  
  column_numeric <- as.numeric(str_replace_all(column, "[^0-9.]", ""))
  
  if (fill_na_0 == TRUE) {
    column_numeric <- replace_na(column_numeric, 0)
  }
  
  return(column_numeric)
}

### Test Functions ---- 


# top level function for running all tests
run_tests <- function(df) {
  check_na_warnings(df)
  check_required_columns(df)
  check_column_types(df)
}



# Returns whether any columns are not 0/100% NA
check_na_warnings <- function(df) {
  
  # calculate % NA for each column in the data frame
  na_summary <- percent_na_per_column(df)
  
  # Identify columns where Percent_NA is neither 0 nor 100
  problematic_columns <- na_summary %>%
    filter(Percent_NA != 0 & Percent_NA != 100)
  
  # Issue warnings if there are any problematic columns
  if (nrow(problematic_columns) > 0) {
    warning("FAIL: The following columns have NA percentages that are neither 0% nor 100%:\n",
            paste0(problematic_columns$Column, ": ", problematic_columns$Percent_NA, "%", collapse = "\n"))
  } else {
    message("PASS: All columns have NA percentages of either 0% or 100%.")
  }
}

# for each column in the dataframe, calculate % NA and create a summary table
percent_na_per_column <- function(df) {
  df %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), names_to = "Column", values_to = "Percent_NA") %>%
    as_tibble()
}


# Returns whether any of the required columns are missing from the final dataset
check_required_columns <- function(df) {
  # Get the actual columns present in the dataframe
  actual_columns <- colnames(df)
  
  required_columns <- c("community_served", "borrower", "pwsid", "project_id", "project_name", "project_type", "project_cost",
                        "requested_amount", "funding_amount", "principal_forgiveness", "population", "project_description",
                        "disadvantaged", "project_rank", "project_score", "expecting_funding", "state", "state_fiscal_year")
  
  # Identify missing columns
  missing_columns <- setdiff(required_columns, actual_columns)
  
  # Check if there are missing columns and issue a warning if so
  if (length(missing_columns) > 0) {
    warning("FAIL: The following required columns are missing: ", 
            paste(missing_columns, collapse = ", "))
  } else {
    message("PASS: All required columns are present.")
  }
}






# Define the test function
check_column_types <- function(df) {
  # Define vectors of column names to be checked
  string_columns <- c("community_served", "borrower", "pwsid", "project_id", "project_name", "project_type", "project_cost",
                      "requested_amount", "funding_amount", "principal_forgiveness", "population", "project_description",
                      "disadvantaged", "project_rank", "project_score", "expecting_funding", "state", "state_fiscal_year") 
  
  
  string_check <- map(string_columns, ~ {
    if (!is.character(df[[.x]])) {
      return(paste("Column", .x, "should be of type character."))
    }
    NULL
  }) %>% compact()  # Remove NULL values
  
  # Print warnings for string columns
  if (length(string_check) > 0) {
    warning("FAIL: ", paste(string_check, collapse = "\n"))
  }
  else {
    message("PASS: All text columns of correct type.")
  }
}