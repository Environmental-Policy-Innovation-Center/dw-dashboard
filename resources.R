### Inputs ----
library(tidyverse)
library(data.table)
library(janitor)
library(googledrive)
library(googlesheets4)
library(aws.s3)
options(scipen=999)


### Data Viz Template Settings ----

# fonts: 
sysfonts::font_add_google("Lato")
showtext::showtext_auto()

# theme: 
# legend position is right, text size is at least 10, and text is Lato
epic_chart_theme <- theme_minimal() + 
  theme(legend.position = "right", 
        text = element_text(size = 11, family = "Lato"), 
        legend.text = element_text(size = 10, family = "Lato"), 
        legend.title = element_text(size = 11, family = "Lato"), 
        axis.text.x = element_text(margin = margin(t = 10, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, 
                                                    b = 0, l = 0)), 
        axis.text.y = element_text(margin = margin(t = 0, r = 10, 
                                                   b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0))) 

cont_palette <- colorRampPalette(c("#172f60","#4ea324"))

cat_palette <- colorRampPalette(c("#172f60","#1054a8",
                                  "#791a7b","#de9d29","#4ea324"))

cat_palette_pastel <- colorRampPalette(c("#526489","#527CAF",
                                         "#B077B2","#E4BE7C",
                                         "#b15712","#82AB6E"))


### Drive Data Access Functions ----

# Because these datasets are pulled from work-in-progress Google Drive sheets, code that relies on them
# can be fragile. As such, these functions exist to standardize pulling and cleaning the data in one location
# so any changes to column names, types, or additional features can be handled here exclusively.

get_financial <- function(state_name, years_list) {
  
  URL <- "https://docs.google.com/spreadsheets/d/10NcgSJAZedRNDTq7_9-UJUefF6tQ6RYhvVSib_Dc-3Y/edit?usp=sharing"
  
  financial <- data.frame(read_sheet(URL, sheet="Financial Data", skip=1))
  
  financial <- financial %>%
    clean_names() %>%
    filter(state == state_name) %>%
    select(-notes) %>%
    mutate(state_fiscal_year = as.character(state_fiscal_year),
           state_fiscal_year = factor(state_fiscal_year, levels=years_list)) %>%
    # each of these are mutated individually because some rows come in as text, numerical or a list, depending on their contents
    # and mutating with across() produces errors or warnings depending on the context, here at least it is only warnings
    # that can be muted, but the resulting dataframe is still as intended
    mutate(total_fcg = convert_to_numeric(total_fcg),
           ffy22_fcg = convert_to_numeric(ffy22_fcg),
           ffy23_fcg = convert_to_numeric(ffy23_fcg),
           ffy24_fcg = convert_to_numeric(ffy24_fcg),
           ffy25_fcg = convert_to_numeric(ffy25_fcg),
           ffy26_fcg = convert_to_numeric(ffy26_fcg),
           nonsrf_state_funds = convert_to_numeric(nonsrf_state_funds),
           arpa_funds = convert_to_numeric(arpa_funds),
           reallotments = convert_to_numeric(reallotments),
           cw_transferred_to_dw = convert_to_numeric(cw_transferred_to_dw),
           dw_transferred_to_cw = convert_to_numeric(dw_transferred_to_cw),
           unutilized_fcg = convert_to_numeric(unutilized_fcg),
           leveraged_funds = convert_to_numeric(leveraged_funds),
           total_funding_available = convert_to_numeric(total_funding_available),
           total_setasides_amt = convert_to_numeric(total_setasides_amt),
           total_setasides_pct = convert_to_numeric(total_setasides_pct),
           dac_pf_pct_base = convert_to_numeric(dac_pf_pct_base),
           dac_pf_amt_base = convert_to_numeric(dac_pf_amt_base),
           dis_pf_amt_base = convert_to_numeric(dis_pf_amt_base),
           dis_pf_pct_base = convert_to_numeric(dis_pf_pct_base),
           total_pf_amt = convert_to_numeric(total_pf_amt),
           total_pf_pct = convert_to_numeric(total_pf_pct),
           gpr_cost = convert_to_numeric(gpr_cost))
  
  return(financial)
}


get_set_asides <- function(state_name, yaers_list) {
  
  URL <- "https://docs.google.com/spreadsheets/d/10NcgSJAZedRNDTq7_9-UJUefF6tQ6RYhvVSib_Dc-3Y/edit?usp=sharing"
  set_asides_allowances <- data.frame(read_sheet(URL, "Set Asides", skip=1))
  
  set_asides_allowances <- set_asides_allowances %>%
    clean_names() %>%
    filter(state==state_name) %>%
    select(-notes) %>%
    mutate(state_fiscal_year = as.character(state_fiscal_year),
           state_fiscal_year = factor(state_fiscal_year, levels=years_list)) %>%
    
    mutate(ffy22_sa_amt = convert_to_numeric(ffy22_sa_amt),
           ffy22_sa_pct = convert_to_numeric(ffy22_sa_pct),
           ffy23_sa_amt = convert_to_numeric(ffy23_sa_amt),
           ffy23_sa_pct = convert_to_numeric(ffy23_sa_pct),
           ffy24_sa_amt = convert_to_numeric(ffy24_sa_amt),
           ffy24_sa_pct = convert_to_numeric(ffy24_sa_pct),
           ffy25_sa_amt = convert_to_numeric(ffy25_sa_amt),
           ffy25_sa_pct = convert_to_numeric(ffy25_sa_pct),
           ffy26_sa_amt = convert_to_numeric(ffy26_sa_amt),
           ffy26_sa_pct = convert_to_numeric(ffy26_sa_pct),
           total_sa_amt = convert_to_numeric(total_sa_amt),
           total_sa_pct = convert_to_numeric(total_sa_pct)
           ) %>%
    #TODO: Modify this as needed once water team updates naming conventions
    mutate(allowance = case_when(
      allowance == "Administration and Technical Assistance (up to 4%)" ~ "Admin & TA",
      allowance == "Small System (<10,000 population) Technical Assistance (up to 2%)" ~ "Small System TA",
      allowance == "State Program Management (10%)" ~ "State Program Management",
      allowance == "Local Assistance and other State Programs (up to 15%)" ~ "Local Assistance & Other",
      allowance == "Local Assistance and other State Programs (up to 10%)" ~ "Local Assistance & Other",
      TRUE ~ "Missing Allowance"),
      )
  
  return(set_asides_allowances)
}

## takes a numeric column and returns one formatted with dollar sign and commas
## IE 1000000 -> $1,000,000
format_currency <- function(num_col) {
  #typically this will already have been done, but a failsafe
  num_col <- as.numeric(num_col)
  
  # Apply formatting for each number in the column
  formatted_col <- sapply(num_col, function(num) {
    formatted <- formatC(num, format = "f", big.mark = ",", digits = 2)
    paste0("$", formatted)
  })
  return(formatted_col)
}

## takes a numeric column of the default percent setting from google sheets
## and returns a string column in a user-friendly format 
## IE .16 -> 16%
format_percent <- function(x) {
  # Format the numbers as percentages, rounding to 0 decimal places
  formatted <- paste0(round(x * 100), "%")
  
  return(formatted)
}



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