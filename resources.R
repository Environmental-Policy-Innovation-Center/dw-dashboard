### Inputs ----
library(tidyverse)
library(data.table)
library(janitor)
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

get_financial <- function(state_name) {
  
  URL <- "https://docs.google.com/spreadsheets/d/10NcgSJAZedRNDTq7_9-UJUefF6tQ6RYhvVSib_Dc-3Y/edit?usp=sharing"
  
  financial <- data.frame(read_sheet(URL, sheet="FinancialData"))
  
  financial <- financial %>%
    clean_names() %>%
    filter(state == state_name) %>%
    select(-notes) %>%
    mutate(state_fiscal_year = as.character(state_fiscal_year),
           state_fiscal_year = factor(state_fiscal_year, levels=years_list)) %>%
    # each of these are mutated individually because some rows come in as text, numerical or a list, depending on their contents
    # and mutating with across() produces errors or warnings depending on the context, here at least it is only warnings
    # that can be muted, but the resulting dataframe is still as intended
    mutate(fed_cap_grant_str = str_squish(fed_cap_grant_total),
           fed_cap_grant_total = convert_to_numeric(fed_cap_grant_total),
           state_funds = convert_to_numeric(state_funds),
           re_allotment_funds_grants = convert_to_numeric(re_allotment_funds_grants),
           #transferred_cwsrf_funds = convert_to_numeric(transferred_cwsrf_funds),
           unutilized_un_drawn_funds_carryover = convert_to_numeric(unutilized_un_drawn_funds_carryover),
           leveraged_funds = convert_to_numeric(leveraged_funds),
           total_funding_available_for_projects = convert_to_numeric(total_funding_available_for_projects),
           set_asides_amount = convert_to_numeric(set_asides_amount),
           set_asides_percent = convert_to_numeric(set_asides_percent),
           pf_for_da_cs_under_base = convert_to_numeric(pf_for_da_cs_under_base),
           pf_for_da_cs_under_base_of_fed_cap_grant_12_35 = convert_to_numeric(pf_for_da_cs_under_base_of_fed_cap_grant_12_35),
           principal_forgiveness_for_any_eligible_applicant_under_base = convert_to_numeric(principal_forgiveness_for_any_eligible_applicant_under_base),
           pf_for_eligibile_of_fed_cap_grant_under_base_14 = convert_to_numeric(pf_for_eligibile_of_fed_cap_grant_under_base_14),
           principal_forgiveness_amount_total = convert_to_numeric(principal_forgiveness_amount_total),
           pf_of_fed_cap_grant = convert_to_numeric(pf_of_fed_cap_grant),
           gpr_component_cost = convert_to_numeric(gpr_component_cost))
  
  return(financial)
}


get_set_asides <- function(state_name) {
  
  URL <- "https://docs.google.com/spreadsheets/d/10NcgSJAZedRNDTq7_9-UJUefF6tQ6RYhvVSib_Dc-3Y/edit?usp=sharing"
  set_asides_allowances <- data.frame(read_sheet(URL, "SetAsides"))
  
  set_asides_allowances <- set_asides_allowances %>%
    clean_names() %>%
    filter(state==state_name) %>%
    select(-notes) %>%
    mutate(state_fiscal_year = as.character(state_fiscal_year),
           state_fiscal_year = factor(state_fiscal_year, levels=years_list)) %>%
    mutate(amount = convert_to_numeric(amount),
           percentage = convert_to_numeric(percentage)) %>%
    #TODO: Modify this as needed once water team updates naming conventions
    mutate(allowance = case_when(
      allowance == "Administration and Technical Assistance (up to 4%)" ~ "Admin & TA",
      allowance == "Small System (<10,000 population) Technical Assistance (up to 2%)" ~ "Small System TA",
      allowance == "State Program Management (10%)" ~ "State Program Management",
      allowance == "Local Assistance and other State Programs (up to 15%)" ~ "Local Assistance & Other",
      allowance == "Local Assistance and other State Programs (up to 10%)" ~ "Local Assistance & Other",
      TRUE ~ "Missing Case When Statement"),
      fed_cap_grant = case_when(
        fed_cap_grant == "General DWSRF (Base)" ~ "Base",
        fed_cap_grant == "General DWSRF (BILGeneral Supplemental)" ~ "BIL Gen/Supp",
        fed_cap_grant == "DWSRF LSLR Program" ~ "LSLR",
        fed_cap_grant == "DWSRF EC Program" ~ "EC",
        TRUE ~ "Missing Fed Cap Grant"
      ))
  
  return(set_asides_allowances)
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