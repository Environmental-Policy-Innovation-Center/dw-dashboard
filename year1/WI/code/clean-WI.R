library(tidyverse)
library(data.table)
library(janitor)

clean_wi <- function() {
  
  # (54,17) -> (46,19)
  wi_fund_raw <- fread("year1/WI/data/49-Wisconsin_PPL.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    ## remove non project rows
    filter(!is.na(project_points)) %>%
    # define disadvantaged for the non-EC projects as total points > 59 per data dictionary
    mutate(disadvantaged = case_when(
      as.numeric(str_replace_all(total_pf_points,"[^0-9.]", "")) > 59 ~ "Yes",
      TRUE ~ "No"),
      # determine project type by keyword search for non-EC projects
      project_type = case_when(grepl("lsl", project_description, ignore.case = T) ~ "Lead",
                               grepl("pfas", project_description, ignore.case=T) ~ "Emerging Contaminants",
                               TRUE ~ "General"),)
  
  # (4,17) -> (4,19)
  wi_ec_fund <- fread("year1/WI/data/49-Wisconsin_EC_Funding_List.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # define disadvantaged as financial need points > 29 for only EC projects
    mutate(disadvantaged = case_when(as.numeric(str_replace_all(financial_need_points,"[^0-9.]", "")) > 29 ~ "Yes",
                                     TRUE ~ "No"),
           project_type = "Emerging Contaminants")
  
  # (50,19)
  wi_fund_raw <- bind_rows(wi_fund_raw, wi_ec_fund)
  
  # -> (50,11)
  wi_clean <- wi_fund_raw %>%
    ## make relevant columns numbers
    mutate(## sum of columns, getting rid of $ and comma and replacing blanks with zero
      funding_amount = as.numeric(str_replace_all(requested_project_costs,"[^0-9.]", "")),
      principal_forgiveness_amount = as.numeric(str_replace_all(pf_estimate,"[^0-9.]", "")),
      ## make principal forgiveness NAs zeros
      principal_forgiveness_amount = replace_na(principal_forgiveness_amount, 0),
      population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
      requested_amount = as.numeric(str_replace_all(requested_project_costs,"[^0-9.]", "")),
    ) %>%
    ## non-numeric columns
    mutate(funding_status = case_when(!is.na(funding_amount) ~ "Funded",
                                      TRUE ~ "Not Funded"),
           ## get rid of numbers from borrower name
           borrower = str_to_title(str_replace_all(municipality, "[0-9.\\#]", "")),
           state_score = str_replace_all(priority_score,"[^0-9.]",""),
           cities_served = borrower,
           state = "Wisconsin",
           category = "3"
    ) %>%
    ## keep relevant columns
    select(borrower, cities_served, project_description, project_type,
           state_score, funding_amount, principal_forgiveness_amount,
           disadvantaged, population, funding_status, state, category)
  
  rm(list=setdiff(ls(), "wi_clean"))
  
  return(wi_clean)
}