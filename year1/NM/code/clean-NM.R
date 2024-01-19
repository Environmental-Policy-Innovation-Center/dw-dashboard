library(tidyverse)
library(data.table)
library(janitor)

clean_nm <- function() {
  
  # (6,13)
  nm_raw <- fread("year1/NM/data/31-NewMexico_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (6,13)
  nm_clean <- nm_raw %>%
    # drop columns
    select(-mandatory_cap_grant_subsidy, -interest_rate, -designation) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population, "[^0-9.]", "")),
           funding_amount = as.numeric(str_replace_all(amount_requested, "[^0-9.]", "")),
           principal_forgiveness_amount = as.numeric(str_replace_all(subsidy_amount_eligible_to_project, "[^0-9.]", "")),
    ) %>%
    # process text columns
    # use the start of the pwsid to separate name and pwsid, then reappend the state abbreviation
    mutate(borrower = as.character(map(strsplit(water_system_name_and_number, split = "NM"), 1)),
           borrower = str_squish(borrower),
           state_rank = str_replace_all(ranking, "[^0-9.]", ""),
           state_score = str_replace_all(score, "[^0-9.]", ""),
           pwsid = paste0("NM", as.character(map(strsplit(water_system_name_and_number, split =  "NM"), 2))),
           cities_served = str_to_title(county),
           project_description = str_to_sentence(str_squish(project_description)),
           disadvantaged = case_when(
             grepl("Disadvantaged", disadvantaged_status, ignore.case=TRUE) ~ "Yes",
             TRUE ~ "No"),
           funding_status = "Funded",
           project_type = "General",
           state = "New Mexico",
           category = "3"
    ) %>%
    select(state_rank, state_score, borrower, pwsid, project_description, funding_amount, principal_forgiveness_amount, 
           disadvantaged, population, cities_served, project_type, funding_status, state, category)
  
  rm(list=setdiff(ls(), "nm_clean"))
  
  return(nm_clean)
}