library(tidyverse)
library(data.table)
library(janitor)

clean_la <- function() {
  
  # (40,9)
  la_ppl <- fread("year1/LA/data/18-Lousiana_Comprehensive_PPL-clean.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(funding_status = "Not Funded")
  
  # (8,11)
  la_fnd <- fread("year1/LA/data/18-Lousiana_Fundable_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(funding_status = "Funded")
  
  # (48,11)
  la_combined <- bind_rows(la_ppl, la_fnd)
  
  # -> (46,12)
  la_clean <- la_combined %>%
    # drop columns and total rows
    select(-est_date_to_close_loan) %>%
    filter(system_name !="Total") %>%
    # process numeric columns
    mutate(funding_amount = as.numeric(str_replace_all(est_loan_amount, "[^0-9.]", "")),
           population = as.numeric(str_replace_all(population, "[^0-9.]", "")),
           disadvantaged_subsidy = as.numeric(str_replace_all(disadvantaged_subsidy, "[^0-9.]", "")),
           # replace 0s specifically for adding columns together
           disadvantaged_subsidy = replace_na(disadvantaged_subsidy, 0),
           additional_subsidy_amount = as.numeric(str_replace_all(additonal_subsidy_amount, "[^0-9.]", "")),
           additional_subsidy_amount = replace_na(additional_subsidy_amount, 0),
           principal_forgiveness_amount = disadvantaged_subsidy + additional_subsidy_amount
    ) %>%
    # process text columns
    mutate(borrower = str_squish(system_name),
           pwsid = paste0("LA", pwsid),
           state_score = str_replace_all(points, "[^0-9.]", ""),
           state_rank = str_replace_all(rank, "[^0-9.]", ""),
           project_description = str_squish(project_description),
           disadvantaged = case_when(
             disadvantaged_subsidy > 0 ~ "Yes",
             TRUE ~ "No"),
           state = "Louisiana",
           category = "3",
           project_type = "General"
    ) %>%
    select(state_rank, state_score, borrower, pwsid, project_description, funding_amount, principal_forgiveness_amount, 
           population, disadvantaged, project_type, funding_status, state, category)

  
  rm(list=setdiff(ls(), "la_clean"))
  
  return(la_clean)
}