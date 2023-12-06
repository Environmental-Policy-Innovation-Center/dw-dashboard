library(tidyverse)
library(data.table)
library(janitor)

clean_ks <- function() {
  
  ## Base
  # -> (98,9)
  ks_base <- fread("year1/KS/data/16-Kansas_PPL-base.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-v3, -v5, -v9, -v10, -v12, -v14, -v15, -v17) %>%
    mutate(
      project_type = "General"
    )
  
  ## Lead
  # -> (16,8)
  ks_lead <- fread("year1/KS/data/16-Kansas_PPL-lead.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-v3, -v5, -v7, -v8, -v10, -v11, -v13, -v14, -v16) %>%
    mutate(
      project_type = "Lead"
    ) 
  
  # Emerging Contaminants
  # -> (9,7)
  ks_ec <- fread("year1/KS/data/16-Kansas_PPL-ec.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-v3, -v5, -v7, -v8, -v9, -v10, -v12, -v13, -v14, -v16, -v18) %>%
    mutate(
      project_type = "Emerging Contaminants"
    ) %>%
    rename(loan_request = x,
           population_served = served,
           loan_forgiveness = forgiveness)
  
  ## Combine
  # -> (123,12)
  ks_combined <- bind_rows(ks_base, ks_lead, ks_ec)
  
  
  ## Clean
  ks_clean <- ks_combined %>%
    # process numeric columns
    mutate(
      requested_amount = as.numeric(str_replace_all(loan_request, "[^0-9.]", "")),
      population = as.numeric(str_replace_all(population_served, "[^0-9.]", ""))
    ) %>%
    # process text columns
    mutate(
      borrower = str_squish(municipality_name),
      project_name = str_squish(project_number),
      state_score = str_squish(rating),
      project_description = str_squish(project_description),
      disadvantaged = case_when(
        requested_amount > 0 ~ "Yes",
        TRUE ~ "No"),
      state = "Kansas",
      category = "2",
      # assume all applicant until confirmed otherwise
      funding_status = "Not Funded"
    ) %>%
    select(borrower, state_score, project_name, project_description, requested_amount,
           population, project_type, state, category, funding_status)
  
  rm(list=setdiff(ls(), "ks_clean"))
  
  return(ks_clean)
}