source("resources.R")

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
      requested_amount = clean_numeric_string(loan_request),
      population = clean_numeric_string(population_served)
    ) %>%
    # process text columns
    mutate(
      borrower = str_squish(municipality_name),
      project_id = str_squish(project_number),
      project_score = str_squish(rating),
      project_description = str_squish(project_description),
      disadvantaged = case_when(
        requested_amount > 0 ~ "Yes",
        TRUE ~ "No"),
      state = "Kansas",
      state_fiscal_year = "2023",
      # assume all applicant until confirmed otherwise
      expecting_funding = "No",
      community_served = as.character(NA),
      pwsid = as.character(NA),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      project_rank = as.character(NA),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(ks_clean)
  rm(list=setdiff(ls(), "ks_clean"))
  
  return(ks_clean)
}