clean_la_y2 <- function() {
  
  ## Base Docs
  base_app <- fread("year2/LA/data/base-applicant.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-est_date_to_close_loan)
  
  base_fund <- fread("year2/LA/data/base-fundable.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(system_name, est_loan_amount, expecting_funding)
  
  base_all <- base_app %>%
    left_join(base_fund, by=c("system_name", "est_loan_amount"))
  
  ## BIL Docs
  
  ### Gen Supp
  
  gs_app <- fread("year2/LA/data/gs-applicant.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-est_date_to_close_loan)
  
  gs_fund <- fread("year2/LA/data/gs-fundable.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(system_name, est_loan_amount, expecting_funding)
  
  gs_all <- gs_app %>%
    left_join(gs_fund, by=c("system_name", "est_loan_amount"))
  
  ### EC 
  
  ec_app <- fread("year2/LA/data/ec-applicant.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-est_date_to_close_loan)
  
  ec_fund <- fread("year2/LA/data/ec-fundable.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(system_name, est_loan_amount, expecting_funding)
  
  ec_all <- ec_app %>%
    left_join(ec_fund, by=c("system_name", "est_loan_amount")) %>%
    mutate(project_type = "Emerging Contaminants")
  
  ### Lead
  
  lead_app <- fread("year2/LA/data/lead-applicant.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-est_date_to_close_loan) %>%
    # if there is a future amendment, check projects on this list against funding lists to determine if this should change
    mutate(expecting_funding = "No Information",
           project_type = "Lead")
  
  # combine bil
  bil_all <- bind_rows(gs_all, ec_all, lead_app)
  
  # almost all projects on bil list overlap with base except one, unlike 2025
  all_match <- merge(base_all, bil_all, all=TRUE, by=c("system_name", "est_loan_amount", "pwsid")) %>%
    mutate(points = ifelse(is.na(points.x), points.y, points.x),
           rank = ifelse(is.na(rank.x), rank.y, rank.x),
           population = ifelse(is.na(population.x), population.y, population.x),
           project_description = ifelse(is.na(project_description.x), project_description.y, project_description.x),
           expecting_funding = ifelse(is.na(expecting_funding.x), expecting_funding.y, expecting_funding.x)) %>%
    select(system_name, est_loan_amount, pwsid, points, rank, population, project_description, expecting_funding, project_type)
  
  la_clean <- all_match %>%
    mutate(community_served = as.character(NA),
           borrower = str_squish(system_name),
           pwsid = paste0("LA", pwsid),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_type = case_when(
             !is.na(project_type) ~ project_type,
             grepl(lead_str, project_description) ~ "Lead",
             grepl(ec_str, project_description) ~ "Emerging Contaminants",
             # all else is general
             TRUE ~ "General"),
           project_cost = as.character(NA),
           requested_amount = clean_numeric_string(est_loan_amount),
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           population = clean_numeric_string(population),
           disadvantaged = as.character(NA),
           project_rank = clean_numeric_string(rank),
           project_score = clean_numeric_string(points),
           expecting_funding = replace_na(expecting_funding, "No"),
           state = "Louisiana",
           state_fiscal_year = "2024"
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
    
  
  run_tests(la_clean)
  rm(list=setdiff(ls(), "la_clean"))
  
  return(la_clean)
}