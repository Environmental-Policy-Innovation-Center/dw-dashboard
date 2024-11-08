source("resources.R")

clean_sd <- function() {
  
  
  # (23,8)
  sd_fnd <- fread("year1/SD/data/41-SouthDakota_Fundable.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # (136,8)
  sd_ppl <- fread("year1/SD/data/41-SouthDakota_PPL2023.csv",
                  colClasses = "character", na.strings = "") %>% clean_names()
  
  # (136,14)
  # merge fundable and applicant projects together as they are listed twice,
  # using priority points and project number as unique ID
  sd_merged <- merge(sd_ppl, sd_fnd, by=c("priority_points", "project_number"), all=TRUE)
  
  # -> (136,10)
  sd_clean <- sd_merged %>%
    # drop columns
    select(-expected_loan_rate_term, -loan_recipient, -funding_date, -expected_funding_source) %>%
    # process numeric columns
    mutate(funding_amount = clean_numeric_string(assistance_amount),
           population = clean_numeric_string(pop_served),
           principal_forgiveness = clean_numeric_string(principal_forgiveness)
           ) %>%
    # process text columns
    mutate(project_description = str_squish(project_description),
           borrower = str_squish(community_public_water_system),
           project_score = str_replace_all(priority_points,"[^0-9.]",""),
           disadvantaged = case_when(
             grepl("Yes", disadvantaged) ~ "Yes",
             TRUE ~ "No"),
           project_id = str_squish(project_number),
           project_type = case_when(
             grepl("4", fund_project_eligibility) ~ "Lead",
             grepl("5", fund_project_eligibility) ~ "Emerging Contaminants",
             TRUE ~ "General"
           ),
           expecting_funding = case_when(
             funding_amount != "No Information" ~ "Yes",
             TRUE ~ "No"
           ),
           state = "South Dakota",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           pwsid = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA),
           project_rank = as.character(NA)
    ) %>% 
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(sd_clean)
  rm(list=setdiff(ls(), "sd_clean"))
  
  return(sd_clean)
}