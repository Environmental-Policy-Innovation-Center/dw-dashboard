library(tidyverse)
library(data.table)
library(janitor)

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
    mutate(funding_amount = as.numeric(str_replace_all(assistance_amount,"[^0-9.]","")),
           population = as.numeric(str_replace_all(pop_served,"[^0-9.]",""))
    ) %>%
    # process numeric columns with NAs
    rowwise() %>%
    mutate(principal_forgiveness_amount = sum(as.numeric(str_replace_all(principal_forgiveness,"[^0-9.]", "")),
                                              na.rm=TRUE),
    ) %>%
    ungroup() %>%
    # process text columns
    mutate(project_description = str_squish(project_description),
           borrower = str_squish(community_public_water_system),
           state_score = str_replace_all(priority_points,"[^0-9.]",""),
           disadvantaged = case_when(
             grepl("Yes", disadvantaged) ~ "Yes",
             TRUE ~ "No"),
           project_type = case_when(
             grepl("4", fund_project_eligibility) ~ "Lead",
             grepl("5", fund_project_eligibility) ~ "Emerging Contaminants",
             TRUE ~ "General"
           ),
           funding_status = case_when(
             funding_amount > 0 ~ "Funded",
             TRUE ~ "Not Funded"
           ),
           state = "South Dakota",
           category = "1"
    ) %>% 
    # keep relevant columns
    select(borrower, project_description, project_type, state_score, funding_amount, 
           principal_forgiveness_amount, population, disadvantaged, funding_status, state, category) 
  
  rm(list=setdiff(ls(), "sd_clean"))
  
  return(sd_clean)
}