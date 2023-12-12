library(tidyverse)
library(data.table)
library(janitor)

clean_ky <- function() {
  
  # (139,13)
  ky_iup <- fread("year1/KY/data/KY-iup-appendix2.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (139,14)
  ky_clean <- ky_iup %>%
    mutate(population = as.numeric(str_replace_all(system_population,"[^0-9.]","")),
           project_cost = as.numeric(str_replace_all(total_project_cost,"[^0-9.]","")),
           requested_amount = as.numeric(str_replace_all(requested_loan_amount,"[^0-9.]","")),
           loan_base = as.numeric(str_replace_all(invited_loan_amount_base,"[^0-9.]","")),
           loan_base = replace_na(loan_base, 0),
           loan_supplemental = as.numeric(str_replace_all(invited_loan_amount_supplemental,"[^0-9.]","")),
           loan_supplemental = replace_na(loan_supplemental, 0),
           loan_lead = as.numeric(str_replace_all(invited_loan_amount_lead_service_line,"[^0-9.]","")),
           loan_lead = replace_na(loan_lead, 0),
           funding_amount = loan_base + loan_supplemental + loan_lead,
           principal_forgiveness_amount = as.numeric(str_replace_all(principal_forgiveness_amount,"[^0-9.]","")),
           principal_forgiveness_amount = replace_na(principal_forgiveness_amount, 0)
           ) %>%
    mutate(borrower = str_squish(applicant),
           project_name = str_squish(wris_number),
           state_rank = str_squish(rank),
           state_score = str_squish(score),
           project_description = str_squish(project_title),
           project_type = case_when(
             grepl("lead", project_title, ignore.case=TRUE) ~ "Lead",
             grepl("lsl", project_title, ignore.case=TRUE) ~ "Lead",
             TRUE ~ "General"),
           funding_status = case_when(
             funding_amount > 0 ~ "Funded",
             TRUE ~ "Not Funded"
           ),
           category = "1",
           state = "Kentucky"
           ) %>%
    select(borrower, project_name, project_type, project_cost, requested_amount, funding_amount, 
           principal_forgiveness_amount, project_description, population, state_rank, state_score, 
           funding_status, category, state)

  rm(list=setdiff(ls(), "ky_clean"))
  
  return(ky_clean)
}