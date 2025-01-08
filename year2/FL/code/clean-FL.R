library(tidyverse)
library(data.table)
library(janitor)
source("resources.R")


clean_fl_y2 <- function() {
  
  
  # (53, 16)
  base <- read.csv("year2/FL/data/tabula-Base_BIL IUP DWSRF SFY2023-24 Revised 01122024.csv") %>%
    clean_names() %>%
    mutate(requested_amount = "No Information", 
           disadvantaged = "No Information", 
           expecting_funding = "Yes", 
           project_type = "No Information", 
           public_water_system_identification_number = as.character(public_water_system_identification_number))
  
  
  # (5, 13)
  ec23 <- read.csv("year2/FL/data/tabula-FINAL EC IUP DWSRF 2022 11072023-FY23-24.csv") %>%
    clean_names() %>%
    mutate(public_water_system_identification_number = as.character(public_water_system_identification_number))
  
  # (7, 13)
  ec22 <- read.csv("year2/FL/data/tabula-FINAL EC IUP DWSRF 2022 11072023- FY22-23.csv") %>%
    clean_names()
  
  # (12, 13)
  ec <- bind_rows(ec22, ec23) %>%   
    separate(applicant_project_number, into = c("borrower", "project_id"), sep = "/") %>%
    mutate(project_type = "Emerging Contaminants", 
           requested_amount = clean_numeric_string(requested_loan_amount), 
           disadvantaged = case_when(
             grepl("1|2", borrower) ~ "Yes", 
             TRUE ~ "No"), 
           # entire table has authorized loan amounts > 0
           expecting_funding = "Yes", 
           applicant_project_number = paste0(borrower, "/", project_id)) 
  
  
  # (46, 12)
  lead22 <- read.csv("year2/FL/data/tabula-Final LSL IUP DWSRF FFY2022-2023 rev 7-12-2023.csv") %>%
    clean_names() %>%
    rename(applicant_proje_ct_number = applicant_pro_ject_number)
  
  # (56, 11)
  lead23 <- read.csv("year2/FL/data/tabula-LSL IUP DWSRF FFY2023 2-23-2024.csv") %>%
    clean_names()
  
  # (102, 12)
  lead <- bind_rows(lead22, lead23) %>%
    mutate(project_type = "Lead", 
           # TODO v confirm w/ janet
           # requested_amount = clean_numeric_string(requested_loan_amount),
           requested_amount = "No Information",
           public_water_system_identification_number = as.character(public_water_system_identification_number), 
           # TODO v confirm w/ janet
           disadvantaged = "No Information", 
           # TODO - gotta grab the waiting list 
           expecting_funding = "Yes") %>%
    rename(applicant_project_number = applicant_proje_ct_number)
  
  
  # -> ()
  fl_clean <- bind_rows(base, ec, lead) %>%
    mutate(community_served = as.character(NA), 
           # borrower & project ID extracted using separate() below 
           pwsid = paste0("FL", public_water_system_identification_number), 
           project_name = as.character(NA), 
           project_cost = as.character(NA), 
           funding_amount = clean_numeric_string(authorized_loan_amount), 
           principal_forgiveness = clean_numeric_string(principal_forgiveness_amount), 
           project_description = str_squish(project_description), 
           population = as.character(NA), 
           project_rank = as.character(NA),
           project_score = str_squish(priority_score), 
           state = "Florida",
           state_fiscal_year = "SFY24") %>%
    separate(applicant_project_number, into = c("borrower", "project_id"), sep = "/") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(fl_clean)
  rm(list=setdiff(ls(), "fl_clean"))
  
  return(fl_clean)
}