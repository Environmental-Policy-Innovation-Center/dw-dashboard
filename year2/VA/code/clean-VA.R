library(tidyverse)
library(data.table)
library(janitor)
source("cleaning-functions.R")


clean_va <- function() {
  
  # (21, 14) -> (21, 18)
  va_clean <- read.csv("./year2/VA/data/tabula-VA-FY2023-IUP_draft-attachment1.csv") %>%
    clean_names() %>%
    mutate(community_served = as.character(NA), 
           borrower = str_squish(owner_information), 
           pwsid = as.character(NA), 
           project_id = str_squish(project),
           project_name = str_squish(project_name), 
           project_type = case_when(
             program_type_code %in% c("BASE", "SUPP") ~ "General", 
             TRUE ~ "Emerging Contaminants"
           ), 
           project_cost = convert_to_numeric(project_cost), 
           requested_amount = as.numeric(NA), 
           funding_amount = convert_to_numeric(srf_amount_for_this_iup), 
           principal_forgiveness = convert_to_numeric(principal_forgiveness), 
           population = as.numeric(NA), 
           project_description = str_squish(project_description), 
           disadvantaged = as.character(NA), 
           project_rank = str_squish(priority), 
           project_score = str_squish(point_total), 
           expected_funding = "Yes", 
           state = "Virginia", 
           state_fiscal_year = "SFY24") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expected_funding, state, state_fiscal_year)
  
  
  run_tests(va_clean)
  rm(list=setdiff(ls(), "df_clean"))
  
  return(va_clean)
}