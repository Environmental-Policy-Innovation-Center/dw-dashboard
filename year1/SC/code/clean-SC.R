library(tidyverse)
library(data.table)
library(janitor)

clean_sc <- function() {
  
  
  # import EC project
  sc_ec <- fread("year1/SC/data/37-SouthCarolina_EC_AppA.csv",
                 colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants") %>%
    rename(estimated_srf_loan_amount = estimated_srf_loan_amount4,
           estimated_principal_forgiveness_assistance = estimated_principal_forgiveness_assistance2)
  
  # import lead
  sc_lead <- fread("year1/SC/data/37-SouthCarolina_LSLR_AppA.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(project_type = "Lead") %>%
    rename(sponsors_service_population = sponsor_s_service_population)
  
  # import base projects
  sc_base <- fread("year1/SC/data/37-SouthCarolina_Base_AppA.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    rename(estimated_srf_loan_amount = estimated_srf_loan_amount4,
           estimated_principal_forgiveness_assistance = estimated_principal_forgiveness_assistance2)
  
  
  # -> (18,11)
  sc_supp <- fread("year1/SC/data/37-SouthCarolina_Supplemental.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    rename(estimated_principal_forgiveness_assistance = estimated_principal_forgiveness_assistance2)
  
  # for non-EC projects, separate by Lead/General based on finding terms
  sc_combined <- bind_rows(sc_base, sc_supp) %>%
    mutate(project_type = case_when(
      grepl("lead", project_description, ignore.case=TRUE) ~ "Lead",
      grepl("lsl", project_description, ignore.case=TRUE) ~ "Lead",
      TRUE ~ "General")
    )
  
  # merge base, supp, and ec together for uniform processing other features
  sc_combined <- bind_rows(sc_combined, sc_ec, sc_lead)
  
  
  sc_clean <- sc_combined %>%
    # process numeric columns
    mutate(funding_amount = as.numeric(str_replace_all(estimated_srf_loan_amount,"[^0-9.]","")),
           funding_amount = replace_na(funding_amount, 0),
           principal_forgiveness_amount = as.numeric(str_replace_all(estimated_principal_forgiveness_assistance,"[^0-9.]","")),
           principal_forgiveness_amount = replace_na(principal_forgiveness_amount, 0),
           # funding amount should include PF
           funding_amount = funding_amount + principal_forgiveness_amount,
           population = as.numeric(str_replace_all(population_affected_by_project,"[^0-9.]","")),
           project_cost = as.numeric(str_replace_all(estimated_total_project_cost,"[^0-9.]",""))
    ) %>%
    # process text columns
    mutate(project_description = str_squish(project_description),
           # take the first portion of the water system id number and attach SC
           pwsid = paste0("SC", as.character(map(strsplit(srf_project_number, split = "-"), 1))),
           borrower = str_squish(as.character(map(strsplit(sponsor_and_project_name, split = " - "), 1))),
           project_name = str_squish(as.character(map(strsplit(sponsor_and_project_name, split = " - "), 2))),
           state_score = str_replace_all(total_points,"[^0-9.]",""),
           state_rank = str_squish(v1),
           state = "South Carolina",
           category = "1",
           funding_status = "Funded"
    ) %>%
    select(borrower, pwsid, state_rank, state_score, project_name, project_description, 
           funding_amount, principal_forgiveness_amount, project_cost,
           population, project_type, state, category, funding_status)
  
  ## Manually fix projects where splitting by dash did not work
  sc_clean <- sc_clean %>%
    mutate(project_name = case_when(
      borrower == "McCormick Commission of Public Works - Isolation Valve Installation" ~ "Isolation Valve Installation",
      borrower == "Marlboro County / Marlboro Water Company- New Production Well and Treatment Facility" ~ "New Production Well and Treatment Facility - Phase 1",
      borrower == "Grand Strand Water and Sewer Authority - Conway Parallel Transmission Main3" ~ "Conway Parallel Transmission Main",
      borrower == "Gilbert-Summit Rural Water District - Siesta Cove Water Main Extension" ~ "Siesta Cove Water Main Extension",
      borrower == "Charleston Water System - Charleston Water System Lead Service Line Replacement" ~ "Charleston Water System Lead Service Line Replacement",
      TRUE ~ project_name),
      
      borrower = case_when(
        borrower == "McCormick Commission of Public Works - Isolation Valve Installation" ~ "McCormick Commission of Public Works",
        borrower == "Marlboro County / Marlboro Water Company- New Production Well and Treatment Facility" ~ "Marlboro County / Marlboro Water Company",
        borrower == "Grand Strand Water and Sewer Authority - Conway Parallel Transmission Main3" ~ "Grand Strand Water and Sewer Authority",
        borrower == "Gilbert-Summit Rural Water District - Siesta Cove Water Main Extension" ~ "Gilbert-Summit Rural Water District",
        borrower == "Charleston Water System - Charleston Water System Lead Service Line Replacement" ~ "Charleston Water System",
        TRUE ~ borrower)
    )
  
  rm(list=setdiff(ls(), "sc_clean"))
  
  return(sc_clean)
}