library(tidyverse)
library(data.table)
library(janitor)
source("cleaning-functions.R")

clean_sc <- function() {
  
  
  # import EC projects, (3,12) -> (3,5)
  sc_ec <- fread("year1/SC/data/sc-fy22-iup-ec.csv",
                 colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           funding_status = "Funded",
           principal_forgiveness_amount = convert_to_numeric(estimated_principal_forgiveness_assistance3),
           funding_amount = replace_na(convert_to_numeric(estimated_srf_loan_amount),0) + principal_forgiveness_amount) %>%
    select(srf_project_number, project_type, funding_status, principal_forgiveness_amount, funding_amount)
  
  
  # import lead (3,12)
  sc_lead <- fread("year1/SC/data/sc-fy22-iup-lslr.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(project_type = "Lead",
           funding_status = "Funded",
           principal_forgiveness_amount = convert_to_numeric(estimated_princpal_forgiveness_3_assistance),
           estimated_srf_loan_amount = replace_na(convert_to_numeric(estimated_srf_loan_amount),0),
           funding_amount = estimated_srf_loan_amount + principal_forgiveness_amount) %>%
    select(srf_project_number, project_type, funding_status, principal_forgiveness_amount, funding_amount)
  
  
  # import base projects (9,11)
  sc_base <- fread("year1/SC/data/sc-fy22-iup-base.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(funding_status = "Funded",
           project_type = "General",
           principal_forgiveness_amount = replace_na(convert_to_numeric(estimated_principal_forgiveness_assistance2), 0),
           funding_amount = replace_na(convert_to_numeric(estimated_srf_loan_amount4), 0) + principal_forgiveness_amount
           ) %>%
    select(srf_project_number, project_type, funding_status, principal_forgiveness_amount, funding_amount)
  
  
  # import supplemental projects (17,11)
  sc_supp <- fread("year1/SC/data/sc-fy22-iup-gen-supp.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(funding_status = "Funded",
           project_type = "General",
           principal_forgiveness_amount = replace_na(convert_to_numeric(estimated_principal_forgiveness_assistance2), 0),
           funding_amount = replace_na(convert_to_numeric(estimated_srf_loan_amount), 0) + principal_forgiveness_amount) %>%
    select(srf_project_number, project_type, funding_status, principal_forgiveness_amount, funding_amount)
  
  
  # merge base, supp, and ec together for uniform processing other features
  sc_combined <- bind_rows(sc_base, sc_supp, sc_ec, sc_lead)
  
  
  # import comprehensive project list of funded and applicants
  sc_comp <- fread("year1/SC/data/sc-fy22-iup-comprehensive-project-list.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    left_join(sc_combined, by="srf_project_number") 
  
  
  
  sc_clean <- sc_comp %>%
    # process numeric columns
    mutate(
           population = convert_to_numeric(sponsors_service_population),
           project_cost = convert_to_numeric(estimated_total_project_cost),
           requested_amount = convert_to_numeric(requested_srf_assistance1),
           funding_amount = replace_na(funding_amount, 0),
           principal_forgiveness_amount = replace_na(principal_forgiveness_amount, 0)
    ) %>%
    # process text columns
    mutate(project_description = str_squish(project_description),
           # take the first portion of the water system id number and attach SC
           pwsid = paste0("SC", as.character(map(strsplit(srf_project_number, split = "-"), 1))),
           borrower = str_extract(sponsor_project_name, "^[^-]+"),
           project_name = str_squish(srf_project_number),
           # manually fix borrower / project_names that didn't have dashes or multiple dashses
           borrower = case_when(
            project_name == "2620004-30" ~ "Grand Strand Water and Sewer Authority Bull Creek",
            project_name == "2620004-28" ~ "Grand Strand Water and Sewer Authority Conway",
            project_name == "3220001-05" ~ "Gilbert-Summit Rural Water District",
            project_name == "3210002-04" ~ "Batesburg-Leesville, Town of",
            project_name == "0410011-04" ~ "Belton-Honea Path Water Authority",
            project_name == "0420005-03" ~ "Starr-Iva Water & Sewer District",
            project_name == "0420005-02" ~ "Starr-Iva Water & Sewer District",
            project_name == "0420005-01" ~ "Starr-Iva Water & Sewer District",
            project_name == "0120001-03" ~ "Donalds-Due West Water & Sewer Authority",
            project_name == "0820002-12" ~ "Berkeley County Water & Sewer",
             TRUE ~ borrower),
           state_score = str_replace_all(total_points,"[^0-9.]",""),
           state_rank = str_squish(v1),
           state = "South Carolina",
           category = "1",
           funding_status = case_when(
             is.na(funding_status) ~ "Not Funded",
             TRUE ~ funding_status
           ),
           project_type = case_when(
             is.na(project_type) & grepl("lead", project_description, ignore.case=TRUE) ~ "Lead",
             is.na(project_type) & grepl("lsl", project_description, ignore.case=TRUE) ~ "Lead",
             is.na(project_type) & grepl("pfas", project_description, ignore.case=TRUE) ~ "Emerging Contamninants",
             is.na(project_type) & grepl("pfoa", project_description, ignore.case=TRUE) ~ "Emerging Contamninants",
             is.na(project_type) & grepl("pfos", project_description, ignore.case=TRUE) ~ "Emerging Contamninants",
             is.na(project_type) ~ "General",
             TRUE ~ project_type)
    ) %>%
    select(borrower, pwsid, state_rank, state_score, project_name, project_description, 
           funding_amount, principal_forgiveness_amount, project_cost, requested_amount,
           population, project_type, state, category, funding_status)

  
  rm(list=setdiff(ls(), "sc_clean"))
  
  return(sc_clean)
}