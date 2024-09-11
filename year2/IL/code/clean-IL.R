# Set the working directory
setwd("/Users/madhvimalhotra/Downloads/EPIC/Github/dw-dashboard")

# Load required libraries
library(tidyverse)
library(data.table)
library(janitor)

source("resources.R")

clean_il <- function() {
  
  base_path <- file.path("year2", "IL", "data")
  
  
  # Read PPL Fundable data
  il_ppl_f <- fread(file.path(base_path, "13-Illinois_PPL_Fundable.csv"),
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "General",
           # replace "N/E" with 0 for principal forgiveness since N/E is different from the NAs in other docs
           disadvantaged_community_principal_forgiveness = str_replace(disadvantaged_community_principal_forgiveness, "N/E", "0")
    )
  
  # Verify shape of il_ppl_f
  cat("Shape of il_ppl_f:", dim(il_ppl_f), "\n")
  
  # Read PPL Applicant data
  il_ppl_a <- fread(file.path(base_path, "13-Illinois_PPL_Applicant.csv"),
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "No",
           project_type="General")
  
  # Verify shape of il_ppl_a
  cat("Shape of il_ppl_a:", dim(il_ppl_a), "\n")
  
  
  # Combine PPL data
  il_ppl <- bind_rows(il_ppl_a, il_ppl_f) %>%
    mutate(disadvantaged = case_when(
      clean_numeric_string(disadvantaged_community_principal_forgiveness) > 0 ~ "Yes",
      TRUE ~ "No"))
  
  # Verify shape of combined il_ppl
  cat("Shape of combined il_ppl:", dim(il_ppl), "\n")
  
  # Create list of communities and DAC status
  dacs <- il_ppl %>%
    select(loan_applicant, disadvantaged) %>%
    distinct()
  
  # Verify shape of dacs
  cat("Shape of dacs:", dim(dacs), "\n")
  
  
  # Preprocess Lead Projects
  
  # Read Lead Fundable data
  il_lead <- fread(file.path(base_path, "13-Illinois_Lead_Fundable.csv"),
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "Lead")
  
  # Verify shape of il_lead
  cat("Shape of il_lead:", dim(il_lead), "\n")

  
  # merge lead projects with dac list
  il_lead <- merge(il_lead, dacs, all.x=TRUE, by="loan_applicant") %>%
    mutate(disadvantaged = replace_na(disadvantaged, "No Information") )
  
  # Verify shape of merged il_lead
  cat("Shape of merged il_lead:", dim(il_lead), "\n")
  
  # Read Emerging Contaminant data
  il_ec_f <- fread(file.path(base_path, "13-Illinois_EC_Fundable.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "Emerging Contaminant") %>%
    rename(disadvantaged_community_principal_forgiveness = principal_forgiveness_reserved)
  
  # Verify shape of il_ec_f
  cat("Shape of il_ec_f:", dim(il_ec_f), "\n")
  
  # Merge EC data with DAC list
  il_ec <- merge(il_ec_f, dacs, all.x=TRUE, by="loan_applicant") %>%
    mutate(disadvantaged = replace_na(disadvantaged, "No Information"))
  
  # Verify shape of merged il_ec
  cat("Shape of merged il_ec:", dim(il_ec), "\n")
  
  # Combine all data
  il_merge <- bind_rows(il_ppl, il_lead, il_ec)

  
  # Verify shape of il_merge
  cat("Shape of il_merge:", dim(il_merge), "\n")
  
  # Clean and process data
  il_clean <- il_merge %>%
    # drop columns
    select(-l17_number) %>%
    # process numeric columns
    mutate(funding_amount = clean_numeric_string(requested_loan_amount),
           principal_forgiveness = clean_numeric_string(disadvantaged_community_principal_forgiveness),
           population = clean_numeric_string(service_population),
    ) %>%
    # process text columns
    mutate(borrower = str_squish(loan_applicant),
           borrower = str_to_title(borrower, locale="en"),
           project_score = str_replace_all(loan_priority_score, "[^0-9.]", ""),
           project_description = str_squish(project_description),
           project_description = str_to_sentence(project_description),
           state = "Illinois",
           state_fiscal_year = "2024",
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA),
           project_rank = as.character(NA),
    ) %>%
    # rename columns
    rename(pwsid = facility_no) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  # Verify final shape of il_clean
  cat("Final shape of il_clean:", dim(il_clean), "\n")
  
  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)
}
