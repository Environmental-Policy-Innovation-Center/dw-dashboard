# Set the working directory
setwd("/Users/madhvimalhotra/Downloads/EPIC/Github/dw-dashboard")

# Load required libraries
library(tidyverse)
library(data.table)
library(janitor)

source("resources.R")

clean_il <- function() {
  base_path <- file.path("year3", "IL", "data")
  
  # Clean and convert numeric values
  clean_numeric <- function(x) {
    as.character(as.numeric(gsub("[^0-9.-]", "", x)))
  }
  
  # Read PPL Fundable data
  il_ppl_f <- fread(file.path(base_path, "y3-Illinois_PPL_Fundable.csv"),
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "General",
           disadvantaged_community_principal_forgiveness = str_replace(disadvantaged_community_principal_forgiveness, "N/E", "0"),
           requested_loan_amount = clean_numeric(requested_loan_amount),
           estimated_loan_amount = clean_numeric(estimated_loan_amount))  # Add this line
  
  # Read PPL Applicant data
  il_ppl_a <- fread(file.path(base_path, "y3-Illinois_PPL_Applicant.csv"),
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "No",
           project_type="General",
           requested_loan_amount = clean_numeric(requested_loan_amount),
           estimated_loan_amount = requested_loan_amount)  # Add this line
  
  # Combine PPL data
  il_ppl <- bind_rows(il_ppl_a, il_ppl_f) %>%
    mutate(disadvantaged = case_when(
      as.numeric(gsub("[^0-9.-]", "", disadvantaged_community_principal_forgiveness)) > 0 ~ "Yes",
      TRUE ~ "No"
    ))
  
  # Create list of communities and DAC status
  dacs <- il_ppl %>%
    select(loan_applicant, disadvantaged) %>%
    distinct()
  
  # Process Lead Projects
  il_lead <- fread(file.path(base_path, "y3-Illinois_Lead_Fundable.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "Lead")
  
  il_lead <- merge(il_lead, dacs, by = "loan_applicant", all.x = TRUE) %>%
    mutate(
      disadvantaged = coalesce(disadvantaged, "No Information"),
      reserved_principal_forgiveness_amount = clean_numeric(reserved_principal_forgiveness_amount),
      reserved_loan_amount = clean_numeric(reserved_loan_amount),
      funding_amount = as.character(as.numeric(reserved_principal_forgiveness_amount) + as.numeric(reserved_loan_amount)),
      principal_forgiveness = reserved_principal_forgiveness_amount,
      requested_loan_amount = reserved_loan_amount
    )
  
  # Process Emerging Contaminant data
  il_ec_f <- fread(file.path(base_path, "y3-Illinois_EC_Fundable.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "Emerging Contaminant",
           pwslp_funds_reserved = clean_numeric(pwslp_funds_reserved))  # Add this line
  
  il_ec <- merge(il_ec_f, dacs, by = "loan_applicant", all.x = TRUE) %>%
    mutate(
      disadvantaged = coalesce(disadvantaged, "No Information"),
      requested_loan_amount = clean_numeric(requested_loan_amount),
      principal_forgiveness_reserved = clean_numeric(principal_forgiveness_reserved),
      funding_amount = as.character(as.numeric(pwslp_funds_reserved) + as.numeric(principal_forgiveness_reserved)),  # Update this line
      principal_forgiveness = principal_forgiveness_reserved
    )
  
  # Combine all data
  il_merge <- bind_rows(il_ppl, il_lead, il_ec) %>%
    mutate(
      funding_amount = case_when(
        project_type == "General" ~ estimated_loan_amount,
        TRUE ~ funding_amount
      ),
      disadvantaged_community_principal_forgiveness = clean_numeric(disadvantaged_community_principal_forgiveness),
      service_population = clean_numeric(service_population),
      principal_forgiveness = coalesce(clean_numeric(principal_forgiveness), "0")
    )
  
  # Clean and process data
  il_clean <- il_merge %>%
    mutate(
      borrower = str_squish(loan_applicant),
      borrower = str_to_title(borrower, locale="en"),
      project_score = str_replace_all(loan_priority_score, "[^0-9.]", ""),
      project_description = str_squish(project_description),
      project_description = str_to_sentence(project_description),
      state = "Illinois",
      state_fiscal_year = "2025",
      community_served = as.character(NA),
      project_id = coalesce(l17_number, ""),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = requested_loan_amount,
      project_rank = as.character(NA),
      population = service_population,
      pwsid = coalesce(facility_no, ""),
      principal_forgiveness = coalesce(principal_forgiveness, "0")  # Ensure no NA values
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  # Replace NA values
  columns_to_replace_na <- c("community_served", "project_name", "project_cost", "project_rank", "project_score", "principal_forgiveness")
  il_clean <- il_clean %>%
    mutate(across(all_of(columns_to_replace_na), ~coalesce(., "")))
  
  # Verify final shape of il_clean
  cat("Final shape of il_clean:", dim(il_clean), "\n")
  
  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)
}








