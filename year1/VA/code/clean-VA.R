library(tidyverse)
library(data.table)
library(janitor)
source("cleaning-functions.R")



clean_va <- function() {
  
  ### Base, BIL Supplemental, Emerging Contaminants
  
  # (10,15)
  va_base <- fread("year1/VA/data/va-fy22-ppl-base.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "General") %>%
    select(-notes)
  
  # (9,14)
  va_bil <- fread("year1/VA/data/va-fy22-ppl-bil.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "General")
  
  # (3,14)
  va_ec <- fread("year1/VA/data/va-fy22-ppl-ec.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants")
  
  # (22,15) 
  va_base_bil <- bind_rows(va_base, va_bil, va_ec) %>%
    mutate(disadvantaged = "No Information")
  
  
  ### Lead
  
  # (22,14) -> (22,10)
  va_lsl_22 <- fread("year1/VA/data/va-fy22-lsl-may22.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(priority, project_number, owner_information, project_name, project_description,
           total_points, principal_forgiveness, project_cost, srf_amount_for_this_iup)
  
  # (39,16) -> (39, 8)
  va_lsl_23 <- fread("year1/VA/data/va-fy22-lsl-may23.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(priority, project_number, owner_information, project_name,
           total_score, principal_forgiveness, project_cost, srf_amount_for_this_iup) %>%
    rename(total_points = total_score,
           project_description = project_name)
  
  # (61,10)
  va_lsl <- bind_rows(va_lsl_22, va_lsl_23) %>%
    mutate(project_type = "Lead") %>%
    rename(point_total = total_points)
  
  
  ### Combine & Clean
  
  # (83,13)
  va_clean <- bind_rows(va_base_bil, va_lsl)  %>%
    mutate(project_cost = convert_to_numeric(project_cost),
           funding_amount = convert_to_numeric(srf_amount_for_this_iup),
           principal_forgiveness_amount = convert_to_numeric(principal_forgiveness)) %>%
    mutate(cities_served = str_squish(city_county),
           borrower = str_squish(owner_information),
           project_name = str_squish(project_number),
           project_description = str_squish(project_description),
           state_rank = str_squish(priority),
           state_score = str_squish(point_total),
           funding_status = "Funded",
           state = "Virginia",
           category = "1"
    ) %>%
    select(cities_served, borrower, project_name, project_type, project_cost,
           funding_amount, principal_forgiveness_amount, project_description,
           state_rank, state_score, funding_status, state, category)

  rm(list=setdiff(ls(), "va_clean"))
  
  return(va_clean)
}