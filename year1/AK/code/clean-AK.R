library(tidyverse)
library(data.table)
library(janitor)

clean_ak <- function() {
  
  ak_ppl <- fread("year1/AK/data/ak-ppl-q4-final-1.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  ak_clean <- ak_ppl %>%
    # process numeric columns
    mutate(requested_amount = as.numeric(str_replace_all(requested_loan_amount,"[^0-9.]", "")),
           # population is after 9 character pwsid, then remove non-numeric characters
           population = str_sub(public_water_system_id_number_community_population, start=10),
           population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
           funding_amount = case_when(
             within_funding_limits == "X" ~ requested_amount,
             TRUE ~ 0),
           # beccause PF columns are either NA or a value but not both, can simply extract value when not NA
           principal_forgiveness_amount = case_when(
             !is.na(estimated_principal_forgiveness_sfy22_and_previous_years) ~ 
               as.numeric(str_replace_all(estimated_principal_forgiveness_sfy22_and_previous_years,"[^0-9.]", "")),
             !is.na(estimated_principal_forgiveness_sfy23) ~ 
               as.numeric(str_replace_all(estimated_principal_forgiveness_sfy23,"[^0-9.]", "")),
             TRUE ~ 0)
           ) %>%
    # process text columns
    mutate(borrower = str_squish(applicant),
           # extract pwsid as AK + 7 digits
           pwsid = str_extract(public_water_system_id_number_community_population, "AK\\d{7}"),
           # split name and description by the hyphen between them
           project_name = as.character(map(strsplit(project_name_and_description, split = " - "), 1)),
           project_description = as.character(map(strsplit(project_name_and_description, split = " - "), 2)),
           disadvantaged = case_when(
             disadvantaged_community == "X" ~ "Yes",
             TRUE ~ "No"),
           state_rank = str_squish(rank),
           state_score = str_squish(score),
           project_type = "General",
           funding_status = case_when(
             within_funding_limits == "X" ~ "Funded",
             TRUE ~ "Not Funded"),
           state = "Alaska",
           category = "3"
    ) %>%
    select(borrower, pwsid, project_name, project_type, requested_amount, funding_amount,
           principal_forgiveness_amount, project_description, population, disadvantaged,
           state_rank, state_score, funding_status, state, category)
  
  
  rm(list=setdiff(ls(), "ak_clean"))
  
  return(ak_clean)
}