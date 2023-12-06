library(tidyverse)
library(data.table)
library(janitor)

clean_wa <- function() {
  
  # import manually merged file (31,12)
  wa_merged <- fread("year1/WA/data/46-Washington_PPL_Merged.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (29,12)
  wa_clean <- wa_merged %>%
    # drop total rows and extra columns
    filter(!grepl("total", health_application, ignore.case=TRUE)) %>%
    select(-health_application, -comments) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
           funding_amount = as.numeric(str_replace_all(loan_request_amount, "[^0-9.]", "")),
           principal_forgiveness_amount = as.numeric(str_replace_all(subsidy_award, "[^0-9.]", "")),
    ) %>%
    # process text columns
    # append state abbreviation and leading numbers to water system id
    mutate(pwsid = paste0("WA53", water_system_id),
           borrower = str_squish(applicant_name),
           state_score = str_replace_all(final_scor_e,"[^0-9.]", ""),
           project_name = str_squish(project),
           cities_served = str_squish(county),
           project_description = str_squish(project_description),
           funding_status = "Funded",
           project_type = case_when(
             grepl("lead", project_description, ignore.case=TRUE) ~ "Lead",
             grepl("PFAS", project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           disadvantaged = case_when(
             principal_forgiveness_amount > 0 ~ "Yes",
             principal_forgiveness_amount == 0 ~ "No"),
           state = "Washington",
           category = ""
    ) %>%
    select(state_score, borrower, pwsid, project_name, project_description, cities_served, population, 
           funding_amount, principal_forgiveness_amount, funding_status, disadvantaged, project_type, state, category)
  
  rm(list=setdiff(ls(), "wa_clean"))
  
  return(wa_clean)
}