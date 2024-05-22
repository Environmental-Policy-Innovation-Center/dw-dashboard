library(tidyverse)
library(data.table)
library(janitor)
source("cleaning-functions.R")

clean_wa <- function() {
  
  # import manually merged file (31,12) that combines appendix B and D
  wa_merged <- fread("year1/WA/data/46-Washington_PPL_Merged.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(score = final_scor_e)
  
  wa_app_c <- fread("year1/WA/data/wa-appendix-c.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()  %>%
    mutate(applicant_name = str_replace_all(water_system_name, "\r", " ")) %>%
    rename(loan_request_amount = request)
  
  wa_merged <- bind_rows(wa_merged, wa_app_c)
  
  # -> (41,15)
  wa_clean <- wa_merged %>%
    # drop total rows and extra columns
    filter(!grepl("total", health_application, ignore.case=TRUE)) %>%
    select(-health_application, -comments) %>%
    # process numeric columns
    mutate(population = convert_to_numeric(population),
           requested_amount = convert_to_numeric(loan_request_amount),
           funding_amount = convert_to_numeric(loan_request_amount),
           principal_forgiveness_amount = convert_to_numeric(subsidy_award)
    ) %>%
    # process text columns
    # append state abbreviation and leading numbers to water system id
    mutate(pwsid = paste0("WA53", water_system_id),
           borrower = str_squish(applicant_name),
           state_score = str_replace_all(score,"[^0-9.]", ""),
           project_name = str_squish(project),
           cities_served = str_squish(county),
           project_description = str_squish(project_description),
           funding_status = case_when(
             grepl("This project was withdrawn by applicant because they were not ready to proceed", project_description) ~ "Not Funded",
             TRUE ~ "Funded"
           ),
           funding_amount = ifelse(funding_status == "Funded", funding_amount, 0), 
           project_type = "General",
           disadvantaged = case_when(
             principal_forgiveness_amount > 0 ~ "Yes",
             principal_forgiveness_amount == 0 ~ "No",
             is.na(principal_forgiveness_amount) ~ "No Information"),
           # with disadvantaged filled in, go back and fill in PF for NA projects
           principal_forgiveness_amount = replace_na(principal_forgiveness_amount, 0),
           state = "Washington",
           category = "3"
    ) %>%
    select(cities_served, borrower, pwsid, project_name, project_type, requested_amount, funding_amount,
           principal_forgiveness_amount, population, project_description, disadvantaged, state_score, funding_status,
           state, category)
  
  rm(list=setdiff(ls(), "wa_clean"))
  
  return(NULL)
}