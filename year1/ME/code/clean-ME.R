library(tidyverse)
library(data.table)
library(janitor)

clean_me <- function() {
  

  
  # process three projects in separate list for emerging contaminants
  me_ec <- fread("year1/ME/data/19-Maine_PL_EC.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # rename columns that don't need to be modified
    rename(borrower = water_system,
           project_name = srf_project_number,
           state_score = total_priority_points) %>%
    # process numeric columns
    mutate(funding_amount = as.numeric(str_replace_all(amount_requested, "[^0-9.]", "")),
           principal_forgiveness_amount = as.numeric(str_replace_all(principal_forgiveness, "[^0-9.]", ""))
    ) %>%
    # process text columns
    mutate(project_type = "Emerging Contaminants",
           funding_status = "Funded"
    ) %>%
    select(borrower, project_name, project_description, state_score, funding_amount, 
           principal_forgiveness_amount, project_type, funding_status)
  
  # process backup list as Applicant projects that were not funded
  me_backup <- fread("year1/ME/data/19-Maine_PPL_BackupList.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(funding_status = "Not Funded")
  
  
  # process comprehensive list
  me_comp <- fread("year1/ME/data/19-Maine_PPL_Comprehensive.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(funding_status = "Funded")
  
  me_comp <- me_comp %>%
    bind_rows(me_comp, me_backup) %>%
    # drop columns
    select(-pf_percent, -percent_annual_water_bill_of_mhi, -project_type) %>%
    # process numeric columns
    # subtract ARPA funding from funding amount after setting both to numeric
    mutate(funding_amount = as.numeric(str_replace_all(amount_requested, "[^0-9.]", "")),
           arpa_mjrp = replace_na(arpa_mjrp, "0"),
           arpa_mjrp = as.numeric(str_replace_all(arpa_mjrp, "[^0-9.]", "")),
           funding_amount = funding_amount - arpa_mjrp,
           principal_forgiveness_amount = as.numeric(str_replace_all(principal_forgiveness, "[^0-9.]", "")),
           population = as.numeric(str_replace_all(population_served, "[^0-9.]", ""))
    ) %>%
    # process text columns
    mutate(borrower = str_squish(water_system),
           pwsid = str_squish(pwsid),
           # NOTE: project_description was manually extracted from borrower given inconsistent
           # splitting and spacing throughout the projects.
           project_description = str_squish(project_description),
           project_name = str_squish(srf_project_number),
           state_score = str_squish(total_priority_points),
           # disadvantaged = case_when PF > 10% funding amount
           disadvantaged = case_when(
             principal_forgiveness_amount > funding_amount * .1 ~ "Yes",
             TRUE ~ "No"),
           project_type = "General",
    ) %>%
    select(borrower, pwsid, state_score, project_name,  project_description, funding_amount, 
           principal_forgiveness_amount,  disadvantaged, project_type, population, funding_status)
    
  
  ##TODO: row bind EC and clean
  me_clean <- bind_rows(me_comp, me_ec) %>%
    mutate(state = "Maine",
           category = "3")
  
  rm(list=setdiff(ls(), "me_clean"))
  
  return(me_clean)
}