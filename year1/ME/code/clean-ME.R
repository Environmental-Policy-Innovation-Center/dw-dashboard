source("resources.R")

clean_me_y1 <- function() {
  

  
  # process three projects in separate list for emerging contaminants
  me_ec <- fread("year1/ME/data/19-Maine_PL_EC.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # rename columns that don't need to be modified
    rename(borrower = water_system,
           project_id = srf_project_number,
           project_score = total_priority_points) %>%
    # process numeric columns
    mutate(funding_amount = clean_numeric_string(amount_requested),
           principal_forgiveness = clean_numeric_string(principal_forgiveness)
    ) %>%
    # process text columns
    mutate(project_type = "Emerging Contaminants",
           expecting_funding = "Yes"
    ) %>%
    select(borrower, project_id, project_description, project_score, funding_amount, 
           principal_forgiveness, project_type, expecting_funding)
  
  
  # process backup list as Applicant projects that were not funded
  me_backup <- fread("year1/ME/data/19-Maine_PPL_BackupList.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "No")
  
  
  # process comprehensive list
  me_comp <- fread("year1/ME/data/19-Maine_PPL_Comprehensive.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes")
  
  me_comp <- me_comp %>%
    bind_rows(me_comp, me_backup) %>%
    # drop columns
    select(-pf_percent, -percent_annual_water_bill_of_mhi, -project_type) %>%
    # process numeric columns
    # subtract ARPA funding from funding amount after setting both to numeric
    mutate(funding_amount = convert_to_numeric(amount_requested, TRUE),
           arpa_mjrp = convert_to_numeric(arpa_mjrp, TRUE),
           funding_amount = funding_amount - arpa_mjrp,
           principal_forgiveness = convert_to_numeric(principal_forgiveness, TRUE),
           population = clean_numeric_string(population_served)
    ) %>%
    # process text columns
    mutate(borrower = str_squish(water_system),
           pwsid = str_squish(pwsid),
           # NOTE: project_description was manually extracted from borrower given inconsistent
           # splitting and spacing throughout the projects.
           project_description = str_squish(project_description),
           project_id = str_squish(srf_project_number),
           project_score = str_squish(total_priority_points),
           # disadvantaged = case_when PF > 10% funding amount
           disadvantaged = case_when(
             principal_forgiveness > funding_amount * .1 ~ "Yes",
             TRUE ~ "No"),
           # once DAC is defined, fix funding and PF columns for strings
           funding_amount = clean_numeric_string(funding_amount),
           funding_amount = case_when(
             funding_amount == "0" ~ "No Information",
             TRUE ~ funding_amount),
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           principal_forgiveness = case_when(
             principal_forgiveness == "0" ~ "No Information",
             TRUE ~ principal_forgiveness),
           project_type = "General",
    ) %>%
    select(borrower, pwsid, project_score, project_id,  project_description, funding_amount, 
           principal_forgiveness,  disadvantaged, project_type, population, expecting_funding)
    
  
  me_clean <- bind_rows(me_comp, me_ec) %>%
    mutate(state = "Maine",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA),
           project_rank = as.character(NA),
           pwsid = replace_na(pwsid, "No Information"),
           population = replace_na(population, "No Information"),
           project_description = replace_na(project_description, "No Information"),
           disadvantaged = replace_na(disadvantaged, "No Information")
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(me_clean)
  rm(list=setdiff(ls(), "me_clean"))
  
  return(me_clean)
}