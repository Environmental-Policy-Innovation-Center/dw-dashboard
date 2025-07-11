clean_wa_y1 <- function() {
  
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
    select(-comments) %>%
    # process numeric columns
    mutate(population = clean_numeric_string(population),
           requested_amount = clean_numeric_string(loan_request_amount),
           funding_amount = clean_numeric_string(loan_request_amount),
           principal_forgiveness = clean_numeric_string(subsidy_award)
    ) %>%
    # process text columns
    # append state abbreviation and leading numbers to water system id
    mutate(pwsid = paste0("WA53", water_system_id),
           borrower = str_squish(applicant_name),
           project_score = str_replace_all(score,"[^0-9.]", ""),
           project_name = str_squish(project),
           project_id = str_squish(health_application),
           community_served = str_squish(county),
           project_description = str_squish(project_description),
           expecting_funding = case_when(
             grepl("This project was withdrawn by applicant because they were not ready to proceed", project_description) ~ "No",
             TRUE ~ "Yes"
           ),
           funding_amount = ifelse(expecting_funding == "Yes", funding_amount, "No Information"), 
           project_type = "General",
           disadvantaged = case_when(
             subsidy_award == "0" ~ "No",
             subsidy_award == "NA" ~ "No Information",
             is.na(subsidy_award) ~ "No Information",
             TRUE ~ "Yes"),
           state = "Washington",
           state_fiscal_year = "2023",
           project_cost = as.character(NA),
           project_rank = as.character(NA)
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(wa_clean)
  rm(list=setdiff(ls(), "wa_clean"))
  
  return(wa_clean)
}