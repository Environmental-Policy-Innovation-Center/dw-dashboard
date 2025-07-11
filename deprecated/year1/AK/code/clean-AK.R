clean_ak_y1 <- function() {
  
  ak_ppl <- fread("year1/AK/data/ak-ppl-q4-final-1.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  ak_clean <- ak_ppl %>%
    # process numeric columns
    mutate(
           requested_amount = clean_numeric_string(requested_loan_amount),
           funding_amount = case_when(
             within_funding_limits == "X" ~ requested_amount,
             TRUE ~ "No Information"),
           # beccause PF columns are either NA or a value but not both, can simply extract value when not NA
           principal_forgiveness = case_when(
             !is.na(estimated_principal_forgiveness_sfy22_and_previous_years) ~ 
               clean_numeric_string(estimated_principal_forgiveness_sfy22_and_previous_years),
             !is.na(estimated_principal_forgiveness_sfy23) ~ 
               clean_numeric_string(estimated_principal_forgiveness_sfy23),
             TRUE ~ "No Information")
           ) %>%
    # process text columns
    mutate(borrower = str_squish(applicant),
           # extract pwsid as AK + 7 digits
           pwsid = str_extract(public_water_system_id_number_community_population, "AK\\d{7}"),
           # split name and description by the hyphen between them
           project_name = as.character(map(strsplit(project_name_and_description, split = " - "), 1)),
           project_description = as.character(map(strsplit(project_name_and_description, split = " - "), 2)),
           # population is after 9 character pwsid, then remove non-numeric characters
           population = str_sub(public_water_system_id_number_community_population, start=10),
           population = clean_numeric_string(population),
           disadvantaged = case_when(
             disadvantaged_community == "X" ~ "Yes",
             TRUE ~ "No"),
           project_rank = str_squish(rank),
           project_score = str_squish(score),
           project_type = "General",
           expecting_funding = case_when(
             within_funding_limits == "X" ~ "Yes",
             TRUE ~ "No"),
           state = "Alaska",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_cost = as.character(NA),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(ak_clean)
  rm(list=setdiff(ls(), "ak_clean"))
  
  return(ak_clean)
}