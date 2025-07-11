clean_nm_y1 <- function() {
  
  # (6,13)
  nm_raw <- fread("year1/NM/data/31-NewMexico_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (6,13)
  nm_clean <- nm_raw %>%
    # drop columns
    select(-mandatory_cap_grant_subsidy, -interest_rate, -designation) %>%
    # process numeric columns
    mutate(population = clean_numeric_string(population),
           funding_amount = clean_numeric_string(amount_requested),
           principal_forgiveness = clean_numeric_string(subsidy_amount_eligible_to_project),
    ) %>%
    # process text columns
    # use the start of the pwsid to separate name and pwsid, then reappend the state abbreviation
    mutate(borrower = as.character(map(strsplit(water_system_name_and_number, split = "NM"), 1)),
           borrower = str_squish(borrower),
           project_rank = str_replace_all(ranking, "[^0-9.]", ""),
           project_score = str_replace_all(score, "[^0-9.]", ""),
           pwsid = paste0("NM", as.character(map(strsplit(water_system_name_and_number, split =  "NM"), 2))),
           community_served = str_to_title(county),
           project_description = str_to_sentence(str_squish(project_description)),
           disadvantaged = case_when(
             grepl("Disadvantaged", disadvantaged_status, ignore.case=TRUE) ~ "Yes",
             TRUE ~ "No"),
           expecting_funding = "Yes",
           project_type = "General",
           state = "New Mexico",
           state_fiscal_year = "2023",
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(nm_clean)
  rm(list=setdiff(ls(), "nm_clean"))
  
  return(nm_clean)
}