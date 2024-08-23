source("resources.R")

clean_ut <- function() {
  
  
  ut_ppl <- fread("year1/UT/data/44-Utah_Comprehensive.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # (58,22) -> (58,13)
  ut_clean <- ut_ppl %>%
    # drop columns
    select(-green_project, -green_amount, -equivalency_project,
           -project_segments_sour, -project_segments_treat, -project_segments_stor, -project_segments_dist) %>%
    # format numeric columns
    mutate(
      population = clean_numeric_string(pop),
      funding_amount = clean_numeric_string(funds_authorized),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      project_cost = clean_numeric_string(project_total),
    ) %>%
    # format non-numeric columns
    mutate(
      pwsid = str_squish(pwsid),
      project_score = str_squish(priority_points),
      borrower = str_squish(system_name),
      community_served = str_squish(county),
      project_description = str_squish(project_title),
      disadvantaged = case_when(disadvantaged == "Y" ~ "Yes",
                                TRUE ~ "No"),
      expecting_funding = case_when(
        funding_amount != "No Information" | principal_forgiveness != "No Information" ~ "Yes",
        TRUE ~ "No"),
      state = "Utah",
      state_fiscal_year = "2023",
      project_id = as.character(NA),
      project_name = as.character(NA),
      project_type = as.character(NA),
      requested_amount = as.character(NA),
      project_rank = as.character(NA),
      project_score = replace_na(project_score, "No Information")
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(ut_clean)
  rm(list=setdiff(ls(), "ut_clean"))
  
  return(ut_clean)
}