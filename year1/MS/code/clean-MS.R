source("resources.R")

clean_ms <- function() {
  
  # (48,9)
  ms_raw <- fread("year1/MS/data/24-Mississippi_PPL.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # -> (42,8)
  ms_clean <- ms_raw %>%
    # drop category rows and funding line row
    filter(!grepl("Category", project) & project_description != "NA") %>%
    # format numeric columns
    mutate(
      population = clean_numeric_string(service_area_population),
      requested_amount = clean_numeric_string(loan_amount_requested),
      # use this to separate funding/applicant projects, but don't keep in standardized data
      state_cumulative = as.numeric(str_replace_all(statewide_cum, "[^0-9.]","")),
      # if above the threshold, funding amount is requested amount. otherwise 0
      funding_amount = ifelse(
        state_cumulative < 42500000, clean_numeric_string(loan_amount_requested), "No Information"),
      ) %>%
    # format text columns
    mutate(borrower = str_squish(project),
           project_description = str_squish(project_description),
           project_score = str_replace_all(priority_points,"[^0-9.]",""),
           disadvantaged = ifelse(
             as.numeric(str_replace_all(eligible_pf_amount,"[^0-9.]","")) > 0, "Yes", "No"),
           state = "Mississippi",
           state_fiscal_year = "2023",
           expecting_funding = case_when(
             # funding line set at 42,500,000 in PPL
             state_cumulative < 42500000 ~ "Yes",
             TRUE ~ "No"),
           community_served = as.character(NA),
           pwsid = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_type = as.character(NA),
           project_cost = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_rank = as.character(NA),
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(ms_clean)
  rm(list=setdiff(ls(), "ms_clean"))
  
  return(ms_clean)
}