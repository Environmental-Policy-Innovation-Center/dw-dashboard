source("resources.R")

clean_la <- function() {
  
  # (40,9)
  la_ppl <- fread("year1/LA/data/18-Lousiana_Comprehensive_PPL-clean.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "No")
  
  # (8,11)
  la_fnd <- fread("year1/LA/data/18-Lousiana_Fundable_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes")
  
  # (48,11)
  la_combined <- bind_rows(la_ppl, la_fnd)
  
  # -> (46,12)
  la_clean <- la_combined %>%
    # drop columns and total rows
    select(-est_date_to_close_loan) %>%
    filter(system_name !="Total") %>%
    # process numeric columns
    mutate(funding_amount = clean_numeric_string(est_loan_amount),
           population = clean_numeric_string(population),
           # replace 0s specifically for adding columns together
           disadvantaged_subsidy = convert_to_numeric(disadvantaged_subsidy, TRUE),
           additional_subsidy_amount = convert_to_numeric(additonal_subsidy_amount, TRUE),
           principal_forgiveness = disadvantaged_subsidy + additional_subsidy_amount,
           principal_forgiveness = case_when(
             principal_forgiveness == 0 ~ "No Information",
             TRUE ~ clean_numeric_string(principal_forgiveness)
           )
    ) %>%
    # process text columns
    mutate(borrower = str_squish(system_name),
           pwsid = paste0("LA", pwsid),
           project_score = str_replace_all(points, "[^0-9.]", ""),
           project_rank = str_replace_all(rank, "[^0-9.]", ""),
           project_description = str_squish(project_description),
           disadvantaged = case_when(
             disadvantaged_subsidy > 0 ~ "Yes",
             TRUE ~ "No"),
           state = "Louisiana",
           state_fiscal_year = "2023",
           project_type = "General",
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA)
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  run_tests(la_clean)
  rm(list=setdiff(ls(), "la_clean"))
  
  return(la_clean)
}