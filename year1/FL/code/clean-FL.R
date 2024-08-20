source("resources.R")

clean_fl <- function() {

  fl_clean <- fread("year1/FL/data/9-Florida_ppl_fundable.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # format numeric columns
    mutate(funding_amount = clean_numeric_string(authorized_loan_amount),
           principal_forgiveness = clean_numeric_string(principal_forgiveness_amt),
    ) %>%
    # format text columns
    mutate(borrower = str_squish(applicant),
           project_id = str_squish(project_number),
           project_description = str_squish(project_description),
           project_score = str_replace_all(priority_score,"[^0-9.]",""),
           expecting_funding = "Yes",
           state = "Florida",
           state_fiscal_year = "2023",
           project_name = as.character(NA),
           community_served = as.character(NA),
           pwsid = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA),
           population = as.character(NA),
           disadvantaged = as.character(NA),
           project_rank = as.character(NA)
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(fl_clean)
  rm(list=setdiff(ls(), "fl_clean"))

  return(fl_clean)
}