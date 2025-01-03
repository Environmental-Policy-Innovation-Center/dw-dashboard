source("resources.R")

clean_wi <- function() {
  
  wi_clean <- fread("year2/WI/data/49-Wisconsin_PPL-y2.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(!is.na(project_points)) %>%
    mutate(
      funding_amount = clean_numeric_string(as.numeric(str_replace_all(estimated_loan_amount,"[^0-9.]", "")) + 
                                              as.numeric(str_replace_all(total_estimated_pf,"[^0-9.]", ""))),
      principal_forgiveness = clean_numeric_string(total_estimated_pf),
      population = clean_numeric_string(population),
      requested_amount = clean_numeric_string(requested_project_costs),
      project_type = case_when(
        program == "LSL" | grepl("LSL", project_description, ignore.case=TRUE) ~ "Lead",
        program == "EC" | grepl("PFAS|Emerging Contaminants", project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      disadvantaged = case_when(
        program == "EC" & as.numeric(str_replace_all(financial_need_points,"[^0-9.]", "")) > 29 ~ "Yes",
        as.numeric(str_replace_all(total_pf_points,"[^0-9.]", "")) > 59 ~ "Yes",
        TRUE ~ "No"),
      expecting_funding = "Yes", 
      borrower = str_to_title(str_replace_all(municipality, "[0-9.\\#]", "")),
      project_score = str_replace_all(priority_score,"[^0-9.]",""),
      project_id = str_squish(project_number),
      community_served = borrower,
      state = "Wisconsin",
      state_fiscal_year = "2024",
      pwsid = as.character(NA),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      project_rank = as.character(NA)
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  print(paste("Final dataframe dimensions:", dim(wi_clean)[1], "rows,", dim(wi_clean)[2], "columns"))
  
  run_tests(wi_clean)
  return(wi_clean)
}