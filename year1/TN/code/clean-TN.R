source("resources.R")



clean_tn <- function() {
  
  tn_ppl <- fread("year1/TN/data/tn-fy22-ppl.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() 
  
  tn_clean <- tn_ppl %>%
    # process numeric columns
    mutate(project_cost = clean_numeric_string(total_project_amount),
           population = clean_numeric_string(pop_served)) %>%
    # process text columns 
    mutate(community_served = str_squish(county),
           borrower = str_squish(local_government),
           pwsid = str_squish(pwsid_number),
           project_type = "General",
           project_description = str_squish(project_description),
           disadvantaged = case_when(
             grepl("\\*", local_government) ~ "Yes",
             TRUE ~ "No"),
           # with dac define, remove extra characters referencing table footnotes
           borrower = str_replace_all(borrower, "\\*", ""),
           borrower = str_replace_all(borrower, "\\+", ""),
           project_rank = str_squish(rank_order),
           project_score = str_squish(priority_points),
           expecting_funding = "No",
           state = "Tennessee",
           state_fiscal_year = "2023",
           project_id = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_rank = replace_na(project_rank, "No Information"),
           project_score = replace_na(project_score, "No Information")
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(tn_clean)
  rm(list=setdiff(ls(), "tn_clean"))
  
  return(tn_clean)
}