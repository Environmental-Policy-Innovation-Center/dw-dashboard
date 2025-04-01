clean_ia_y1 <- function() {
  
  ia_ppl <- read.csv("year1/IA/data/iowa-ppl-q4.csv") %>%
    clean_names()
  
  ia_clean <- ia_ppl %>%
    filter(project_status != "") %>%
    mutate(population = clean_numeric_string(population),
           requested_amount = clean_numeric_string(current_funding_request),
           funding_amount = clean_numeric_string(loan_amount)) %>%
    mutate(borrower = str_squish(project_name),
           project_id = str_squish(dwsrf_no),
           project_type = case_when(
             grepl("LSL", lf_eligible) ~ "Lead",
             grepl("EC", lf_eligible) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           project_description = str_squish(project_description),
           project_score = str_squish(priority_points),
           expecting_funding = case_when(
             project_status == "L" | project_status == "R" ~ "Yes",
             TRUE ~ "No"),
           state = "Iowa",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           pwsid = as.character(NA),
           project_name = as.character(NA),
           principal_forgiveness = as.character(NA),
           disadvantaged = as.character(NA),
           project_rank = as.character(NA),
           project_cost = as.character(NA)
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(ia_clean)
  rm(list=setdiff(ls(), "ia_clean"))
  
  return(ia_clean)
}