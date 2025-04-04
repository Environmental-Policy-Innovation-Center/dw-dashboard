clean_wi_y0 <- function() {
  
  wi_ppl <- read.csv("year0/WI/data/sfy2022-ppl.csv") %>%
    clean_names() %>%
    mutate(community_served = str_to_title(municipality),
           borrower = community_served,
           project_id = str_squish(project_number),
           project_id = str_replace(project_id, "‐", "-"),
           project_cost = clean_numeric_string(estimated_project_cost),
           project_description = str_squish(project_description),
           population = clean_numeric_string(population),
           project_score = str_squish(self_score)) %>%
    select(community_served, borrower, project_id, project_cost,
           project_description, population, project_score)
  
  
  wi_fund <- read.csv("year0/WI/data/SFY2022-Funding-List.csv") %>%
    clean_names() %>%
    mutate(project_id = str_squish(project_number),
           project_id = str_replace(project_id, "‐", "-"),
           requested_amount = clean_numeric_string(requested_project_costs),
           estimated_loan_amount = convert_to_numeric(estimated_loan_amount, fill_na=TRUE),
           principal_forgiveness = convert_to_numeric(pf_estimate, fill_na=TRUE),
           funding_amount = estimated_loan_amount + principal_forgiveness,
           funding_amount = clean_numeric_string(funding_amount),
           principal_forgiveness = clean_numeric_string(pf_estimate),
           disadvantaged = ifelse(eligible_pf != "0%", "Yes", "No"),
           expecting_funding = "Yes") %>%
    select(project_id, requested_amount, funding_amount, principal_forgiveness,
           disadvantaged, expecting_funding)
  
  
  # separate the funding projects that don't appear on the PPL
  wi_fund_extras <- read.csv("year0/WI/data/SFY2022-Funding-List.csv") %>%
    clean_names() %>%
    mutate(project_id = str_squish(project_number),
           project_id = str_replace(project_id, "‐", "-")) %>%
    filter(project_id %in% c("5584-08", "5191-13", "5340-07",
                             "5430-09", "5430-10")) %>%
    mutate(community_served = str_to_title(municipality),
           borrower = community_served,
           project_description = str_squish(project_description),
           population = clean_numeric_string(population),
           project_score = str_squish(priority_score),
           requested_amount = clean_numeric_string(requested_project_costs),
           estimated_loan_amount = convert_to_numeric(estimated_loan_amount, fill_na=TRUE),
           principal_forgiveness = convert_to_numeric(pf_estimate, fill_na=TRUE),
           funding_amount = estimated_loan_amount + principal_forgiveness,
           funding_amount = clean_numeric_string(funding_amount),
           principal_forgiveness = clean_numeric_string(pf_estimate),
           disadvantaged = ifelse(eligible_pf != "0%", "Yes", "No"),
           expecting_funding = "Yes"
           ) %>%
    select(community_served, borrower, project_description, population,
           project_score, requested_amount, principal_forgiveness,
           funding_amount, disadvantaged, expecting_funding, project_id)
    
  
  
  wi_clean <- wi_ppl %>%
    left_join(wi_fund, by="project_id")
  
  wi_clean <- bind_rows(wi_clean, wi_fund_extras)
  
  
  
  wi_clean <- wi_clean %>%
    mutate(pwsid = as.character(NA),
           project_name = as.character(NA),
           project_type = as.character(NA),
           project_rank = as.character(NA),
           project_cost = replace_na(project_cost, "No Information"),
           project_score = replace_na(project_score, "No Information"),
           requested_amount = replace_na(requested_amount, "No Information"),
           funding_amount = replace_na(funding_amount, "No Information"),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
           disadvantaged = replace_na(disadvantaged, "No Information"),
           expecting_funding = replace_na(expecting_funding, "No"),
           state_fiscal_year = "2022",
           state = "Wisconsin") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year)
  
  
  run_tests(wi_clean)
  rm(list=setdiff(ls(), "wi_clean"))
  
  return(wi_clean)
}
