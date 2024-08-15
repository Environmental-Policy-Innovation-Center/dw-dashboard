source("resources.R")

clean_ia <- function() {
  
  # (185, 15) -> (185, 18)
  ia_clean <- read.csv("./year2/IA/data/Attachment-1-Table-1.csv") %>%
    clean_names() %>%
    mutate(community_served = as.character(NA), 
           borrower = str_squish(project_name), 
           pwsid = as.character(NA), 
           project_id = str_squish(dwsrf_no),
           project_name = as.character(NA), 
           project_type = case_when(
             nchar(lsl) != 0 ~ "Lead", 
             nchar(pfas_ec) != 0 ~ "Emerging Contaminants", 
             TRUE ~ "General"
           ), 
           project_cost = as.character(NA), 
           requested_amount = clean_numeric_string(current_funding_request), 
           funding_amount = clean_numeric_string(total_loan_amount_to_date), 
           principal_forgiveness = as.character(NA), 
           project_description = str_squish(project_description), 
           # there are some NA populations in the original dataset 
           # TODO: replace NAs with "no information" when new function comes out 
           population = clean_numeric_string(pop),
           disadvantaged = as.character(NA), 
           project_rank = as.character(NA), 
           project_score = str_squish(priority_points), 
           expecting_funding = case_when(
             project_status %in% c("L", "R", "C") ~ "Yes", 
             TRUE ~ "No"
           ),
           state = "Iowa", 
           state_fiscal_year = "SFY24") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(ia_clean)
  rm(list=setdiff(ls(), "ia_clean"))
  
  return(ia_clean)
}