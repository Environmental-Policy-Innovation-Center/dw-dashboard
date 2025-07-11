clean_id_y1 <- function() {
  
  # (81, 9)
  id_ppl <- fread("year1/ID/data/12-Idaho_PPL.csv", 
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # (13,11) -> (13,4)
  # reduce fundable projects to only their rank (a UID in this case), 
  # principal forgiveness amount, and loan amount
  id_fnd <- fread("year1/ID/data/12-Idaho_Fundable.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(rank = str_replace_all(rank,"[^0-9.]", ""),
           loan_amt = as.character(map(strsplit(loan_amt_est_loan_date, split = "  "), 1)),
           proposed_funding_terms = str_squish(proposed_funding_terms),
           # see NOTE for disadv definition
           disadvantaged = case_when(
             grepl("30 years", proposed_funding_terms) ~ "Yes",
             grepl("principal forgiveness", proposed_funding_terms, ignore.case = TRUE) ~ "Yes",
             grepl("PF", proposed_funding_terms) ~ "Yes",
             TRUE ~ "No"),
           # extract PF value and format as numeric
           principal_forgiveness = as.character(map(strsplit(proposed_funding_terms, split = "with"), 2)),
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information")
    ) %>%
    select(rank, loan_amt, principal_forgiveness, disadvantaged)
  
  # (81,12)
  id_merged <- merge(id_ppl, id_fnd, by=c("rank"), all.x=TRUE)
  
  # -> (81,13)
  id_clean <- id_merged %>%
    # process numeric columns
    mutate(population = clean_numeric_string(pop_served),
           project_cost = clean_numeric_string(project_cost),
           funding_amount = clean_numeric_string(loan_amt),
    ) %>%
    # process text columns
    mutate(project = str_squish(project),
           # remove excess spaces from scrape
           regional_office = str_squish(regional_office),
           project_score = str_replace_all(rating_points, "[^0-9.]", ""),
           project_rank = str_replace_all(rank, "[^0-9.]", ""),
           project_description = str_squish(project_description),
           # custom touch-up squishing doesn't fix
           project_description = str_replace(project_description, "C onstruction", "Construction"),
           # if from fundable table, else Applicant
           expecting_funding = case_when(
             !is.na(funding_amount) ~ "Yes",
             TRUE ~ "No"),
           state = "Idaho",
           state_fiscal_year = "2023",
           # only project with Lead is the "All System fund"
           project_type = case_when(
             grepl("Lead", project) ~ "Lead",
             TRUE ~ "General"),
           pwsid = str_replace(system_number, "Unknown", "No Information"),
           pwsid = str_replace(pwsid, "All", "No Information"),
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
           disadvantaged = replace_na(disadvantaged, "No Information")
    ) %>%
    rename(borrower = project) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(id_clean)
  rm(list=setdiff(ls(), "id_clean"))
  
  return(id_clean)
}