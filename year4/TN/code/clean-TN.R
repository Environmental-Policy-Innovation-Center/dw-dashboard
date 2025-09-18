clean_tn_y4 <- function() {
  
  tn_ppl <- fread("year4/TN/data/TN-2025-prl-draft.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  tn_clean <- tn_ppl %>%
    mutate(community_served = str_squish(county),
           borrower = str_squish(local_government),
           pwsid = str_squish(pwsid_number),
           pwsid = str_replace(pwsid, "NA", "No Information"),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_description = str_squish(project_description), 
           project_type = case_when(
             grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"), 
           project_cost = clean_numeric_string(total_project_amount),
           requested_amount = as.character(NA),
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_description = str_squish(project_description),
           population = clean_numeric_string(pop_served),
           disadvantaged = ifelse(grepl("\\*", local_government), "Yes", "No"),
           project_rank = str_squish(rank_order),
           project_rank = replace_na(project_rank, "No Information"),
           project_score = str_squish(priority_points),
           project_score = replace_na(project_score, "No Information"),
           expecting_funding = ifelse(project_rank != "No Information", "Yes", "No"),
           state = "Tennessee",
           state_fiscal_year = "2026") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(tn_clean)
  rm(list=setdiff(ls(), "tn_clean"))
  
  return(tn_clean)
}