clean_ri_y1 <- function() {
  
  # (129, 16)
  ri_comp <- fread("year1/RI/data/39-RhodeIsland_PPL_Comprehensive.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # -> (129, 10)
  ri_clean <- ri_comp %>%
    mutate(
      requested_amount = clean_numeric_string(funds_requested),
      population = clean_numeric_string(pop_served),
      
    ) %>%
    mutate(borrower = str_squish(system_name),
           borrower = str_remove(borrower, "\\*"),
           project_score = str_replace_all(scores_total,"[^0-9.]", ""),
           pwsid = paste0("RI", pws_id),
           project_description = str_squish(project_description),
           project_type = case_when(
             grepl("EC", source_fund, ignore.case=TRUE) ~ "Emerging Contaminants",
             grepl("LL", source_fund, ignore.case=TRUE) ~ "Lead",
             grepl("BS", source_fund) | grepl("SS", source_fund) ~ "General",
             TRUE ~ "No Information"),
           state = "Rhode Island",
           state_fiscal_year = "2023",
           expecting_funding = "No",
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           disadvantaged = as.character(NA),
           project_rank = as.character(NA)
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(ri_clean)
  rm(list=setdiff(ls(), "ri_clean"))
  
  return(ri_clean)
}