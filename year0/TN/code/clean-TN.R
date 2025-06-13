clean_tn_y0 <- function() {
  
  # there were a few new additions to the EC string on June 12th 2025 to include
  # various new strings 
  ec_str_tn <- "cyanotoxin|dioxane|emerging contaminant|lithium|manganese|Mn|Perfluoro-n-pentanoic acid|PFPeA|PFAS|PFOA|PFOS|trihalomethane|THM|Unregulated Contaminant Monitoring Rule|DBP|disinfection byproduct|HAA5|haloacetic acid"
  
  # (73, 17) - one of these is a total column that needs to be removed
  tn_ppl <- read.csv("year0/TN/data/tn-srf-dw-fy2021-priority-ranking-list.csv") %>%
    clean_names()
  
  tn_clean <- tn_ppl %>%
    filter(local_government != "") %>%
    mutate(
      community_served = str_squish(county),
      borrower = str_squish(local_government),
      borrower = str_replace_all(borrower, "\\*", ""),
      borrower = str_replace_all(borrower, "\\+", ""),
      pwsid = str_squish(pwsid),
      project_cost = clean_numeric_string(total_project_amount),
      project_description = str_squish(project_description),
      population = clean_numeric_string(pop),
      project_rank = str_squish(rank_order),
      project_score = str_squish(priority_points),
      expecting_funding = case_when(rank_order <= 30 ~ "Yes", 
                                    TRUE ~ "No"),
      project_id = as.character(NA),
      project_name = as.character(NA),
      project_type = case_when(grepl("lead|LSL", project_description, ignore.case = TRUE) ~ "Lead", 
                               grepl(ec_str_tn, project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
                               TRUE ~ "General"),
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      disadvantaged = case_when(atpi > 50 ~ "No", 
                                TRUE ~ "Yes"),
      state = "Tennessee",
      state_fiscal_year = "2022",
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year)
  
  
  
  run_tests(tn_clean)
  rm(list=setdiff(ls(), "tn_clean"))
  
  return(tn_clean)
}
