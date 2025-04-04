clean_mn_y2 <- function() {
  
  # (187, 15) -> (187, 18)
  table_1a <- read.csv("year2/MN/data/draft-2024-drinking-water-intended-use-plan-project-priority-list-table-1a.csv") %>%
    clean_names() %>%
    mutate(
      principal_forgiveness = convert_to_numeric(estimated_dwrf_emerging_contaminant_pf_grant_not_final_1) +
                              convert_to_numeric(estimated_dwrf_disadvantgd_community_principal_forgiveness_not_final_2),
      funding_amount = convert_to_numeric(estimated_dwrf_loan) + 
                       principal_forgiveness,
      expecting_funding = case_when(
        funding_amount > 0 ~ "Yes",
        TRUE ~ "No"), 
      funding_amount = clean_numeric_string(funding_amount), 
      principal_forgiveness = clean_numeric_string(principal_forgiveness), 
           )
  
  # (55, 14) -> (55,17)
  table_1b <- read.csv("year2/MN/data/draft-2024-drinking-water-intended-use-plan-project-priority-list-table-1b.csv") %>%
    clean_names() %>%
    mutate(
      principal_forgiveness = convert_to_numeric(estimated_dwrf_lsl_pf_grant_3),
      funding_amount = convert_to_numeric(estimated_dwrf_lsl_loan_4) +
                       principal_forgiveness,
      funding_amount = clean_numeric_string(funding_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      expecting_funding = "Yes"
    )
  
  # (-> 242,4)
  table_1 <- bind_rows(table_1a, table_1b) %>%
    rename(project_id = project_number)  %>%
    select(project_id, funding_amount, principal_forgiveness, expecting_funding)
  
  # (844, 7)
  ppl <- read.csv("year2/MN/data/draft-2024-drinking-water-intended-use-plan-project-priority-list-appendix.csv") %>%
    clean_names()
  
  # -> (844,16)
  mn_clean <- ppl %>%
    left_join(table_1, by="project_id") %>%
    mutate(
      community_served = as.character(NA),
      borrower = str_squish(system),
      pwsid = as.character(NA),
      project_id = str_squish(project_id),
      project_name = as.character(NA),
      project_type = case_when(
        grepl("LSL", project) ~ "Lead",
        grepl("Manganese", project) | grepl("PFAS", project) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = clean_numeric_string(project_cost),
      requested_amount = as.character(NA),
      funding_amount = clean_numeric_string(funding_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      population = clean_numeric_string(population),
      project_description = str_squish(project),
      disadvantaged = as.character(NA),
      project_rank = str_squish(rank),
      project_score = str_squish(points),
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Minnesota",
      state_fiscal_year = "2024"
    )  %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(mn_clean)
  
  rm(list=setdiff(ls(), "mn_clean"))
  
  return(mn_clean)
}
