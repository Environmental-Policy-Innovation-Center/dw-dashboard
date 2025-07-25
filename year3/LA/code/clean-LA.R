clean_la_y3 <- function() {
  
  base_app <- fread("year3/LA/data/base-applicant.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names()
  
  
  base_fund <- fread("year3/LA/data/base-fundable.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           principal_forgiveness = clean_numeric_string(amount_of_principal_forgiveness)) %>%
    select(dwrlf_project_number, expecting_funding, principal_forgiveness)
  
  base_all <- base_app %>%
    left_join(base_fund, by="dwrlf_project_number") %>%
    select(-potential_funding_source_s, -expected_loan_term, -est_date_to_close_loan, -readiness_to_proceed,
           -compliance_correction_list_ao, -arpa_wsp_match_project, -preliminary_water_system_grade)
  
  # 2025 combines gen, lead, and ec applicants into a single list
  # whereas fundable lists are separate by fed cap grant
  bil_comp <- fread("year3/LA/data/comprehensive-bil-list.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(est_loan_amount = clean_numeric_string(est_loan_amount))
  
  gs_fundable <- fread("year3/LA/data/fundable-gen-supp.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes")
  
  ec_fundable <- fread("year3/LA/data/fundable-ec.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           expecting_funding = "Yes")
  
  lead_fundable <- fread("year3/LA/data/fundable-lead.csv",
                         colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead",
           expecting_funding = "Yes")
  
  # combine and simplify fundable lists
  fundable <- bind_rows(gs_fundable, ec_fundable, lead_fundable) %>%
    mutate(principal_forgiveness = clean_numeric_string(amount_of_principal_forgiveness),
           est_loan_amount = clean_numeric_string(est_loan_amount)) %>%
    select(dwrlf_project_number, est_loan_amount, expecting_funding, project_type, principal_forgiveness)
  
  # join comprehensive applicant list with fundable list
  bil_all <- bil_comp %>%
    left_join(fundable, by=c("dwrlf_project_number", "est_loan_amount")) %>%
    select(-potential_funding_source_s, -expected_loan_term, -est_date_to_close_loan, -readiness_to_proceed,
           -compliance_correction_list_ao, -arpa_wsp_match_project, -preliminary_water_system_grade)
  
  # combine and clean base and bil lists, which appear to not be overlapping, unlike 2024
  la_clean <- bind_rows(base_all, bil_all) %>% 
    mutate(
      community_served = as.character(NA),
      borrower = str_squish(water_system_name),
      # extract pwsid from project number
      pwsid = paste0("LA",str_sub(dwrlf_project_number, start=1, end=7)),
      project_id = str_squish(dwrlf_project_number),
      project_name = as.character(NA),
      project_type = case_when(
        # use type if already defined
        !is.na(project_type) ~ project_type,
        # define type by source of BIL funding considered
        grepl("BIL-EC", bil_project) ~ "Emerging Contaminants",
        grepl("BIL-LSL", bil_project) ~ "Lead",
        # search for keywords
        grepl(lead_str, project_description) ~ "Lead",
        grepl(ec_str, project_description) ~ "Emerging Contaminants",
        # all else is general
        TRUE ~ "General"),
      project_cost = as.character(NA),
      requested_amount = clean_numeric_string(est_loan_amount),
      funding_amount = as.character(NA),
      principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
      project_description = str_squish(project_description),
      population = clean_numeric_string(system_population),
      disadvantaged = ifelse(grepl("Yes", meets_affordability_criteria), "Yes", "No"),
      project_rank = as.character(NA),
      project_score = clean_numeric_string(priority_points),
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Louisiana",
      state_fiscal_year = "2025") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(la_clean)
  rm(list=setdiff(ls(), "la_clean"))
  
  return(la_clean)
}