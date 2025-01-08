source("resources.R")

clean_mi_y3 <- function() {
  
  mi_raw <- fread("year3/MI/data/MI-FY2025-DWSRF-Final-IUP.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  mi_clean <- mi_raw %>%
    filter(!is.na(project_number)) %>%
    #convert all of the various funding columns to numeric and add for funding and PF
    mutate(dwsrf_loan = convert_to_numeric(dwsrf_loan, TRUE),
           dwsrf_pf = convert_to_numeric(dwsrf_pf, TRUE),
           bil_dwsrf_supplemental_loan = convert_to_numeric(bil_dwsrf_supplemental_loan, TRUE),
           bil_dwsrf_supplemental_pf = convert_to_numeric(bil_dwsrf_supplemental_pf, TRUE),
           bil_emerging_contaminant_pf = convert_to_numeric(bil_emerging_contaminant_pf, TRUE),
           bil_dwsrf_lslr_loan = convert_to_numeric(bil_dwsrf_lslr_loan, TRUE),
           bil_dwsrf_lslr_pf = convert_to_numeric(bil_dwsrf_lslr_pf, TRUE),
           
           funding_amount = dwsrf_loan + dwsrf_pf + bil_dwsrf_supplemental_loan + bil_dwsrf_supplemental_pf +
             bil_emerging_contaminant_pf + bil_dwsrf_lslr_loan + bil_dwsrf_lslr_pf,
           funding_amount = clean_numeric_string(funding_amount),
           
           principal_forgiveness = dwsrf_pf + bil_dwsrf_supplemental_pf + bil_emerging_contaminant_pf + bil_dwsrf_lslr_pf,
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           
           requested_amount = clean_numeric_string(total_loan_amount_requested),
           
           project_id = str_squish(project_number),
           project_description = str_squish(project_scope),
           population = clean_numeric_string(population_served_by_project),
           borrower = str_squish(applicant),
           community_served = str_squish(project_county),
           project_type = case_when(bil_emerging_contaminant_pf > 0 ~ "Emerging Contaminants",
                                    grepl("LSLR", project_scope) ~ "Lead",
                                    TRUE ~ "General"),
           project_score = str_squish(total_priority_points),
           disadvantaged = ifelse(is.na(overburdened_determination), "No", "Yes"),
           state = "Michigan",
           state_fiscal_year = "2025",
           pwsid = as.character(NA),
           project_name = as.character(NA),
           project_rank = as.character(NA),
           project_cost = as.character(NA),
           expecting_funding = case_when(funding_amount != "0" ~ "Yes",
                                         TRUE ~ "No")
           
    ) %>%
    ## keep relevant columns
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(mi_clean)
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}