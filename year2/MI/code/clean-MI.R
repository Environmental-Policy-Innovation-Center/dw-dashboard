source("resources.R")

clean_mi_y2 <- function() {

  mi_raw <- fread("year2/MI/data/mi-sfy24-iup.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()

  mi_clean <- mi_raw %>%
           #convert all of the various funding columns to numeric and add for funding and PF
    mutate(dwsrf_traditional_loan = convert_to_numeric(dwsrf_traditional_loan, TRUE),
           dwsrf_traditional_pf = convert_to_numeric(dwsrf_traditional_pf, TRUE),
           bil_dwsrf_supplemental_loan = convert_to_numeric(bil_dwsrf_supplemental_loan, TRUE),
           bil_dwsrf_supplemental_pf = convert_to_numeric(bil_dwsrf_supplemental_pf, TRUE),
           bil_dwsrf_emerging_contaminants_pf = convert_to_numeric(bil_dwsrf_emerging_contaminants_pf, TRUE),
           bil_dwsrf_lslr_loan = convert_to_numeric(bil_dwsrf_lslr_loan, TRUE),
           bil_dwsrf_lslr_pf = convert_to_numeric(bil_dwsrf_lslr_pf, TRUE),
           
           funding_amount = dwsrf_traditional_loan + dwsrf_traditional_pf + bil_dwsrf_supplemental_loan + bil_dwsrf_supplemental_pf +
             bil_dwsrf_emerging_contaminants_pf + bil_dwsrf_lslr_loan + bil_dwsrf_lslr_pf,
           funding_amount = clean_numeric_string(funding_amount),
           
           principal_forgiveness = dwsrf_traditional_pf + bil_dwsrf_supplemental_pf + bil_dwsrf_emerging_contaminants_pf + bil_dwsrf_lslr_pf,
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           
           project_id = str_squish(project_number),
           project_id = replace_na(project_id, "No Information"),
           project_cost = clean_numeric_string(total_project_costs),
           project_description = str_squish(project_components),
           population = clean_numeric_string(population),
           borrower = str_squish(applicant),
           community_served = str_squish(county),
           project_type = case_when(grepl("LSL", project_components) ~ "Lead",
                                    bil_dwsrf_emerging_contaminants_pf > 0 ~ "Emerging Contaminants",
                                    TRUE ~ "General"),
           project_score = str_squish(total_priority_points),
           disadvantaged = ifelse(is.na(disadvantaged_status), "No", "Yes"),
           state = "Michigan",
           state_fiscal_year = "2024",
           pwsid = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           project_rank = as.character(NA),
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