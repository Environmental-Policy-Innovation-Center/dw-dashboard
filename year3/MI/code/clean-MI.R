clean_mi_y3 <- function() {
  
  mi_raw <- fread("year3/MI/data/MI-FY2025-DWSRF-Final-IUP.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  mi_clean <- mi_raw |>
    filter(!is.na(project_number)) |>
    dplyr::mutate(
      community_served = stringr::str_squish(project_county),
      borrower = stringr::str_squish(applicant),
      pwsid = as.character(NA),
      project_id = dplyr::case_when(
        is.na(project_number) ~ "No Information",
        .default = stringr::str_squish(project_number)
      ),
      project_name = as.character(NA),
      project_description = stringr::str_squish(project_scope),
      project_type =  case_when(
             grepl(lead_str, project_description, ignore.case=TRUE) | convert_to_numeric(bil_lslr_eligible_costs, TRUE)>0  ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) | convert_to_numeric(emerging_contaminant_costs, TRUE)>0 ~ "Emerging Contaminants",
             TRUE ~ "General"),
      project_cost = as.character(NA),
      requested_amount = clean_numeric_string(total_loan_amount_requested),
      funding_amount = convert_to_numeric(dwsrf_loan, TRUE) + 
        convert_to_numeric(dwsrf_pf, TRUE) +
        convert_to_numeric(bil_emerging_contaminant_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_supplemental_loan, TRUE) +
        convert_to_numeric(bil_dwsrf_supplemental_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_lslr_loan, TRUE) +
        convert_to_numeric(bil_dwsrf_lslr_pf, TRUE),
      funding_amount = clean_numeric_string(funding_amount), 
      principal_forgiveness = convert_to_numeric(dwsrf_pf, TRUE) +
        convert_to_numeric(bil_emerging_contaminant_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_supplemental_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_lslr_pf, TRUE),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      population = as.character(NA),
      disadvantaged = dplyr::case_when(
        is.na(overburdened_determination) ~ "No",
        .default = "Yes"
      ),
      project_rank = as.character(NA),
      project_score = stringr::str_squish(total_priority_points),
      expecting_funding = dplyr::case_when(funding_amount != "0" ~ "Yes",
           TRUE ~ "No"),
      state = "Michigan",
      state_fiscal_year = "2025"
    ) |>
    ## keep relevant columns
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  
  mi_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: No duplicates
  
  # Check for disinfection byproduct in description
  mi_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: No disinfection byproduct string
    
  ####### SANITY CHECKS END #######
  
  run_tests(mi_clean)
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}