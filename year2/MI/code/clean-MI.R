clean_mi_y2 <- function() {

  mi_raw <- fread("year2/MI/data/mi-sfy24-iup.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()

  mi_clean <- mi_raw |>
    dplyr::mutate(
      community_served = stringr::str_squish(county),
      borrower = stringr::str_squish(applicant),
      pwsid = as.character(NA),
      project_id = dplyr::case_when(
        is.na(project_number) ~ "No Information",
        .default = stringr::str_squish(project_number)
      ),
      project_name = as.character(NA),
      project_type =  case_when(
             grepl(lead_str, project_components, ignore.case=TRUE) | !is.na(lead_service_line_costs)  ~ "Lead",
             grepl(ec_str, project_components, ignore.case=TRUE) | !is.na(ec_related_costs) ~ "Emerging Contaminants",
             TRUE ~ "General"),
      project_cost = clean_numeric_string(total_project_costs),
      requested_amount = as.character(NA),

      funding_amount = convert_to_numeric(dwsrf_traditional_loan, TRUE) + 
        convert_to_numeric(dwsrf_traditional_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_supplemental_loan, TRUE) +
        convert_to_numeric(bil_dwsrf_supplemental_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_emerging_contaminants_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_lslr_loan, TRUE) +
        convert_to_numeric(bil_dwsrf_lslr_pf, TRUE),
      funding_amount = clean_numeric_string(funding_amount), 
      principal_forgiveness = convert_to_numeric(dwsrf_traditional_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_supplemental_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_emerging_contaminants_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_lslr_pf, TRUE),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      population = stringr::str_squish(population),
      project_description = stringr::str_squish(project_components),
      disadvantaged = dplyr::case_when(
        is.na(disadvantaged_status) ~ "No",
        .default = "Yes"
      ),
      project_rank = as.character(NA),
      project_score = stringr::str_squish(total_priority_points),
      expecting_funding = dplyr::case_when(funding_amount != "0" ~ "Yes",
           TRUE ~ "No"),
      state = "Michigan",
      state_fiscal_year = "2024"
    ) |>   
    ## keep relevant columns
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  
  mi_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  # mi_clean |> dplyr::filter(project_id == "7588-01")
  ####### Decision: Keep as two separate projects, one gets funding from Gen and the other from Lead
 
  
  # Check for disinfection byproduct in description
  mi_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: No disinfection byproduct string
    
  ####### SANITY CHECKS END #######

  run_tests(mi_clean)
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}