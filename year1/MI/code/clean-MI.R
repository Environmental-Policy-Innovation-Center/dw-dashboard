clean_mi_y1 <- function() {
  
  # (112,24)
  mi_raw <- fread("year1/MI/data/22-Michigan_PPL.csv",
                  colClasses = "character", na.strings = "")
  # -> (110,12)
  mi_clean <- mi_raw %>%
    ## clean names
    clean_names() %>%
    ## get rid of rows without projects
    filter(!is.na(project_number)) |>
    dplyr::mutate(
      community_served = stringr::str_squish(project_location),
      borrower = stringr::str_squish(applicant_name),
      pwsid = as.character(NA),
      project_id = str_squish(project_number),
      project_name = as.character(NA),
      project_description = stringr::str_squish(project_description),
      project_type =  case_when(
             grepl(lead_str, project_description, ignore.case=TRUE) | !is.na(lslr_costs)  ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) | !is.na(pfas_costs) ~ "Emerging Contaminants",
             TRUE ~ "General"),
      project_cost = dplyr::case_when(
        !is.na(full_estimated_project_cost_prior_to_30_percent_limitation) ~ 
          clean_numeric_string(full_estimated_project_cost_prior_to_30_percent_limitation),
        .default = clean_numeric_string(estimated_project_cost)
      ),
      requested_amount = as.character(NA),
      funding_amount = convert_to_numeric(dwsrf_loan, TRUE) + 
        convert_to_numeric(dwsrf_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_supplemental_loan, TRUE) +
        convert_to_numeric(bil_dwsrf_supplemental_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_emerging_contaminants_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_lslr_loan, TRUE) +
        convert_to_numeric(bil_dwsrf_lslr_pf, TRUE),
      funding_amount = clean_numeric_string(funding_amount), 
      principal_forgiveness = convert_to_numeric(dwsrf_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_supplemental_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_emerging_contaminants_pf, TRUE) +
        convert_to_numeric(bil_dwsrf_lslr_pf, TRUE),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      population = as.character(NA),
      disadvantaged = dplyr::case_when(
        is.na(disadvantaged) ~ "No",
        .default = "Yes"
      ),
      project_rank = stringr::str_squish(rank),
      project_score = stringr::str_squish(total_points),
      expecting_funding = ifelse((funding_amount != "0" & funding_amount != "No Information"),
                                        "Yes", "No"),
      state = "Michigan",
      state_fiscal_year = "2023",
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
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
  

  mi_clean <- mi_clean |>
    dplyr::mutate(
      project_description = dplyr::case_when(
        project_id == "7704-01" ~ "WM and LSLR looping",
        .default = project_description
      )
    )
  
  mi_clean |>
    dplyr::filter(project_id == "7704-01")
  
  run_tests(mi_clean)
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}