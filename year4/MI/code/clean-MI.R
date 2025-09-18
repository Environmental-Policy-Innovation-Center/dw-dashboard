clean_mi_y4 <- function() {
  
  mi_traditional <- data.table::fread("year4/MI/data/MI-FY26-DWSRF-DRAFT-IUP-PPL.csv",
                  colClasses = "character", na.strings = "") |>
    clean_names() |>
    dplyr::mutate(
      list = "traditional"
    )
  
  mi_lead <- data.table::fread("year4/MI/data/MI-FY26-DWSRF-DRAFT-IUP-Lead-PPL.csv",
                  colClasses = "character", na.strings = "") |>
    clean_names() |>
    dplyr::mutate(
      project_type = "Lead",
      list = "lead")

  mi_combined <- mi_traditional |>
    dplyr::bind_rows(mi_lead)

  mi_clean <- mi_combined |>
    filter(!is.na(project_number)) |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = stringr::str_squish(applicant),
      pwsid = as.character(NA),
      project_id = stringr::str_squish(project_number),
      project_name = as.character(NA),
      project_type =  case_when(
        !is.na(project_type) ~ project_type,
        grepl(lead_str, project_scope, ignore.case=TRUE)  ~ "Lead",
        grepl(ec_str, project_scope, ignore.case=TRUE) | !is.na(emerging_contaminant_ec_cost) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = clean_numeric_string(project_cost),
      requested_amount = as.character(NA),
      funding_amount = dplyr::case_when(
        list == "traditional" ~ convert_to_numeric(total_award),
        list == "lead" ~ convert_to_numeric(bil_lslr_loan_allocation, TRUE) +
        convert_to_numeric(bil_lslr_pf_allocation, TRUE) 
      ), 
      funding_amount = clean_numeric_string(funding_amount),
      principal_forgiveness = convert_to_numeric(dwsrf_pf_allocation, TRUE) +
        convert_to_numeric(bil_ec_allocation, TRUE) +
        convert_to_numeric(bil_pf_allocation, TRUE) ,
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      population = as.character(NA),
      project_description = dplyr::case_when(
        list == "traditional" ~ stringr::str_squish(project_scope),
        list == "lead" ~ stringr::str_squish(scope_of_work)
      ),
      disadvantaged = dplyr::case_when(
        is.na(disadvantaged_status) ~ "No",
        .default = "Yes"
      ),
      project_rank = as.character(NA),
      project_score = stringr::str_squish(project_score),
      expecting_funding = dplyr::case_when(funding_amount != "0" ~ "Yes",
           TRUE ~ "No"),
      state = "Michigan",
      state_fiscal_year = "2026"
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