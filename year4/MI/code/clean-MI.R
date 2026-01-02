clean_mi_y4 <- function() {
  
  mi_traditional <- data.table::fread("year4/MI/data/Final-SFY26 Comprehensive PPL.csv",
                  colClasses = "character", na.strings = "") |>
    clean_names() |>
    dplyr::mutate(
      list = "traditional"
    )
  
  mi_lead <- data.table::fread("year4/MI/data/Final-SFY26 LSLR PPL.csv",
                  colClasses = "character", na.strings = "") |>
    clean_names() |>
    dplyr::mutate(
      project_type = "Lead",
      list = "lead")

  mi_combined <- mi_traditional |>
    dplyr::bind_rows(mi_lead)

  mi_clean <- mi_combined |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = stringr::str_squish(applicant),
      pwsid = as.character(NA),
      project_id = stringr::str_squish(project_number),
      project_name = as.character(NA),
      project_description = dplyr::case_when(
        list == "traditional" ~ stringr::str_squish(scope_of_work),
        list == "lead" ~ stringr::str_squish(scope_of_work)
      ),
      project_type =  case_when(
        !is.na(project_type) ~ project_type,
        grepl(lead_str, project_description, ignore.case=TRUE)  ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) | convert_to_numeric(ec_cost)>0 ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = clean_numeric_string(project_cost),
      requested_amount = as.character(NA),
      funding_amount = dplyr::case_when(
        list == "traditional" ~ convert_to_numeric(total_award),
        list == "lead" ~ convert_to_numeric(lslr_bil_loan, TRUE) + convert_to_numeric(lslr_bil_pf, TRUE) +
        convert_to_numeric(dwsrf_loan, TRUE) + convert_to_numeric(bil_pf, TRUE) 
      ), 
      funding_amount = clean_numeric_string(funding_amount),
      principal_forgiveness = dplyr::case_when(
        list == "traditional" ~ convert_to_numeric(dwsrf_pf_allocation, TRUE) +
        convert_to_numeric(bil_ec_allocation, TRUE) +
        convert_to_numeric(bil_pf_allocation, TRUE),
        list == "lead" ~ convert_to_numeric(lslr_bil_pf, TRUE) + convert_to_numeric(bil_pf, TRUE)
      ),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      population = as.character(NA),
      disadvantaged = dplyr::case_when(
       list == "traditional" & disadvantaged_status == "NA" ~ "No",
       list == "traditional" ~ "Yes",
       list == "lead" & disadvantaged_status == "NA" ~ "No",
       list == "lead" ~ "Yes",
       .default = "check" #Q/C
      ),
      project_rank = as.character(NA),
      project_score = dplyr::case_when(
        list == "traditional" ~ stringr::str_squish(project_score),
        list == "lead" ~ stringr::str_squish(score),
        .default = "check" #Q/C
      ),
      expecting_funding = dplyr::case_when(
        comment == "Declined" ~ "No",
        funding_amount != "0" ~ "Yes",
        .default = "No"
      ),
      state = "Michigan",
      state_fiscal_year = "2026"
    ) 
  
  mi_clean <- mi_clean |>
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

  # Check for lead subtypes
  mi_clean |>
    dplyr::filter(project_type=="Lead") |>
  dplyr::mutate(
    lead_type = dplyr::case_when(
    stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
    stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
    stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
   # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
   stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
   stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
    TRUE ~ "unknown"
  )) |>
    dplyr::filter(lead_type == "both")
  ####### Decision: No lead projects classified as both
    
  ####### SANITY CHECKS END #######
  
  run_tests(mi_clean)
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}