clean_il_y0 <- function() {
  
  il_base_fundable <- data.table::fread("./year0/IL/data/Base Fundable List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "Base Fundable",
      facility_no = stringr::str_replace(facility_no, "^..", "IL")
    )

  il_base_exhausted <- data.table::fread("./year0/IL/data/Base Exhausted Funding List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "Base Exhausted",
      facility_no = stringr::str_replace(facility_no, "^..", "IL")
    )

  il_base_ineligible <- data.table::fread("./year0/IL/data/Base Ineligible Funding Cap List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "Base Ineligible",
      facility_no = stringr::str_replace(facility_no, "^..", "IL")
    )

  il_base_planning_approval <- data.table::fread("./year0/IL/data/Base Planning Approval List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "Base Planning",
      facility_no = stringr::str_replace(facility_no, "^..", "IL")
    )

  il_base_no_planning_approval <- data.table::fread("./year0/IL/data/Base No Planning Approval List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "Base No Planning",
      facility_no = stringr::str_replace(facility_no, "^..", "IL")
    )

  il_lslr_fundable <- data.table::fread("./year0/IL/data/LSLR Fundable List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "LSLR Fundable",
      facility_no = stringr::str_replace(facility_no, "^..", "IL"),
      project_type = "Lead"
    )
  
  il_lslr_planning_approval <- data.table::fread("./year0/IL/data/LSLR Planning Approval List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "LSLR Planning",
      facility_no = stringr::str_replace(facility_no, "^..", "IL"),
      project_type = "Lead"
    )

  il_combined <- dplyr::bind_rows(
    il_base_fundable, 
    il_base_exhausted, 
    il_base_ineligible, 
    il_base_planning_approval, 
    il_base_no_planning_approval, 
    il_lslr_fundable, 
    il_lslr_planning_approval)
  
  il_clean <- il_combined |>
    # process numeric cols: 
    mutate(
      community_served = as.character(NA),
      borrower = loan_applicant, 
      pwsid = facility_no, 
      project_id = ifelse(l17_number == "TSD|TSO|TBD", "No Information", l17_number),
#       unique(il_combined$l17_number[grepl("S", il_combined$l17_number)])
# [1] "552S" "TSD"  "S186" "3S12" "TSO"
      project_id = dplyr::case_when(
        project_id == "552S" ~ "5525",
        project_id == "S186" ~ "5186",
        project_id == "3S12" ~ "3512",
        .default = project_id
      ),
      project_name = as.character(NA),
      project_description = project_description, 
      project_type =  dplyr::case_when(
        !is.na(project_type) ~ project_type,
        grepl(lead_str, project_description, ignore.case=TRUE)  ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE)  ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = as.character(NA),
      requested_amount = as.character(NA),
      funding_amount = dplyr::case_when(
        list == "Base Fundable" ~ clean_numeric_string(projected_loan_amount),
        list == "LSLR Fundable" ~ clean_numeric_string(estimated_loan_amount),
        .default = "No Information"
      ),
      principal_forgiveness = dplyr::case_when(
        list == "Base Fundable" & (disadvantaged_community_principal_forgiveness == "N/E" | disadvantaged_community_principal_forgiveness == "Funding Exhausted") ~ "0",
        list == "Base Fundable" ~ clean_numeric_string(disadvantaged_community_principal_forgiveness),
        list == "LSLR Fundable" ~ clean_numeric_string(estimated_loan_amount),
        .default = "No Information"
      ),
      population = dplyr::case_when(
        list == "Base Fundable" ~ service_population,
        list == "Base Exhausted" ~ service_population,
        list == "Base Ineligible" ~ service_population,
        .default = "No Information"
      ),
      disadvantaged = dplyr::case_when(
        list == "Base Fundable" & (!is.na(convert_to_numeric(disadvantaged_community_principal_forgiveness)) | disadvantaged_community_principal_forgiveness == "Funding Exhausted") ~ "Yes",
        list == "Base Exhausted" & (!is.na(convert_to_numeric(disadvantaged_community_principal_forgiveness)) | disadvantaged_community_principal_forgiveness == "Funding Exhausted") ~ "Yes",
        list == "Base Ineligible" & (!is.na(convert_to_numeric(disadvantaged_community_principal_forgiveness)) | disadvantaged_community_principal_forgiveness == "Funding Exhausted") ~ "Yes",
        list == "Base Fundable" & disadvantaged_community_principal_forgiveness == "N/E"  ~ "No",
        list == "Base Exhausted" & disadvantaged_community_principal_forgiveness == "N/E"  ~ "No",
        list == "Base Ineligible" & disadvantaged_community_principal_forgiveness == "N/E"  ~ "No",
        .default = "No Information"
      ),
      project_rank = as.character(NA),
      project_score = dplyr::case_when(
        list == "Base Fundable" ~ clean_numeric_string(loan_priority_score),
        list == "Base Exhausted" ~ clean_numeric_string(loan_priority_score),
        list == "Base Ineligible" ~ clean_numeric_string(loan_priority_score),
        list == "LSLR Fundable" ~ clean_numeric_string(loan_priority_score),
        .default = "No Information"
      ), 
      expecting_funding = dplyr::case_when(
        list == "Base Fundable" ~ "Yes",
        list == "LSLR Fundable" ~ "Yes",
        .default = "No"
      ),
      state = "Illinois", 
      state_fiscal_year = "2022") 
  
  il_clean <- il_clean |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  il_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
    
  ####### SANITY CHECKS END #######
  
  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)
}