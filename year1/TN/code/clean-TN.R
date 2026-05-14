clean_tn_y1 <- function() {
  
  # (143, 19) - this file has been updated with the correct one on 
  # March 19th 2025
  tn_ppl <- data.table::fread("year1/TN/data/sfy_23_basegen_supp_ppl.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY23 Base/IIJA Gen Supp PPL") 
  
  tn_clean <- tn_ppl |>
    # # process numeric columns
    dplyr::mutate(
      project_cost = clean_numeric_string(total_project_amount),
      population = clean_numeric_string(pop_served)) |>
    # process text columns 
    dplyr::mutate(
      community_served = str_squish(county),
      borrower = stringr::str_squish(local_government),
      pwsid = stringr::str_squish(pwsid_number),
      pwsid = ifelse(pwsid == "TN000253", "TN0000253", pwsid),
      project_id = as.character(NA),
      project_name = as.character(NA), 
      project_description = stringr::str_squish(project_description), 
      project_type = case_when(
        grepl("lsl|lead", project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"), 
      requested_amount = as.character(NA), 
      funding_amount = as.character(NA),
      #[keep] FY22 Base Principal Forgiveness Amount corresponds to principal_forgiveness_amount
      principal_forgiveness_amount = convert_to_numeric(principal_forgiveness_amount,fill_na_0 = TRUE),
      #[keep] FY22 BIL Principal Principal Forgiveness Amount corresponds to principal_forgiveness_amount_2
      principal_forgiveness_amount_2 = convert_to_numeric(principal_forgiveness_amount_2,fill_na_0 = TRUE),
      #[keep] Revolving Base Principal Forgiveness corresponds to revolving_base_principal_forgiveness
      revolving_base_principal_forgiveness = convert_to_numeric(revolving_base_principal_forgiveness,fill_na_0 = TRUE),
      principal_forgiveness = principal_forgiveness_amount + principal_forgiveness_amount_2 + revolving_base_principal_forgiveness,
      # [keep] all projects are ranked
      principal_forgiveness = clean_numeric_string(principal_forgiveness),       
      disadvantaged = dplyr::case_when(
        grepl("\\*", local_government) ~ "Yes",
        TRUE ~ "No"),
      # [keep] with dac defined, remove extra characters referencing table footnotes
      borrower = stringr::str_replace_all(borrower, "\\*", ""),
      borrower = stringr::str_replace_all(borrower, "\\+", ""),
      project_rank = stringr::str_squish(rank_order),
      project_score = stringr::str_squish(priority_points),
      expecting_funding = ifelse(row_number() < 108, "Yes", "No"),
      state = "Tennessee",
      state_fiscal_year = "2023"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  
  # Check pwsid length
  # tn_clean[stringr::str_count(tn_clean$pwsid) != 9, pwsid]

  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  # tn_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: Disinfection byproduct string
  
  # Check for lead subtypes: Both
  # tn_clean |>
  #   dplyr::filter(project_type=="Lead") |>
  #   dplyr::mutate(
  #     lead_type = dplyr::case_when(
  #       stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
  #       stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
  #       stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
  #       # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
  #       stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
  #       stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
  #       TRUE ~ "unknown"
  #     )
  #   ) |>
  #   dplyr::filter(lead_type == "both")

  ####### Decision: No lead projects classified as both
  
  # Check for lead subtypes: Unknown
  # tn_clean |>
  #   dplyr::filter(project_type=="Lead") |>
  #   dplyr::mutate(
  #     lead_type = dplyr::case_when(
  #       stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
  #       stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
  #       stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
  #       # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
  #       stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
  #       stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
  #       TRUE ~ "unknown"
  #     )
  #   ) |>
  #   dplyr::filter(lead_type == "unknown")

  ####### Decision: No lead projects classified as unknown

  ####### SANITY CHECKS END #######
  
  run_tests(tn_clean)
  rm(list=setdiff(ls(), "tn_clean"))
  
  return(tn_clean)
}
