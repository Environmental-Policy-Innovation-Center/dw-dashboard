clean_tn_y2 <- function() {
  
  # (70, 18) - gen PPL
  gen_ppl <- data.table::fread("year2/TN/data/tn_ppl.csv",
                   colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY24 Base/IIJA Gen Supp PPL")

  # (70, 18)
  tn_clean <- gen_ppl |>
    dplyr::mutate(
      community_served = str_squish(county),
      # [keep] removing weird characters from local government column
      borrower = stringr::str_replace_all(local_government, "\\*", ""),
      borrower = stringr::str_replace_all(borrower, "\\+", ""),
      borrower = stringr::str_squish(borrower),
      pwsid = stringr::str_squish(pwsid_number), 
      project_id = as.character(NA),
      project_name = as.character(NA), 
      project_description = stringr::str_squish(project_description), 
      project_type = dplyr::case_when(
        grepl("lsl|lead", project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"),         
      project_cost = clean_numeric_string(total_project_amount), 
      requested_amount = as.character(NA), 
      funding_amount = as.character(NA),
      fy23_base_principal_forgiveness_amount = convert_to_numeric(fy23_base_principal_forgiveness_amount,fill_na_0 = TRUE),
      fy23_bil_principal_forgiveness_amount = convert_to_numeric(fy23_bil_principal_forgiveness_amount,fill_na_0 = TRUE),
      principal_forgiveness = fy23_base_principal_forgiveness_amount + fy23_bil_principal_forgiveness_amount,
      principal_forgiveness = ifelse(is.na(rank_order), "No Information", clean_numeric_string(principal_forgiveness)), 
      population = clean_numeric_string(pop_served), 
      disadvantaged = case_when(
        grepl("\\*", local_government) ~ "Yes",
        TRUE ~ "No"),
      project_rank = replace_na(rank_order, "No Information"), 
      project_score = replace_na(priority_points, "No Information"),
      expecting_funding = case_when(
        project_rank == "No Information" ~ "No", 
        TRUE ~ "Yes"), 
      state = "Tennessee",
      state_fiscal_year = "2024") |>
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
  ####### Decision: No disinfection byproduct string
  
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
