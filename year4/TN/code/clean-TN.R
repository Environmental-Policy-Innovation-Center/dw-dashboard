clean_tn_y4 <- function() {
  
  tn_ppl_base <- data.table::fread("year4/TN/data/TN_SFY26_base_PPL.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY26 Base/IIJA Gen Supp PPL"
    )

  tn_ppl_base_unranked <- data.table::fread("year4/TN/data/TN_SFY26_base_PPL_unranked.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY26 Base/IIJA Gen Supp PPL unranked"
    )

  tn_ppl_ec <- data.table::fread("year4/TN/data/TN_SFY26_EC_PPL.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_type = "Emerging Contaminants",
      list = "SFY26 EC PPL"
    )
  
  tn_ppl <- dplyr::bind_rows(tn_ppl_base, tn_ppl_base_unranked, tn_ppl_ec)

  tn_clean <- tn_ppl |>
    dplyr::mutate(
      community_served = str_squish(county),
      borrower = str_squish(local_government),
      pwsid = str_squish(pwsid_number),
      pwsid = str_replace(pwsid, "NA", "No Information"),
      project_id = as.character(NA),
      project_name = as.character(NA),
      project_description = str_squish(project_description), 
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        grepl("lsl|lead", project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"), 
      project_cost = clean_numeric_string(total_project_amount),
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      estimated_amount_for_green_project_subsidy = convert_to_numeric(estimated_amount_for_green_project_subsidy,fill_na_0 = TRUE),
      estimated_amount_for_disadvantaged_communities_principal_forgiveness = convert_to_numeric(estimated_amount_for_disadvantaged_communities_principal_forgiveness,fill_na_0 = TRUE),
      estimated_amount_for_small_systems_principal_forgiveness = convert_to_numeric(estimated_amount_for_small_systems_principal_forgiveness,fill_na_0 = TRUE),
      total_project_amount = convert_to_numeric(total_project_amount,fill_na_0 = TRUE),
      principal_forgiveness = dplyr::case_when(
        list == "SFY26 EC PPL" ~ total_project_amount,
        list == "SFY26 Base/IIJA Gen Supp PPL" ~ estimated_amount_for_green_project_subsidy + estimated_amount_for_disadvantaged_communities_principal_forgiveness + estimated_amount_for_small_systems_principal_forgiveness,
        list == "SFY26 Base/IIJA Gen Supp PPL unranked" ~ NA
      ),
      principal_forgiveness = clean_numeric_string(principal_forgiveness), 
      principal_forgiveness = ifelse(principal_forgiveness == "0", "No Information", principal_forgiveness),
      population = clean_numeric_string(pop_served),
      disadvantaged = dplyr::case_when(
        list %in% c("SFY26 Base/IIJA Gen Supp PPL", "SFY26 Base/IIJA Gen Supp PPL unranked") ~ ifelse(grepl("\\*", local_government), "Yes", "No"),
        list == "SFY26 EC PPL" & as.numeric(atpi) <= 50 ~ "Yes",
        .default = "No"
        ),
      project_rank = str_squish(rank_order),
      project_rank = replace_na(project_rank, "No Information"),
      project_score = str_squish(priority_points),
      project_score = replace_na(project_score, "No Information"),
      expecting_funding = dplyr::case_when(
        list == "SFY26 EC PPL" ~ "Yes",
        list == "SFY26 Base/IIJA Gen Supp PPL" ~ "Yes",
        .default = "No"
        ), 
      state = "Tennessee",
      state_fiscal_year = "2026") |>
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