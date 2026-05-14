clean_tn_y5 <- function() {
  tn_ppl_base <- data.table::fread("year5/TN/data/TN_SFY27_base_PPL.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY27 Base/IIJA Gen Supp PPL"
    )

  tn_ppl_base_unranked <- data.table::fread("year5/TN/data/TN_SFY27_base_PPL_unranked.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY27 Base/IIJA Gen Supp PPL unranked"
    )

  tn_ppl_lslr <- data.table::fread("year5/TN/data/TN_SFY27_LSLR.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_type = "Lead",
      list = "SFY27 LSLR PPL",
      project_rank = paste0(rank_order, " ", solicitation)
    )
  
  tn_ppl_sa_hmw <- data.table::fread("year5/TN/data/TN_SFY27_SA-HMW_PPL.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_type = "General",
      list = "SFY27 SA-HMW PPL"
    )
  
  tn_ppl <- dplyr::bind_rows(tn_ppl_base, tn_ppl_base_unranked, tn_ppl_lslr, tn_ppl_sa_hmw)

  tn_clean <- tn_ppl |>
    dplyr::mutate(
      community_served = str_squish(county),
      borrower = str_squish(local_government),
      pwsid = str_squish(pwsid_number),
      # pwsid = str_replace(pwsid, "NA", "No Information"),
      project_id = str_squish(application_id),
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
      estimated_amount_of_principal_forgiveness = convert_to_numeric(estimated_amount_of_principal_forgiveness, fill_na_0 = TRUE),
      estimated_amount_of_sa_hmw_principal_forgiveness = convert_to_numeric(estimated_amount_of_sa_hmw_principal_forgiveness, fill_na_0 = TRUE),
      principal_forgiveness = dplyr::case_when(
        list == "SFY27 Base/IIJA Gen Supp PPL" ~ estimated_amount_for_green_project_subsidy + estimated_amount_for_disadvantaged_communities_principal_forgiveness + estimated_amount_for_small_systems_principal_forgiveness,
        list == "SFY27 LSLR PPL" ~ estimated_amount_of_principal_forgiveness,  
        list == "SFY27 SA-HMW PPL" ~ estimated_amount_of_sa_hmw_principal_forgiveness,
        list == "SFY27 Base/IIJA Gen Supp PPL unranked" ~ NA

      ),
      principal_forgiveness = clean_numeric_string(principal_forgiveness), 
      population = clean_numeric_string(pop_served),
      disadvantaged = dplyr::case_when(
        as.numeric(atpi) <= 50 ~ "Yes",
        list %in% c("SFY27 Base/IIJA Gen Supp PPL", "SFY27 Base/IIJA Gen Supp PPL unranked") ~ ifelse(grepl("\\*", local_government), "Yes", "No"),
        list == "SFY27 LSLR PPL" & as.numeric(atpi) <= 80 ~ "Yes",
        list == "SFY27 LSLR PPL" ~ ifelse(grepl("\\*", local_government), "Yes", "No"),
        .default = "No"
        ),
      project_rank = ifelse(!is.na(project_rank), project_rank, rank_order),
      project_rank = replace_na(project_rank, "No Information"),
      project_score = str_squish(priority_points),
      project_score = replace_na(project_score, "No Information"),
      expecting_funding = dplyr::case_when(
        list == "SFY27 LSLR PPL" ~ "Yes",
        list == "SFY27 Base/IIJA Gen Supp PPL" ~ "Yes",
        list == "SFY27 SA-HMW PPL" ~ "Yes",
        .default = "No"
        ), 
      state = "Tennessee",
      state_fiscal_year = "2027") |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  # Check pwsid length
  # tn_clean[stringr::str_count(tn_clean$pwsid) != 9, pwsid]
  # Hone in on project id duplication
  # tn_clean |> dplyr::group_by(project_id) |> dplyr::tally() |> dplyr::arrange(desc(n))
  ####### Decision: 1 duplicate, different projects 2025-13624
  
  # Check for disinfection byproduct in description
  # tn_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
  
  # Check for lead subtypes: Both
  tn_clean |>
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
      )
    ) |>
    dplyr::filter(lead_type == "both")

  ####### Decision: 4 projects classified as both --> LSLR
  
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

  ####### Decision: 1 projects classified as unknown --> LSLR

  tn_clean <- tn_clean |>
    dplyr::mutate(
      project_description = dplyr::case_when(
        project_id == "2024-10763" ~ paste0(project_description, " | FT: LSLR"),
        .default = project_description
      )
    )

  ####### SANITY CHECKS END #######
  
  
  run_tests(tn_clean)
  rm(list=setdiff(ls(), "tn_clean"))
  
  return(tn_clean)
}