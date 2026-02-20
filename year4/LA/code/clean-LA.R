clean_la_y4 <- function() {
  # Base Comprehensive -----
  base_comprehensive <- data.table::fread("year4/LA/data/LA_SFY26_comprehensive.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_id = stringr::str_squish(dwrlf_project_number_for_all_bil_projects_add_either_a_gs_ec_lsli_or_lslr_to_the_end),
      project_id = stringr::str_remove_all(project_id, " "),
      list = "base comprehensive"
    )

  # Base Fundable -----
  base_fundable <- data.table::fread("year4/LA/data/LA_SFY26_fundable.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_id = stringr::str_squish(dwrlf_project_number_for_all_bil_projects_add_either_a_gs_ec_lsli_or_lslr_to_the_end),
      project_id = stringr::str_remove_all(project_id, " "),
      list = "base fundable",
      expecting_funding = "Yes",
      principal_forgiveness = clean_numeric_string(amount_of_principal_forgivenes)
    ) |>
    dplyr::select(project_id, expecting_funding, principal_forgiveness)

  # Join base ----
  base <- dplyr::left_join(base_comprehensive, base_fundable, by = "project_id") |>
    dplyr::mutate(
      expecting_funding = tidyr::replace_na(expecting_funding, "No"),
      list = "base"
    )
  
  # IIJA Comprehensive -----
  iija_comprehensive <- data.table::fread("year4/LA/data/LA_SFY26_comprehensive_IIJA.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_id = stringr::str_squish(dwrlf_project_number_for_all_iija_projects_add_either_a_gs_ec_lsli_or_lslr_to_the_end),
      project_id = stringr::str_remove_all(project_id, " "),
      list = "IIJA comprehensive"
    )
  
  # Defaulting to info from fundable list for this project after confirming with Lauren 2026_02_09
  iija_comprehensive <- iija_comprehensive |>
    dplyr::mutate(project_id = ifelse(project_id == "1067003-01-GS", "1067003-02-GS", project_id)) |>
    dplyr::mutate(water_system_name = ifelse(project_id == "1067003-02-GS", "Bastrop Water System - Loan 2", water_system_name))
  
  # IIJA Gen Supp Fundable -----
  gen_supp_fundable <- data.table::fread("year4/LA/data/LA_SFY26_fundable_IIJA_Gen_Sup.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_id = stringr::str_squish(dwrlf_project_number_for_all_iija_projects_add_either_a_gs_ec_lsli_or_lslr_to_the_end),
      project_id = stringr::str_remove_all(project_id, " "),
      list = "Gen Supp fundable",
      expecting_funding = "Yes",
      principal_forgiveness = clean_numeric_string(amount_of_principal_forgivenes)
    ) |>
    dplyr::select(project_id, expecting_funding, principal_forgiveness)

  # IIJA EC Fundable -----
  ec_fundable <- data.table::fread("year4/LA/data/LA_SFY26_fundable_IIJA_EC.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_id = stringr::str_squish(dwrlf_project_number_for_all_bil_projects_add_either_a_gs_ec_lsli_or_lslr_to_the_end),
      project_id = stringr::str_remove_all(project_id, " "),
      project_type = "Emerging Contaminants",
      list = "EC fundable",
      expecting_funding = "Yes",
      principal_forgiveness = clean_numeric_string(amount_of_principal_forgivenes)
    ) |>
    dplyr::select(project_id, project_type, expecting_funding, principal_forgiveness)

  # IIJA Lead Fundable -----
  lead_fundable <- data.table::fread("year4/LA/data/LA_SFY26_fundable_IIJA_LSL.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_id = stringr::str_squish(dwrlf_project_number_for_all_iija_projects_add_either_a_gs_ec_lsli_or_lslr_to_the_end),
      project_id = stringr::str_remove_all(project_id, " "),
      project_type = "Lead",
      list = "Lead fundable",
      expecting_funding = "Yes",
      principal_forgiveness = clean_numeric_string(amount_of_principal_forgivenes)
    ) |>
    dplyr::select(project_id, project_type, expecting_funding, principal_forgiveness)

  # Join ----
  iija <- dplyr::left_join(iija_comprehensive, gen_supp_fundable, by = "project_id") |>
    dplyr::left_join(ec_fundable, by = "project_id") |>
    dplyr::mutate(
      expecting_funding = dplyr::coalesce(expecting_funding.x, expecting_funding.y)
    ) |>
    dplyr::select(-expecting_funding.x, -expecting_funding.y) |>
    dplyr::left_join(lead_fundable, by = "project_id") |>
    dplyr::mutate(
      expecting_funding = dplyr::coalesce(expecting_funding.x, expecting_funding.y)
    ) |>
    dplyr::select(-expecting_funding.x, -expecting_funding.y) |>
    dplyr::mutate(
      expecting_funding = tidyr::replace_na(expecting_funding, "No"),
      list = "iija"
    ) |>
    dplyr::mutate(
      project_type = dplyr::coalesce(project_type.x, project_type.y)
    ) |>
    dplyr::mutate(
      principal_forgiveness = dplyr::coalesce(principal_forgiveness, principal_forgiveness.x, principal_forgiveness.y) 
    ) |>
    dplyr::select(-project_type.x, -project_type.y, -principal_forgiveness.x, -principal_forgiveness.y)
  


  # Join ----
  la_bind <- dplyr::bind_rows(base, iija)

  # Clean ----
  clean_la <- la_bind |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = stringr::str_squish(water_system_name),
      # extract pwsid from project number
      pwsid = paste0("LA",str_sub(project_id, start=1, end=7)),
      project_id = project_id, 
      project_name = as.character(NA),
      project_type = dplyr::case_when(
        # use type if already defined
        !is.na(project_type) ~ project_type,
        #list = "base" & bil_project %in% c("IIJA-LSL", "IIJA-LSLI") ~ "Lead", #nothing meets this criteria
        #list = "base" & bil_project == "IIJA-EC" ~ "Emerging Contaminants", #nothing meets this criteria
        list == "iija" & iija_project %in% c("IIJA-LSL", "IIJA-LSLI") ~ "Lead",
        list == "iija" & iija_project == "IIJA-EC" ~ "Emerging Contaminants",
        grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = as.character(NA),
      requested_amount = clean_numeric_string(est_loan_amount),
      funding_amount = as.character(NA),
      principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
      project_description = str_squish(project_description),
      population = clean_numeric_string(system_population),
      disadvantaged = ifelse(grepl("Yes", meets_affordability_criteria_dac), "Yes", "No"),
      project_rank = as.character(NA),
      project_score = clean_numeric_string(priority_points),
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Louisiana",
      state_fiscal_year = "2026") |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year) 
  
  la_clean <- clean_la |>
    dplyr::distinct() #remove cases where both base and iija produce the same info
  
  ####### SANITY CHECKS START #######

  # Hone in on project id duplication
  #la_clean |> dplyr::distinct() |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision : one project ids duplicated in base and iija list, with the same info except for fundable designation for 1111020-01
  ####### Default to fundable list

  la_clean <- la_clean |>
    dplyr::group_by(project_id) |>
    dplyr::filter(n() == 1 | expecting_funding == "Yes") |>
    dplyr::ungroup()

  #la_clean |> dplyr::distinct() |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

  # Check for disinfection byproduct in description
  #la_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision : No disinfection byproduct string
    
    
  # Check for lead subtypes: Both
  # la_clean |>
  #     dplyr::filter(project_type=="Lead") |>
  #     dplyr::mutate(
  #       lead_type = dplyr::case_when(
  #         stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
  #         stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
  #         stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
  #         # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
  #         stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
  #         stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
  #         TRUE ~ "unknown"
  #       )
  #     ) |>
  #     dplyr::filter(lead_type == "both")
    ####### Decision: One project classified as both
  
  # Check for lead subtypes: Unknown
  # la_clean |>
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
  ######## Decision: Can't amend based on description, this correspond to a project from a lead specific list.

  ####### SANITY CHECKS END #######
  
  run_tests(la_clean)
  rm(list=setdiff(ls(), "la_clean"))
  
  return(la_clean)
}