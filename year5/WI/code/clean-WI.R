clean_wi_y5 <- function() {
  
  wi_comp <- fread("year5/WI/data/wi-sfy27-comp-list.csv",
                          colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY27 Comprehensive List") |>
    unique()
  
  wi_lsl_comp <- fread("year5/WI/data/wi-sfy27-lsl-comp-list.csv",
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY27 LSL Comprehensive List",
      project_type = "Lead"
    )
  
  wi_not_fundable <- fread("year5/WI/data/wi-sfy27-not-fundable-list.csv",
                           colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY27 Not Fundable List",
                  expecting_funding = "No")
  
  
  wi_comp_all <- bind_rows(wi_comp, wi_not_fundable, wi_lsl_comp) |>
    dplyr::mutate(project_cost = clean_numeric_string(estimated_project_cost),
                  project_score = stringr::str_squish(self_1score),
                  community_served = stringr::str_to_title(municipality),
                  project_id = stringr::str_squish(project_number),
                  project_id = normalize_dashes(project_id)) |>
    dplyr::select(project_score, community_served, project_id, project_description, project_type, project_cost, list, expecting_funding)

  
  
  wi_clean <- wi_comp_all |>
    dplyr::mutate(
      borrower = as.character(NA),
      pwsid = as.character(NA),
      project_name = as.character(NA),
      project_rank = as.character(NA),
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        grepl(ec_str, project_description) ~ "Emerging Contaminants",
        grepl("(EC1)|(EC2)", project_description) ~ "Emerging Contaminants",
        grepl("lsl|lead", project_description, ignore.case=T) ~ "Lead",
        TRUE ~ "General"),
      # project score NA for projects on Not Fundable list
      project_score = replace_na(project_score, "No Information"),
      #TODO: Update these fields once fundable list is released
      population = as.character(NA),
      requested_amount = as.character(NA),
      disadvantaged = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      expecting_funding = replace_na(expecting_funding, "No Information"),
      
      state = "Wisconsin",
      state_fiscal_year = "2027"
    ) |>    
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type,
                  project_cost, requested_amount, funding_amount, principal_forgiveness,
                  project_description, population, disadvantaged, project_rank, project_score,
                  expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  # wi_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: 2 project ids used twice, different projects for same community. Determined to 
  
  ####### Decision: 
  # Multiple duplicates. 
  # 1 set of 4 duplicates due to 3 redundant, identical duplicates in Gen/EC Comp List. Removed with unique() call.
  # TODO: 4922-21 is for the same municipality and almost identical project descriptions, but different scores and estimated project costs. Leaving both for now, but should consider combining?
  # 5621-12 is one project on Gen/EC Comp List and one on LSL Comp List for same Municipality, but otherwise distinct info. Leaving both.
  
  # Check for disinfection byproduct in description
  # wi_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: No change, classified as expected
  
  # Check for lead subtypes
  # wi_clean |>
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
  #     )) |>
  #   dplyr::filter(lead_type == "both")
  # ####### Decision: No lead projects classified as both
  
  # # Check for lead subtypes: Unknown
  # wi_clean |>
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
  
  ### Decision: No lead types classified as unknown
  ####### SANITY CHECKS END #######
  
  run_tests(wi_clean)
  return(wi_clean)
}


