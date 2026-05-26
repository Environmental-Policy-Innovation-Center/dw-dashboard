clean_wi_y4 <- function() {
  
  wi_gen_ec_comp <- fread("year4/WI/data/wi-sfy26-gen-ec-comp-list.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY26 General and EC Comprehensive List") |>
    unique()
  
  wi_lsl_comp <- fread("year4/WI/data/wi-sfy26-lsl-comp-list.csv",
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY26 LSL Comprehensive List",
      project_type = "Lead"
    )
  
  wi_not_fundable <- fread("year4/WI/data/wi-sfy26-not-fundable-list.csv",
                           colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY26 Not Fundable List",
           expecting_funding = "No")
  
  # join comp and not funding list to merge onto
  wi_comp_all <- bind_rows(wi_gen_ec_comp, wi_not_fundable, wi_lsl_comp) |>
    dplyr::mutate(project_cost = clean_numeric_string(estimated_project_cost),
           project_score = stringr::str_squish(self_score1),
           community_served = stringr::str_to_title(municipality),
           project_id = stringr::str_squish(project_number),
           project_id = normalize_dashes(project_id)) |>
    dplyr::select(project_score, community_served, project_id, project_description, project_type, project_cost, list, expecting_funding)
  
  
  # NOTE: EC projects included on General list, don't require separate list, but file kept for posterity
  wi_gen_ec_fundable <- fread("year4/WI/data/wi-sfy26-general-fundable-list.csv",
                              colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY26 General and EC Fundable List",
      project_type = case_when(
        program == "BASE" ~ "General",
        program == "EC" ~ "Emerging Contaminants",
        grepl("(EC)", project_description) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = "No Information",
      community_served = stringr::str_to_title(municipality),
      project_description = stringr::str_squish(project_description),
      project_id = stringr::str_squish(project_number),
      project_id = normalize_dashes(project_id),
      requested_amount = clean_numeric_string(requested_project_costs),
      population = clean_numeric_string(population),
      project_score = stringr::str_squish(priority_score),
      estimated_loan_amount = convert_to_numeric(estimated_loan_amount, T),
      principal_forgiveness = convert_to_numeric(total_estimated_pf, T),
      funding_amount = estimated_loan_amount + principal_forgiveness,
      funding_amount = clean_numeric_string(funding_amount),
      principal_forgiveness = clean_numeric_string(total_estimated_pf),
      expecting_funding = "Yes",
      disadvantaged = case_when(
        as.numeric(pf_points) >= 60 ~ "Yes",
        TRUE ~ "No")
    ) |>
    dplyr::select(project_id, project_score, community_served, project_description, population, project_type, project_cost, requested_amount, 
           funding_amount, principal_forgiveness, disadvantaged, list, expecting_funding)
  
  
  wi_lsl_fundable <- fread("year4/WI/data/wi-sfy26-lsl-fundable-list.csv",
                           colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY26 LSL Fundable List",
      project_type = "Lead",
      project_cost = "No Information",
      community_served = stringr::str_to_title(municipality),
      project_id = stringr::str_squish(project_number),
      project_id = normalize_dashes(project_id),
      requested_amount = clean_numeric_string(requested_costs),
      funding_amount = clean_numeric_string(total_funding_allocated),
      principal_forgiveness = clean_numeric_string(total_pf_allocated),
      disadvantaged = ifelse(private_side_pf_percent != "0%", "Yes", "No Information"),
      project_score = stringr::str_squish(lsl_perf_score),
      expecting_funding = "Yes"
    ) |>
    dplyr::select(project_id, community_served, project_score, project_type, project_cost, requested_amount, 
                  funding_amount, principal_forgiveness, disadvantaged, expecting_funding, list)
  
  
  wi_fundable <- bind_rows(wi_gen_ec_fundable, wi_lsl_fundable)
  
  
  # merge funding list onto comp list, keeping projects only on comp funding list
  wi_all <- merge(wi_comp_all, wi_fundable, by="project_id", all=TRUE) |>
    dplyr::mutate(
      project_description = ifelse(!is.na(project_description.y), project_description.y, project_description.x),
      project_type = coalesce(project_type.y, project_type.x),
      project_cost = dplyr::coalesce(project_cost.y, project_cost.x),
      project_score = ifelse(!is.na(project_score.y), project_score.y, project_score.x),
      list = ifelse(!is.na(list.y), list.y, list.x),
      community_served = ifelse(!is.na(community_served.y), community_served.y, community_served.x),
      expecting_funding = ifelse(!is.na(expecting_funding.y), expecting_funding.y, expecting_funding.x),
    ) |>
    dplyr::select(project_id, project_score, community_served, project_description, project_cost, population, project_type,
           requested_amount, funding_amount, principal_forgiveness, disadvantaged, expecting_funding, list)
  
  
  wi_clean <- wi_all |>
    dplyr::mutate(
      borrower = as.character(NA),
      pwsid = as.character(NA),
      project_name = as.character(NA),
      project_rank = as.character(NA),
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        grepl(ec_str, project_description) ~ "Emerging Contaminants",
        grepl("(EC)", project_description) ~ "Emerging Contaminants",
        grepl("lsl|lead", project_description, ignore.case=T) ~ "Lead",
        TRUE ~ "General"),
      # lead projects missing project description when not on comp list
      project_description = replace_na(project_description, "No Information"),
      # project score NA for projects on Not Fundable list
      project_score = replace_na(project_score, "No Information"),
      # project cost NA for projects not on comp list, but on fundable list
      #this should have caught fundable projects, however projects in the fundable lists that were also in comp inherited project costs
      project_cost = replace_na(project_cost, "No Information"),
      # population NA for all but Gen/EC fundable list
      population = replace_na(population, "No Information"),
      # NA for non-fundable list projects
      requested_amount = replace_na(requested_amount, "No Information"),
      disadvantaged = replace_na(disadvantaged, "No Information"),
      funding_amount = clean_numeric_string(funding_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      # all projects not Yes from comp funding list No
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Wisconsin",
      state_fiscal_year = "2026"
    ) |>    
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######

# Hone in on project id duplication
# wi_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

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

  ### Decision: 85 unknowns cannot be ressolved
####### SANITY CHECKS END #######
  
  run_tests(wi_clean)
  return(wi_clean)
}


