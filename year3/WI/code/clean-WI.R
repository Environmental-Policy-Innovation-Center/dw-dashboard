clean_wi_y3 <- function() {
  
  wi_comp <- fread("year3/WI/data/wi-sfy25-comp-list.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY25 Comprehensive List") |>
    unique()
  
  wi_not_fundable <- fread("year3/WI/data/wi-sfy25-not-fundable-list.csv",
                           colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY25 Not Fundable List",
      expecting_funding = "No"
    )
  
  # join comp and not funding list to merge onto
  wi_comp_all <- bind_rows(wi_comp, wi_not_fundable) |>
    dplyr::mutate(
      project_cost = clean_numeric_string(estimated_project_cost),
      project_score = stringr::str_squish(self_score1),
      project_score = clean_numeric_string(project_score),
      community_served = stringr::str_to_title(municipality),
      project_id = stringr::str_squish(project_number),
      project_id = normalize_dashes(project_id)) |>
    dplyr::select(project_score, community_served, project_id, project_description, project_cost, list, expecting_funding)
  
  
  # NOTE: EC projects included on General list, don't require separate list, but file kept for posterity
  wi_gen_ec_fundable <- fread("year3/WI/data/wi-sfy25-general-fundable-list.csv",
                            colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY25 General Fundable List",
      project_type = case_when(
        program == "BASE" ~ "General",
        program == "EC" ~ "Emerging Contaminants",
        grepl("lsl|lead", project_description, ignore.case=T) ~ "Lead",
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
      principal_forgiveness = as.character(principal_forgiveness),
      expecting_funding = "Yes",
      disadvantaged = case_when(
          as.numeric(total_pf_points) >= 60 ~ "Yes",
          TRUE ~ "No")
    ) |>
    dplyr::select(project_id, project_score, community_served, project_description, population, project_type, project_cost, requested_amount, 
           funding_amount, principal_forgiveness, disadvantaged, list, expecting_funding)
  
  
  wi_lsl_fundable <- fread("year3/WI/data/wi-sfy25-lsl-fundable-list.csv",
                           colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY25 LSL Fundable List",
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
    dplyr::select(project_id, community_served, project_score, project_type, project_cost, requested_amount, funding_amount, principal_forgiveness, disadvantaged, expecting_funding, list)
  
  
  wi_fundable <- bind_rows(wi_gen_ec_fundable, wi_lsl_fundable)
  
  # merge funding list onto comp list, keeping projects only on comp funding list
  wi_all <- merge(wi_comp_all, wi_fundable, by="project_id", all=TRUE) |>
    dplyr::mutate(
      project_description = ifelse(!is.na(project_description.y), project_description.y, project_description.x),
      project_cost = coalesce(project_cost.y, project_cost.x),
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
      state_fiscal_year = "2025"
    ) |>    
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######

# Hone in on project id duplication
#wi_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision: Duplicate included in original Comp PDF, removed with unique call above

# Check for disinfection byproduct in description
#wi_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
####### Decision: No disinfection byproduct string

# # Check for lead subtypes
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

# Check for lead subtypes: Unknown
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

  ### Decision: 87 lead unknown --> cannot resolve
  

####### SANITY CHECKS END #######
  
  run_tests(wi_clean)
  return(wi_clean)
}


