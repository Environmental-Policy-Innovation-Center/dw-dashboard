clean_al_y0 <- function() {

  al_iup <- data.table::fread("year0/AL/data/al-fy21-iup.csv",
                   colClasses = "character", na.strings = "") |>
    clean_names() |>
    dplyr::mutate(list = "SFY22 Base PPL")

  al_clean <- al_iup |>
    mutate(
      community_served = str_squish(county_served),
      borrower = str_squish(applicant_name),
      pwsid = as.character(NA),
      project_id = as.character(NA),
      project_name = str_squish(project_name),
      project_type = case_when(
        grepl("lead|lsl", project_description, ignore.case = TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      project_cost = as.character(NA),
      requested_amount = as.character(NA),
      funding_amount = clean_numeric_string(assistance_amount),
      principal_forgiveness = clean_numeric_string(additional_subsidization_principal_forgiveness),
      project_description = str_squish(project_description),
      population = clean_numeric_string(population_served),
      disadvantaged = as.character(NA),
      project_rank = as.character(NA),
      project_score = str_squish(priority_point_rank),
      expecting_funding = "Yes",
      state = "Alabama",
      state_fiscal_year = "2022"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year, list)
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
# al_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision : No project ids

# Check for disinfection byproduct in description
#al_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision : No disinfection byproduct string

 # Check for lead subtypes: Both
  # al_clean |>
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
  # al_clean |>
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

  ### No lead unknown
   
  
####### SANITY CHECKS END #######
  
  run_tests(al_clean)
  rm(list=setdiff(ls(), "al_clean"))
  
  return(al_clean)
}

