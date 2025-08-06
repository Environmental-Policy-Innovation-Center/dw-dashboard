clean_al_y0 <- function() {

  al_iup <- data.table::fread("year0/AL/data/al-fy21-iup.csv",
                   colClasses = "character", na.strings = "") |>
    clean_names() |>
    dplyr::mutate(source_file = "Base")

  al_clean <- al_iup |>
    mutate(
      community_served = str_squish(county_served),
      borrower = str_squish(applicant_name),
      pwsid = as.character(NA),
      project_id = as.character(NA),
      project_name = str_squish(project_name),
      project_type = case_when(
        grepl(lead_str, project_description) ~ "Lead",
        grepl(ec_str, project_description) ~ "Emerging Contaminants",
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
           project_score, expecting_funding, state, state_fiscal_year, source_file)
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
al_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision : No duplicates

# Check for disinfection byproduct in description
al_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision : No disinfection byproduct string

####### SANITY CHECKS END #######
  
  al_clean <- al_clean |>
    dplyr::select(-source_file)
  
  run_tests(al_clean)
  rm(list=setdiff(ls(), "al_clean"))
  
  return(al_clean)
}

