clean_al_y4 <- function() {

  al_iup <- data.table::fread(
    "year4/AL/data/PPL_AL_FFY2025_base_raw_curated.csv",
    colClasses = "character", na.strings = "") |>
      janitor::clean_names() |>
    dplyr::mutate(source_file = "Base")

  al_clean <- al_iup |>
    mutate(
      community_served = str_squish(city_town),
      community_served = dplyr::case_when(
        is.na(community_served) ~ "No Information",
        .default = community_served
      ),
      borrower = str_squish(applicant_name),
      pwsid = as.character(NA),
      project_id = as.character(project_number),
      project_name = str_squish(attachment_project_name),
      project_type = case_when(
        grepl(lead_str, attachment_description) ~ "Lead",
        grepl(ec_str, attachment_description) ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      project_cost = as.character(NA),
      requested_amount = clean_numeric_string(applied_for_project_amount),
      funding_amount = as.character(NA),
      principal_forgiveness = 
        as.numeric(tidyr::replace_na(stringr::str_remove(string = dw_srf_percent_pf, "%"), "0"))*as.numeric(clean_numeric_string(applied_for_project_amount)),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      project_description = str_squish(attachment_description),
      population = clean_numeric_string(population),
      disadvantaged = dplyr::case_when(
        disadvantaged_score > 1 ~ "Yes",
        disadvantaged_score < 1 ~ "No"
      ),
      project_rank = as.character(NA),
      project_score = str_squish(priority_ranking_points)
    ) |>
    dplyr::mutate(
      project_score = dplyr::case_when(
        project_score == "SUPP" ~ "No Information",
        project_score == "NA" ~ "No Information",
        is.na(project_score) ~ "No Information",
        .default = project_score
      )
    ) |>
    dplyr::mutate(
      expecting_funding = "Yes",
      state = "Alabama",
      state_fiscal_year = "2026"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year, source_file)
  
####### SANITY CHECKS #######
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

