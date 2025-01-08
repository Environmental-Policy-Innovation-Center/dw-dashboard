source("resources.R")

clean_mi_y2 <- function() {
  
  base_path <- file.path("year2", "MI", "data")
  
  # Read the CSV file
  mi_raw <- fread(file.path(base_path, "22-y2-Michigan_PPL.csv"),
                  colClasses = "character", na.strings = "")
  
  mi_clean <- mi_raw %>%
    clean_names() %>%
    filter(!is.na(project_number)) %>%
    rowwise() %>%
    mutate(
      population = clean_numeric_string(population),
      funding_amount = sum(
        as.numeric(gsub("\\$|,", "", dwsrf_traditional_loan)),
        as.numeric(gsub("\\$|,", "", dwsrf_traditional_pf)),
        as.numeric(gsub("\\$|,", "", bil_dwsrf_supplemental_loan)),
        as.numeric(gsub("\\$|,", "", bil_dwsrf_supplemental_pf)),
        as.numeric(gsub("\\$|,", "", bil_dwsrf_emerging_contaminants_pf)),
        as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_loan)),
        as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_pf)),
        na.rm = TRUE
      ),
      principal_forgiveness = sum(
        as.numeric(gsub("\\$|,", "", dwsrf_traditional_pf)),
        as.numeric(gsub("\\$|,", "", bil_dwsrf_supplemental_pf)),
        as.numeric(gsub("\\$|,", "", bil_dwsrf_emerging_contaminants_pf)),
        as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_pf)),
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    rename(
      borrower = applicant_name,
      community_served = county,
      project_description = project_description,
      project_cost = total_project_costs,
      project_score = total_priority_points
    ) %>%
    mutate(
      funding_amount = clean_numeric_string(funding_amount),
      funding_amount = if_else(funding_amount == 0, "No Information", as.character(funding_amount)),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      principal_forgiveness = if_else(principal_forgiveness == 0, "No Information", as.character(principal_forgiveness)),
      project_type = case_when(
        as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_loan)) > 0 | as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_pf)) > 0 ~ "Lead",
        as.numeric(gsub("\\$|,", "", bil_dwsrf_emerging_contaminants_pf)) > 0 ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      funding_amount_numeric = suppressWarnings(as.numeric(funding_amount)),
      expecting_funding = case_when(
        is.na(funding_amount_numeric) ~ "No",
        funding_amount_numeric > 0 ~ "Yes",
        TRUE ~ "No"
      ),
      project_id = str_squish(project_number),
      project_cost = clean_numeric_string(project_cost),
      disadvantaged = case_when(
        disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") ~ "Yes",
        disadvantaged_status == "" | is.na(disadvantaged_status) ~ "No",
        TRUE ~ "No"
      ),
      state = "Michigan",
      state_fiscal_year = "2024",
      pwsid = as.character(NA),
      project_name = as.character(NA),
      requested_amount = as.character(NA),
      project_rank = as.character(NA)
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(mi_clean)
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}

