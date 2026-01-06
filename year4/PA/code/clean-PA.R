clean_pa_y4 <- function() {
  
  ### Fundable lists ----
  pa_fundable_ec <- data.table::fread("year4/PA/data/PA_Y4_ECChart1_Final.csv",
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
       expecting_funding = "Yes",
       project_type = "Emerging Contaminants"
    )
  
  pa_fundable_gs <- data.table::fread("year4/PA/data/PA_Y4_GenSupChart1_Final.csv",
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
       expecting_funding = "Yes"
    )
  
   pa_fundable_base <- data.table::fread("year4/PA/data/PA_Y4_BaseChart1_Final.csv",
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
       expecting_funding = "Yes"
    )
  
  pa_fundable <- dplyr::bind_rows(pa_fundable_ec, pa_fundable_base, pa_fundable_gs) |>
    dplyr::mutate(
       list = "fundable",
       applicant_name = stringr::str_to_upper(project_name),
       applicant_name = stringr::str_remove_all(applicant_name, "\n"),
       pwsid = paste0("PA", stringr::str_extract(applicant_name, "PWSID\\s+\\d+")),
       pwsid = stringr::str_remove(pwsid, "PWSID"),
       pwsid = stringr::str_remove(pwsid, " "),
       pwsid = stringr::str_remove(pwsid, "\n"),
       pwsid = ifelse(pwsid == "PANA", NA_character_, pwsid)
  )
  
  ### Comprehensive list ----
  pa_comp <- data.table::fread("year4/PA/data/PA_Y4_PPL_Final.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(pwsid = paste0("PA", trimws(pwsid)))
  
  ### Clean  ----
  pa_clean <- pa_comp |> 
    dplyr::full_join(pa_fundable, by = c("applicant_name")) |>
    dplyr::mutate(
       community_served = paste0(stringr::str_to_title(city), ", ", stringr::str_to_title(county), " County"), 
       applicant = stringr::str_to_title(applicant_name),
           ## borrower is text before dash
       borrower = trimws(stringr::str_extract(applicant, "[^-]+")),
       pwsid = pwsid.x,
       project_id = clean_numeric_string(loan_number), 
       ## project name is text after dash
       project_name = trimws(stringr::str_extract(applicant, "(?<=-).*")), 
       project_name = dplyr::case_when(
              is.na(project_name) ~ borrower, 
              .default = project_name),
       project_description = paste0("Project Description: ", proj_description, "; Problem Description: ", prob_description),
       project_type = dplyr::case_when(
              !is.na(project_type.y) ~ project_type.y, 
              grepl(lead_str, project_description, ignore.case = T) ~ "Lead", 
              grepl(ec_str, project_description, ignore.case = T) ~ "Emerging Contaminants", 
              TRUE ~ "General"), 
       project_cost = clean_numeric_string(project_cost),
       requested_amount = as.character(NA), 
       funding_amount = clean_numeric_string(total_assistance_amount), 
       principal_forgiveness = clean_numeric_string(principal_forgiveness),
       population = clean_numeric_string(population),  
       disadvantaged = "No Information",
       project_rank = clean_numeric_string(projrank), 
       project_score = clean_numeric_string(dep_project_rating),
       expecting_funding = replace_na(expecting_funding, "No"),
       state = "Pennsylvania",
       state_fiscal_year = "2026") |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
    
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  pa_clean |> dplyr::group_by(project_id) |> dplyr::tally()
  ####### Decision: No duplicates from funding lists
  
  # Check for disinfection byproduct in description
  pa_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
  
  # Check for lead subtypes: Both
  pa_clean |>
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

  ####### Decision: No lead projects classified as both
  
  # Check for lead subtypes: Unknown
  pa_clean |>
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
    dplyr::filter(lead_type == "unknown")

  # one unknown --> replacement

  ####### SANITY CHECKS END #######
  
  run_tests(pa_clean)
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}