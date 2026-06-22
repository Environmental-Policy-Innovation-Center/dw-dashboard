clean_pa_y5 <- function() {
  
  ### Fundable lists ----
  pa_fundable_ec <- data.table::fread("year5/PA/data/pa-sfy26-ec-fundable-list.csv",
                                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      project_type = "Emerging Contaminants",
      list = "SFY26 Fundable EC"
    ) |>
    rename(loan = loan_2)
  
  pa_fundable_gs <- data.table::fread("year5/PA/data/pa-sfy26-supp-fundable-list.csv",
                                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      list = "SFY26 Fundable Gen Supp"
    )
  
  pa_fundable_base <- data.table::fread("year5/PA/data/pa-sfy26-base-fundable-list.csv",
                                        colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      list = "SFY26 Fundable Base"
    )
  
  pa_fundable <- dplyr::bind_rows(pa_fundable_ec, pa_fundable_base, pa_fundable_gs) |>
    select(loan_number, project_name, total_assistance_amount, loan, principal_forgiveness, expecting_funding, project_type, list)
  
  ### Comprehensive list ----
  pa_comp <- data.table::fread("year5/PA/data/pa-sfy26-comprehensive-list.csv",
                               colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      pwsid = paste0("PA", trimws(pwsid)),
      list = "SFY26 Comprehensive",
      applicant_name = stringr::str_trim(applicant_name),
      applicant_name = stringr::str_squish(stringr::str_replace_all(applicant_name, "[\u2013\u2014]", "-")),
      applicant_name = stringr::str_replace_all(applicant_name, "(?<![\\s])PROJECT", " PROJECT"),
      applicant_name = stringr::str_replace_all(applicant_name, "(?<![\\s])PUBLIC", " PUBLIC"),
      applicant_name = ifelse(grepl("ERIE", applicant_name), stringr::str_remove(applicant_name, "-(?=[^-]*$)"), applicant_name),
      applicant_name = stringr::str_replace_all(applicant_name, "GALVANIZED LINE REPLACEMENT|GALVANIZED LINEREPLACEMENT", "GALVANIZED SERVICE LINE REPLACEMENT"),
      applicant_name = ifelse(grepl("WILKINSBURG", applicant_name), stringr::str_replace(applicant_name, "REPLACEMENTS", "REPLACEMENT"), applicant_name),
      applicant_name = stringr::str_squish(applicant_name)
    )
  
  ### Clean  ----
  pa_clean <- pa_comp |> 
    dplyr::full_join(pa_fundable, by = c("loan_number")) |>
    dplyr::mutate(
      list = ifelse(is.na(list.y), list.x, list.y),
      applicant_name = stringr::str_replace_all(applicant_name, "[\u2013\u2014]", "-"),
      community_served = paste0(stringr::str_to_title(city), ", ", stringr::str_to_title(county), " County"), 
      applicant = stringr::str_to_title(applicant_name),
      # [keep] borrower is text before dash
      borrower = trimws(stringr::str_extract(applicant, "^[^\\-\u2013\u2014]+")),
      # [keep] projects in fundable list are crosswalked to comprehensive, and we default the pwsid to the comprehensive list
      pwsid = stringr::str_squish(pwsid),
      project_id = clean_numeric_string(loan_number), 
      # [keep] project name is text after dash, we default o name on comprehensive
      project_name = trimws(stringr::str_extract(applicant, "(?<=-).*")), 
      project_name = dplyr::case_when(
        project_id == "82261" ~ "2025 SDWMR (NON-LEAD)",
        project_id == "80304" ~ "CROYLE WATER LINE PROJECT CONTRACT 2025-W-01",
        is.na(project_name) ~ borrower, 
        .default = project_name
      ),
      # [keep] Project description and Problem description variables are only present in the comprehensive list and we default to these values as all fundable projets are in comprehensive lists
      project_description = paste0("Project Description: ", proj_description, "; Problem Description: ", prob_description),
      project_type = dplyr::case_when(
        !is.na(project_type.y) ~ project_type.y, 
        grepl("lead|lsl", project_description, ignore.case = T) ~ "Lead", 
        (list %in% c("SFY26 Fundable Base", "SFY26 Fundable Gen Supp") & grepl("lead|lsl", project_name, ignore.case = T)) ~ "Lead", 
        grepl(ec_str, project_description, ignore.case = T) ~ "Emerging Contaminants", 
        (list %in% c("SFY26 Fundable Base", "SFY26 Fundable Gen Supp") & grepl(ec_str, project_name, ignore.case = T)) ~ "Emerging Contaminants", 
        TRUE ~ "General"), 
      project_cost = clean_numeric_string(project_cost),
      requested_amount = as.character(NA), 
      funding_amount = clean_numeric_string(total_assistance_amount), 
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      population = clean_numeric_string(population),  
      disadvantaged = as.character(NA),
      project_rank = clean_numeric_string(projrank), 
      # [keep] PV rating is a numeric feature,  clean_numeric_string will coerce any missing values to "No Information", and set the column to character type
      project_score = clean_numeric_string(pv_rating),
      # [keep] there are no projects id by PENNVEST and DEP as future projects
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Pennsylvania",
      state_fiscal_year = "2026") |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
                  requested_amount, funding_amount, principal_forgiveness, population, project_description,
                  disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)

  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  # pa_clean |> dplyr::group_by(project_id) |> dplyr::tally() |>dplyr::arrange(dplyr::desc(n))
  ####### Decision: No duplicates from funding lists
  
  # Check for disinfection byproduct in description
  # pa_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
  
  # Check for lead subtypes: Both
  #  pa_clean |>
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
  
  ####### Decision: 4 lead projects classified as both
  
  # Check for lead subtypes: Unknown
#  pa_clean |>
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
#     dplyr::filter(lead_type == "unknown")
  
  # one unknown --> actually a General project

  pa_clean <- pa_clean |>
    dplyr::mutate(
      project_type = dplyr::case_when(
        project_id == "82256" ~ "General",
        project_id == "82258" ~ "General",
        project_id == "80293" ~ "General",
        .default = project_type
    ))
  
  ####### SANITY CHECKS END #######
  
  run_tests(pa_clean)
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}