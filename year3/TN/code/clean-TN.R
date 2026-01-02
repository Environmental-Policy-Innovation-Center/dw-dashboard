clean_tn_y3 <- function() {
  
  comprehensive_ppl <- fread("year3/TN/data/comprehensive_ppl.csv",
                   colClasses = "character", na.strings = "") |>
    clean_names() |>
    mutate(
           project_description = str_squish(project_description), 
           project_type = dplyr::case_when(
             grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           # these rows are blank for projects below the ranking line & not 
           # expecting funding
           project_rank = replace_na(rank_order, "No Information"), 
           project_score = replace_na(priority_points, "No Information"),
           expecting_funding = case_when(project_rank == "No Information" ~ "No", 
                                         TRUE ~ "Yes"))
  
  tn_clean <- comprehensive_ppl %>%
    mutate(community_served = str_squish(county),
           # removing weird characters from local government column
           borrower = str_replace_all(local_government, "\\*", ""),
           borrower = str_replace_all(borrower, "\\+", ""),
           borrower = str_squish(borrower),
           pwsid = str_squish(pwsid_number), 
           project_id = as.character(NA),
           project_name = as.character(NA), 
           project_cost = clean_numeric_string(total_project_amount), 
           requested_amount = as.character(NA), 
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_description = str_squish(project_description), 
           population = clean_numeric_string(pop_served), 
           disadvantaged = case_when(grepl("\\*", local_government) ~ "Yes",
                                     TRUE ~ "No"), 
           state = "Tennessee",
           state_fiscal_year = "2025")%>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  tn_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
  
  # Check for lead subtypes: Both
  tn_clean |>
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
  tn_clean |>
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

  ####### Decision: No lead projects classified as unknown

  ####### SANITY CHECKS END #######

  run_tests(tn_clean)
  rm(list=setdiff(ls(), "tn_clean"))
  
  return(tn_clean)
}
