clean_tn_y0 <- function() {
  
  # there were a few new additions to the EC string on June 12th 2025 to include
  # various new strings 
  ec_str_tn <- "cyanotoxin|dioxane|emerging contaminant|lithium|manganese|Mn|Perfluoro-n-pentanoic acid|PFPeA|PFAS|PFOA|PFOS|trihalomethane|THM|Unregulated Contaminant Monitoring Rule|DBP|disinfection byproduct|HAA5|haloacetic acid"
  
  # (73, 17) - one of these is a total column that needs to be removed
  tn_ppl <- read.csv("year0/TN/data/tn-srf-dw-fy2021-priority-ranking-list.csv") %>%
    clean_names()
  
  tn_clean <- tn_ppl %>%
    filter(local_government != "") %>%
    mutate(
      community_served = str_squish(county),
      borrower = str_squish(local_government),
      borrower = str_replace_all(borrower, "\\*", ""),
      borrower = str_replace_all(borrower, "\\+", ""),
      pwsid = str_squish(pwsid),
      project_cost = clean_numeric_string(total_project_amount),
      project_description = str_squish(project_description),
      population = clean_numeric_string(pop),
      project_rank = str_squish(rank_order),
      project_score = str_squish(priority_points),
      expecting_funding = case_when(rank_order <= 30 ~ "Yes", 
                                    TRUE ~ "No"),
      project_id = as.character(NA),
      project_name = as.character(NA),
      project_description = str_squish(project_description), 
           project_type = case_when(
             grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"), 
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      disadvantaged = case_when(atpi > 50 ~ "No", 
                                TRUE ~ "Yes"),
      state = "Tennessee",
      state_fiscal_year = "2022",
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  tn_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: Disinfection byproduct string
  
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
