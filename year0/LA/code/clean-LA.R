clean_la_y0 <- function() {
  
  # (37,8)
  la_comp <- fread("year0/LA/data/comprehensive-applicants-list.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      est_loan_amount = clean_numeric_string(est_loan_amount),
      population = clean_numeric_string(population),
      list = "SFY22 Base Comprehensive List"
    
    ) %>%
    select(-est_date_to_close_loan) %>%
    filter(system_name != "Total")
  
  # (18,10)
  la_fund <- fread("year0/LA/data/fundable-list.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      expecting_funding = "Yes",
      est_loan_amount = clean_numeric_string(est_loan_amount),
      # replace 0s specifically for adding columns together
      disadvantaged_subsidy_amount = convert_to_numeric(disadvantaged_subsidy_amount, TRUE),
      additional_subsidy_amount = convert_to_numeric(additional_subsidy_amount, TRUE),
      principal_forgiveness = as.character(disadvantaged_subsidy_amount + additional_subsidy_amount),
      population = clean_numeric_string(population),
      list = "SFY22 Base Fundable List"
    
    ) %>%
    # drop rank because it differs from the comprehensive list
    select(-est_date_to_close_loan, -rank, -additional_subsidy_amount) %>%
    filter(system_name != "Total")
  
  # (37,10) -> (37,18)
  la_clean <- merge(la_comp, la_fund, all=TRUE,
                           by=c("system_name", "pwsid", "est_loan_amount",
                                "points", "population", "project_description")) %>%
    # process numeric columns
    mutate(requested_amount = clean_numeric_string(est_loan_amount),
           population = clean_numeric_string(population),
           principal_forgiveness = case_when(
             is.na(principal_forgiveness) ~ "No Information",
            principal_forgiveness == "0" ~ "No Information",
             TRUE ~ principal_forgiveness),
             project_cost = as.character(NA),
             funding_amount = as.character(NA)
           ) %>%
    # process text columns
    mutate(borrower = str_squish(system_name),
           community_served = as.character(NA),
           pwsid = paste0("LA", pwsid),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_type = case_when(
             grepl(lead_str, project_description) ~ "Lead",
             grepl(ec_str, project_description) ~ "Emerging Contaminants",
             TRUE ~ "General"
           ),
           project_score = str_squish(points),
           project_rank = str_squish(rank),
           project_description = str_squish(project_description),
           disadvantaged = case_when(
             disadvantaged_subsidy_amount > 0 ~ "Yes",
             TRUE ~ "No Information"),
           expecting_funding = ifelse(is.na(expecting_funding), "No", "Yes"),
           state = "Louisiana",
           state_fiscal_year = "2022",
           list = dplyr::coalesce(list.y, list.x)
          ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
####### Decision : No project_id

# Check for disinfection byproduct in description
#la_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision : No disinfection byproduct string

# Check for lead subtypes: Both
# la_clean |>
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
#     dplyr::filter(lead_type == "both")

  ####### Decision: No type both
  
  # Check for lead subtypes: Unknown
  # la_clean |>
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
  
  ######## Decision: No type unknow
  
    # Check pwsid lengths
  # la_clean |>
  #   dplyr::mutate(
  #     length_pwsid = stringr::str_length(pwsid)
  #   ) |> 
  #   dplyr::filter(!length_pwsid == 9) |>
  #   dplyr::filter(!pwsid == "No Information")   

  ####### SANITY CHECKS END #######

  run_tests(la_clean)
  rm(list=setdiff(ls(), "la_clean"))
  
  return(la_clean)
}