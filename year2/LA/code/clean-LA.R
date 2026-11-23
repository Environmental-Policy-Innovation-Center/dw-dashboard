clean_la_y2 <- function() {
  
  ## Base Docs
  base_app <- fread("year2/LA/data/base-applicant.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-est_date_to_close_loan) |>
    dplyr::mutate(
      list = "SFY24 Base Comprehensive List"
    )
  
  base_fund <- fread("year2/LA/data/base-fundable.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(system_name, est_loan_amount, expecting_funding) |>
    dplyr::mutate(
      list = "SFY24 Base Fundable List"
    )
  
  base_all <- base_app %>%
    left_join(base_fund, by=c("system_name", "est_loan_amount")) |>
    dplyr::mutate(
      list = dplyr::coalesce(list.y, list.x)
    ) |>
    dplyr::select(-c("list.y", "list.x"))
  
  ## BIL Docs
  
  ### Gen Supp
  
  gs_app <- fread("year2/LA/data/gs-applicant.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-est_date_to_close_loan) |>
    dplyr::mutate(
      list = "SFY24 IIJA Gen Supp Comprehensive List"
    )
  
  gs_fund <- fread("year2/LA/data/gs-fundable.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(system_name, est_loan_amount, expecting_funding) |>
    dplyr::mutate(
      list = "SFY24 IIJA Gen Supp Fundable List"
    )
  
  gs_all <- gs_app %>%
    left_join(gs_fund, by=c("system_name", "est_loan_amount")) |>
    dplyr::mutate(
      list = dplyr::coalesce(list.y, list.x)
    ) |>
    dplyr::select(-c("list.y", "list.x"))
  
  ### EC 
  
  ec_app <- fread("year2/LA/data/ec-applicant.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-est_date_to_close_loan) |>
    dplyr::mutate(
      list = "SFY24 EC Comprehensive List"
    )
  
  ec_fund <- fread("year2/LA/data/ec-fundable.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(system_name, est_loan_amount, expecting_funding) |>
    dplyr::mutate(
      list = "SFY24 EC Fundable List"
    )
  
  ec_all <- ec_app %>%
    left_join(ec_fund, by=c("system_name", "est_loan_amount")) %>%
    mutate(
      project_type = "Emerging Contaminants",
      list = dplyr::coalesce(list.y, list.x)
    ) |>
    dplyr::select(-c("list.y", "list.x"))
  
  ### Lead
  
  lead_app <- fread("year2/LA/data/lead-applicant.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-est_date_to_close_loan) %>%
    # if there is a future amendment, check projects on this list against funding lists to determine if this should change
    mutate(
      expecting_funding = "No Information",
      project_type = "Lead",
      list = "SFY24 LSLR Comprehensive List"
    )
  
  # combine bil
  bil_all <- bind_rows(gs_all, ec_all, lead_app)
  
  # almost all projects on bil list overlap with base except one, unlike 2025
  all_match <- merge(base_all, bil_all, all=TRUE, by=c("system_name", "est_loan_amount", "pwsid")) %>%
    mutate(
      points = dplyr::coalesce(points.y, points.x),
      rank = dplyr::coalesce(rank.y, rank.x),
      list = dplyr::coalesce(list.y, list.x),
      rank = ifelse(grepl("EC", list), "No Information", rank),
      population = dplyr::coalesce(population.y, population.x),
      project_description = dplyr::coalesce(project_description.y, project_description.x),
      expecting_funding = dplyr::coalesce(expecting_funding.y, expecting_funding.x)
    ) %>%
    select(system_name, est_loan_amount, pwsid, points, rank, population, project_description, expecting_funding, project_type, list)
  
  la_clean <- all_match %>%
    mutate(community_served = as.character(NA),
           borrower = str_squish(system_name),
           pwsid = paste0("LA", pwsid),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_type = case_when(
             !is.na(project_type) ~ project_type,
             grepl("lead|lsl", project_description, ignore.case = TRUE) ~ "Lead",
             grepl(ec_str, project_description) ~ "Emerging Contaminants",
             # all else is general
             TRUE ~ "General"),
           project_cost = as.character(NA),
           requested_amount = clean_numeric_string(est_loan_amount),
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           population = clean_numeric_string(population),
           disadvantaged = as.character(NA),
           project_rank = clean_numeric_string(rank),
           project_score = clean_numeric_string(points),
           expecting_funding = replace_na(expecting_funding, "No"),
           state = "Louisiana",
           state_fiscal_year = "2024"
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

  la_clean <- la_clean |>
    dplyr::mutate(
      pwsid = dplyr::case_when(
        borrower == "City of Natchitoches" & pwsid == "LA106007" ~ "LA1069007",
        .default = pwsid
      )
    ) 
  
  ####### SANITY CHECKS END ####### 
  
  run_tests(la_clean)
  rm(list=setdiff(ls(), "la_clean"))
  
  return(la_clean)
}