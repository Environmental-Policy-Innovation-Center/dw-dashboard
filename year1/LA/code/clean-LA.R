clean_la_y1 <- function() {
  
  # (39,7)
  la_srf_ppl <- fread("year1/LA/data/18-Lousiana_Comprehensive_PPL-clean.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      est_loan_amount = clean_numeric_string(est_loan_amount),
      population = clean_numeric_string(population),
      list = "SFY23 Base Comprehensive List") %>%
    select(-est_date_to_close_loan) %>%
    filter(system_name != "Total")
  
  # (7,10)
  la_srf_fnd <- fread("year1/LA/data/18-Lousiana_Fundable_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      expecting_funding = "Yes",
      est_loan_amount = clean_numeric_string(est_loan_amount),
      additonal_subsidy_amount = clean_numeric_string(additonal_subsidy_amount),
      disadvantaged_subsidy = clean_numeric_string(disadvantaged_subsidy),
      population = clean_numeric_string(population),
      list = "SFY23 Base Fundable List"
    ) %>%
    select(-est_date_to_close_loan) %>%
    filter(system_name != "Total")
  
  # (39,10)
  la_srf_combined <- merge(la_srf_ppl, la_srf_fnd, all=TRUE,
                           by=c("system_name", "pwsid", "est_loan_amount",
                                "points", "rank", "population", "project_description")) |>
    dplyr::mutate(
      list = dplyr::coalesce(list.y, list.x)
    ) |>
    dplyr::select(-c("list.y", "list.x"))
  
  la_srf_clean <- la_srf_combined %>%
    # process numeric columns
    mutate(requested_amount = clean_numeric_string(est_loan_amount),
           population = clean_numeric_string(population),
           # replace 0s specifically for adding columns together
           disadvantaged_subsidy = convert_to_numeric(disadvantaged_subsidy, TRUE),
           additional_subsidy_amount = convert_to_numeric(additonal_subsidy_amount, TRUE),
           principal_forgiveness = disadvantaged_subsidy + additional_subsidy_amount,
           principal_forgiveness = case_when(
             principal_forgiveness == 0 ~ "No Information",
             TRUE ~ clean_numeric_string(principal_forgiveness)
           )
    ) %>%
    # process text columns
    mutate(borrower = str_squish(system_name),
           pwsid = paste0("LA", pwsid),
           project_score = str_replace_all(points, "[^0-9.]", ""),
           project_rank = str_replace_all(rank, "[^0-9.]", ""),
           project_description = str_squish(project_description),
           disadvantaged = case_when(
             disadvantaged_subsidy > 0 ~ "Yes",
             TRUE ~ "No Information"),
           project_type = case_when(
             # search for keywords from full PPL, otherwise General project
             grepl("lead|lsl", project_description, ignore.case = TRUE) ~ "Lead",
             grepl(ec_str, project_description) ~ "Emerging Contaminants",
             TRUE ~ "General"))

  
  ## IIJA Docs
  ### Note: For SFY23, they released partial amendments for both applicant and fundable lists,
  ### but they do not entirely replace the original tables, so the two are merged together.
  ### If duplicate or slightly different information existed in both tables,
  ### the amendment details are kept. For lead, there is no fundable list.
  ### Numbers in the variable names represent pages from documents for reference.
  
  ### Applicant Lists
  la_base_app_26 <- fread("year1/LA/data/comprehensive-list-base-supp-26-27.csv",
        colClasses = "character", na.strings = "") %>%
    clean_names()
  
  la_base_app_34 <- fread("year1/LA/data/comprehensive-list-base-supp-34-35.csv",
                          colClasses = "character", na.strings = "") %>%
    clean_names()
  
  la_base_app <- merge(la_base_app_26, la_base_app_34, all=TRUE,
                       by=c("system_name", "pwsid", "est_loan_amount", "points",
                            "population", "project_description", "est_date_to_close_loan")) %>%
    mutate(
      rank = ifelse(is.na(rank.y), rank.x, rank.y),
      project_type = case_when(
      # search for keywords from full PPL, otherwise General project
      grepl("lsl|lead", project_description, ignore.case = TRUE) ~ "Lead",
      grepl(ec_str, project_description) ~ "Emerging Contaminants",
      TRUE ~ "General"),
      ) %>%
    select(-rank.x, -rank.y) |>
    dplyr::mutate(
      list = "SFY23 IIJA Gen Supp Comprehensive Lists"
    )
  
  
  ### Fundable Lists
  la_base_fundable_28 <- fread("year1/LA/data/fundable-list-base-supp-28.csv",
                            colClasses = "character", na.strings = "") %>%
    clean_names()
  
  la_base_fundable_36 <- fread("year1/LA/data/fundable-list-base-supp-36.csv",
                               colClasses = "character", na.strings = "") %>%
    clean_names()
  
  la_base_fund <- bind_rows(la_base_fundable_28, la_base_fundable_36) %>%
    # turns out they are the same, but maintaining code should future amendments modify one
    distinct() %>%
    mutate(expecting_funding = "Yes",
           disadvantaged = ifelse(!is.na(disadvantaged_subsidy), "Yes", "No Information"),
           principal_forgiveness_amount = clean_numeric_string(disadvantaged_subsidy)) %>%
    select(system_name, est_loan_amount, expecting_funding, disadvantaged, principal_forgiveness_amount) |>
    dplyr::mutate(
      list = "SFY23 IIJA Gen Supp Fundable Lists"
    )
  
  ### Combined
  la_base <- la_base_app %>%
    left_join(la_base_fund, by=c("system_name", "est_loan_amount")) %>%
    select(-est_date_to_close_loan) |>
    dplyr::mutate(
      list = dplyr::coalesce(list.y, list.x)
    ) |>
    dplyr::select(-c("list.y", "list.x"))
  
  
  ## EC Docs
  ### Applicant Lists
  la_ec_app_29<- fread("year1/LA/data/comprehensive-list-ec-29-30.csv",
                          colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-est_date_to_close_loan, -project_description)
  
  la_ec_app_32 <- fread("year1/LA/data/comprehensive-list-ec-32-33.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-est_date_to_close_loan)
  
  la_ec_app <- merge(la_ec_app_29, la_ec_app_32, all=TRUE,
                     by=c("system_name", "pwsid", "est_loan_amount", "points",
                          "population", "rank")) %>%
    mutate(project_type = "Emerging Contaminants") %>%
    distinct() |>
    dplyr::mutate(
      list = "SFY23 EC Comprehensive Lists"
    )

  
  ### Fundable Lists
  la_ec_fundable_31 <- fread("year1/LA/data/fundable-list-ec-31.csv",
                               colClasses = "character", na.strings = "") %>%
    clean_names()
  
  la_ec_fundable_37 <- fread("year1/LA/data/fundable-list-ec-37.csv",
                               colClasses = "character", na.strings = "") %>%
    clean_names()
  
  la_ec_fund <- bind_rows(la_ec_fundable_31, la_ec_fundable_37) %>%
    distinct() %>%
    mutate(
           expecting_funding = "Yes",
           disadvantaged = ifelse(!is.na(disadvantaged_assistance), "Yes", "No Information"),
           principal_forgiveness_amount = as.character(convert_to_numeric(disadvantaged_assistance, TRUE))) %>%
    select(system_name, est_loan_amount, expecting_funding, principal_forgiveness_amount, disadvantaged) |>
    dplyr::mutate(
      list = "SFY23 EC Fundable Lists"
    )
  
  la_ec <- la_ec_app %>%
    left_join(la_ec_fund, by=c("system_name", "est_loan_amount")) %>%
    distinct()  |>
    dplyr::mutate(
      list = dplyr::coalesce(list.y, list.x)
    ) |>
    dplyr::select(-c("list.y", "list.x"))
  
  ## Lead Doc
  la_lead_app <- fread("year1/LA/data/comprehensive-list-lead-38.csv",
                        colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead",
           expecting_funding = "No Information",
           points = clean_numeric_string(points)) %>%
    select(-est_date_to_close_loan) |>
    dplyr::mutate(
      list = "SFY23 LSLR Comprehensive List"
    )
  
  ## Merge IIJA Tables
  all_iija <- bind_rows(la_base, la_ec, la_lead_app) %>%
    mutate(
      pwsid = paste0("LA", pwsid)) %>%
    rename(
      borrower = system_name,
      requested_amount = est_loan_amount,
      project_rank = rank,
      project_score = points,
      principal_forgiveness = principal_forgiveness_amount
    
    )
  
  ## Merge SRF Table
  la_clean <- merge(la_srf_clean, all_iija, by=c("borrower", "requested_amount"), all=TRUE) %>%
    mutate(
      pwsid = ifelse(is.na(pwsid.y), pwsid.x, pwsid.y),
      project_type = ifelse(is.na(project_type.y), project_type.x, project_type.y),
      population = ifelse(is.na(population.y), population.x, population.y),
      project_description = ifelse(is.na(project_description.y), project_description.x, project_description.y),
      disadvantaged = ifelse(is.na(disadvantaged.y), disadvantaged.x, disadvantaged.y),
      expecting_funding = ifelse(is.na(expecting_funding.y), expecting_funding.x, expecting_funding.y),
      project_rank = ifelse(is.na(project_rank.y), project_rank.x, project_rank.y),
      project_score = ifelse(is.na(project_score.y), project_score.x, project_score.y),
      community_served = as.character(NA),
      project_id = as.character(NA),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      funding_amount = as.character(NA),
      disadvantaged = replace_na(disadvantaged, "No Information"),
      expecting_funding = replace_na(expecting_funding, "No"),
      principal_forgiveness = dplyr::coalesce(principal_forgiveness.y, principal_forgiveness.x),
      principal_forgiveness = tidyr::replace_na(principal_forgiveness, "No Information"),
      principal_forgiveness = ifelse(principal_forgiveness == "0", "No Information", principal_forgiveness),
      state = "Louisiana",
      state_fiscal_year = "2023",
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