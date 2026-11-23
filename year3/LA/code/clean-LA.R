clean_la_y3 <- function() {

  base_app <- fread("year3/LA/data/base-applicant.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() |>
    dplyr::mutate(list = "SFY25 Base Comprehensive List")
  
  
  base_fund <- fread("year3/LA/data/base-fundable.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      expecting_funding = "Yes",
      principal_forgiveness = clean_numeric_string(amount_of_principal_forgiveness) #real 0s
      ) %>%
    select(dwrlf_project_number, expecting_funding, principal_forgiveness) |>
    dplyr::mutate(list =  "SFY25 Base Fundable List")
  
  base_all <- base_app %>%
    left_join(base_fund, by="dwrlf_project_number") %>%
    dplyr::mutate(list = dplyr::coalesce(list.y, list.x)) |>
    dplyr::rename(meets_affordability_criteria = meets_affordability_criteria_dac) |>
    select(water_system_name, dwrlf_project_number, system_population, est_loan_amount,
           potential_funding_source_s, amount_of_principal_forgiveness, priority_points,
           project_description, type_of_principal_forgiveness_received, meets_affordability_criteria,
           iija_project, expecting_funding, principal_forgiveness, list)
  


  # 2025 combines gen, lead, and ec applicants into a single list
  # whereas fundable lists are separate by fed cap grant
  bil_comp <- fread("year3/LA/data/comprehensive-bil-list.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      est_loan_amount = clean_numeric_string(est_loan_amount),
      list = "SFY25 IIJA Gen Supp/LSLR/EC Comprehensive List"
    ) 
  
  gs_fundable <- fread("year3/LA/data/fundable-gen-supp.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      expecting_funding = "Yes",
      list = "SFY25 IIJA Gen Supp Fundable List"
    )
  
  ec_fundable <- fread("year3/LA/data/fundable-ec.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      project_type = "Emerging Contaminants",
      expecting_funding = "Yes",
      list = "SFY25 EC Fundable List"
    )
  
  lead_fundable <- fread("year3/LA/data/fundable-lead.csv",
                         colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      project_type = "Lead",
      expecting_funding = "Yes",
      list = "SFY25 LSLR Fundable List"
    )
  
  # combine and simplify fundable lists
  fundable <- bind_rows(gs_fundable, ec_fundable, lead_fundable) %>%
    mutate(principal_forgiveness = clean_numeric_string(amount_of_principal_forgiveness),
           est_loan_amount = clean_numeric_string(est_loan_amount)) %>%
    select(dwrlf_project_number, est_loan_amount, expecting_funding, project_type, principal_forgiveness, list)
  
  # join comprehensive applicant list with fundable list
  bil_all <- bil_comp %>%
    left_join(fundable, by=c("dwrlf_project_number", "est_loan_amount")) %>%
    dplyr::mutate(list = dplyr::coalesce(list.y, list.x)) |>
    select(-potential_funding_source_s, -expected_loan_term, -est_date_to_close_loan, -readiness_to_proceed,
           -compliance_correction, -arpa_wsp_match_project, -preliminary_water_system_grade)
  
  # bil_all |>
  #   dplyr::group_by(dwrlf_project_number) |>
  #   dplyr::tally() |>
  #   dplyr::filter(n>1)

  # project number 1071009-02-LSLR duplicated, with diff $ amounts. Keep as separate.
  
  # combine and clean base and bil lists
  la_clean <- bind_rows(base_all, bil_all) %>% 
    mutate(
      community_served = as.character(NA),
      borrower = str_squish(water_system_name),
      # extract pwsid from project number
      pwsid = paste0("LA",str_sub(dwrlf_project_number, start=1, end=7)),
      project_id = str_squish(dwrlf_project_number),
      project_id = stringr::str_remove(project_id, " "),
      project_name = as.character(NA),
      project_type = case_when(
        # use type if already defined
        !is.na(project_type) ~ project_type,
        # define type by source of IIJA funding considered
        grepl("IIJA-EC", iija_project) ~ "Emerging Contaminants",
        grepl("IIJA-LSL", iija_project) ~ "Lead",
        # search for keywords
        grepl("lead|lsl", project_description, ignore.case = TRUE) ~ "Lead",
        grepl(ec_str, project_description) ~ "Emerging Contaminants",
        # all else is general
        TRUE ~ "General"),
      project_cost = as.character(NA),
      requested_amount = clean_numeric_string(est_loan_amount),
      funding_amount = as.character(NA),
      principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
      project_description = str_squish(project_description),
      population = clean_numeric_string(system_population),
      disadvantaged = ifelse(grepl("Yes", meets_affordability_criteria), "Yes", "No"),
      project_rank = as.character(NA),
      project_score = clean_numeric_string(priority_points),
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Louisiana",
      state_fiscal_year = "2025") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)

   
  ####### SANITY CHECKS START #######

# Hone in on project id duplication
# la_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::filter(counts>1)
# duplicated_ids <- la_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::filter(counts>1) |> dplyr::pull(project_id)
  
  # la_clean |>
  #   dplyr::filter(project_id %in% duplicated_ids) |> View()

la_clean <- la_clean |>
  dplyr::mutate(
    drop = dplyr::case_when(
      project_id == "1015004-03-GS" & list == "SFY25 Base Comprehensive List" ~ "Yes", #default to fundable
      project_id == "1019044-01-GS" & list == "SFY25 Base Fundable List" ~ "Yes", #default to IIJA
      # project_id == "1019051-01-GS" & list == "" ~ "Yes", # diff requested amounts kept as ambiguous
      project_id == "1019084-02-GS" & list == "SFY25 Base Fundable List" ~ "Yes", #default to IIJA
      project_id == "1029007-01-GS" & list == "SFY25 Base Comprehensive List" ~ "Yes", #default to IIJA
      project_id == "1039010-05-GS" & list == "SFY25 Base Comprehensive List" ~ "Yes", #default to IIJA
      project_id == "1067003-02-GS" & list == "SFY25 Base Comprehensive List" ~ "Yes", #default to IIJA
      project_id == "1097012-01-GS" & list == "SFY25 Base Comprehensive List" ~ "Yes", #default to IIJA
      project_id == "1113032-01-GS" & list == "SFY25 Base Comprehensive List" ~ "Yes", #default to IIJA
      .default = "No"
    )
  ) |>
  dplyr::filter(!drop == "Yes") |>
  dplyr::select(-drop)



# Check for disinfection byproduct in description
# la_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
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

  ####### Decision: 1071009-03-LSLR should be LSLR only
  
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
  
  ######## Decision: 
# "1019044-01-GS" --> both
# "1073047-02-GS" --> lslr
# "1021002-01-GS") --> lsli

  la_clean <- la_clean |>
    dplyr::mutate(
      project_description = dplyr::case_when(
        project_type == "Lead" & project_id == "1021002-01-GS" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_id == "1073047-02-GS" ~ paste0(project_description, " | FT: LSLR"),
        project_type == "Lead" & project_id == "1019044-01-GS" ~ paste0(project_description, " | FT: LSLI , LSLR"),
        .default = project_description
      )
    )
  
####### SANITY CHECKS END #######
  
  run_tests(la_clean)
  rm(list=setdiff(ls(), "la_clean"))
  
  return(la_clean)
}