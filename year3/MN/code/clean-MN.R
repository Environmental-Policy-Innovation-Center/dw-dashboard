clean_mn_y3 <- function() {
  
  mn_base_ec_fundable <- data.table::fread("year3/MN/data/mn_base_ec_fundable.csv",
                                           colClasses = "character", na.strings = "") |> 
    janitor::clean_names() %>%
    dplyr::filter(!is.na(project_number)) %>%
    dplyr::mutate(project_type = case_when(
      # set EC funded projects as EC, fill in remainder with text search downstream
      convert_to_numeric(estimated_dwrf_emerging_contaminant_pf_grant_not_final_1, TRUE) > 0 ~ "Emerging Contaminants",
      TRUE ~ as.character(NA)),
      estimated_dwrf = convert_to_numeric(estimated_dwrf_loan, TRUE),
      estimated_ec_pf_1 = convert_to_numeric(estimated_dwrf_emerging_contaminant_pf_grant_not_final_1, TRUE),
      estimated_dac_pf_2 = convert_to_numeric(estimated_dwrf_disadvantaged_community_pf_grant_not_final_2, TRUE),
      funding_amount = estimated_dwrf + estimated_ec_pf_1 + estimated_dac_pf_2,
      principal_forgiveness = estimated_ec_pf_1 + estimated_dac_pf_2,
      estimated_wif = convert_to_numeric(estimated_wif_grant_not_final_3, TRUE),
      disadvantaged = case_when(
        estimated_wif > 0 ~ "Yes",
        estimated_dac_pf_2 > 0 ~ "Yes",
        grepl("B", iup_status) ~ "No Information",
        # if not Part B and no DAC, known No
        TRUE ~ "No"),
      expecting_funding = "Yes",
      list = "SFY25 General & EC Fundable List"
    )
  
  mn_not_eligible <-  data.table::fread("year3/MN/data/mn_not_eligible.csv",
                                        colClasses = "character", na.strings = "") |> 
    janitor::clean_names() %>%
    dplyr::mutate(
      expecting_funding = "No",
      list = "SFY25 Not Eligible List",
    )
  
  mn_not_fundable <- data.table::fread("year3/MN/data/mn_not_fundable.csv",
                                       colClasses = "character", na.strings = "") |> 
    janitor::clean_names() %>%
    dplyr::mutate(
      expecting_funding = "No",
      list = "SFY25 Not Fundable List",
    )
  
  mn_combined <- bind_rows(mn_base_ec_fundable, mn_not_eligible, mn_not_fundable) %>%
    dplyr::filter(!is.na(estimated_project_cost)) %>%
    mutate(
    ) %>%
    dplyr::select(project_number, list, project_type, estimated_project_cost, expecting_funding, funding_amount, principal_forgiveness, disadvantaged)
  
  
  
  
  mn_lsl_fundable <- data.table::fread("year3/MN/data/mn_lsl_fundable.csv",
                                       colClasses = "character", na.strings = "") |> 
    janitor::clean_names() %>%
    dplyr::filter(!is.na(project_number)) %>%
    dplyr::mutate(project_type = "Lead",
           expecting_funding = "Yes",
           list = "SFY25 LSL Fundable List",
           estimated_lsl_pf = convert_to_numeric(estimated_dwrf_lsl_pf_grant_3,TRUE),
           estimated_lsl_loan = convert_to_numeric(estimated_dwrf_lsl_loan_4, TRUE),
           estimated_state_lsl_grant = convert_to_numeric(estimated_state_lsl_grant_5, TRUE),
           funding_amount = estimated_lsl_pf + estimated_lsl_loan,
           principal_forgiveness = estimated_lsl_pf,
           disadvantaged = case_when(
             nchar(estimated_dwrf_lsl_pf_grant_3) > 1 ~ "Yes",
             TRUE ~ "No Information")
    ) %>%
    dplyr::rename(estimated_project_cost = estimated_total_project_cost) %>%
    dplyr::select(project_number, list, project_type, estimated_project_cost, expecting_funding, funding_amount, principal_forgiveness, disadvantaged)
  
  
  mn_combined <- bind_rows(mn_combined, mn_lsl_fundable) %>%
    dplyr::rename(project_id = project_number)
  
  mn_comp_ppl <- data.table::fread("year3/MN/data/mn_comp_ppl.csv",
                                   colClasses = "character", na.strings = "") |> 
    janitor::clean_names() %>%
    dplyr::filter(!is.na(system))
  
  
  mn_clean <- mn_comp_ppl %>%
    dplyr::left_join(mn_combined, by="project_id") %>%
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = stringr::str_squish(system),
      pwsid = paste0("MN", stringr::str_remove(project_id, "-.*")),
      project_id = stringr::str_squish(project_id),
      project_name = as.character(NA),
      project_description = stringr::str_squish(project),
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        grepl(ec_str, project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
        grepl("lsl|lead", project_description, ignore.case = TRUE) ~ "Lead",
        TRUE ~ "General"),
      requested_amount = as.character(NA),
      project_cost = case_when(
        !is.na(estimated_project_cost) ~ estimated_project_cost,
        TRUE ~ project_cost),
      funding_amount = clean_numeric_string(funding_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      disadvantaged = replace_na(disadvantaged, "No Information"),
      population = clean_numeric_string(population),
      project_rank = stringr::str_squish(rank),
      project_score = stringr::str_squish(points),
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Minnesota",
      state_fiscal_year = "2025",
      list = replace_na(list, "SFY25 Comprehensive PPL")
    ) %>%
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
# mn_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision: No duplicates

# Check for disinfection byproduct in description
# mn_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
####### Decision: No change, classified as expected

# Check for lead subtypes
# mn_clean |>
#   dplyr::filter(project_type=="Lead") |>
# dplyr::mutate(
#   lead_type = dplyr::case_when(
#   stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
#   stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
#   stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
#  # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
#  stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
#  stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
#   TRUE ~ "unknown"
# )) |>
#   dplyr::filter(lead_type == "both")
####### Decision: No lead projects classified as both

  ## Check for lead subtypes: Unknown
  # mn_clean |>
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
  ### Decision: 24 unknowns 16--> LSLR, 8 cannot be ressolved
  
   mn_clean <- mn_clean |>
      dplyr::mutate(
       project_description = dplyr::case_when(
        grepl("LSL Repl", project_description) ~ paste0(project_description, " | FT: LSLR"),
              .default = project_description
       )
      )

####### SANITY CHECKS END #######

  run_tests(mn_clean)
  
  rm(list=setdiff(ls(), "mn_clean"))
  
  return(mn_clean)
}
