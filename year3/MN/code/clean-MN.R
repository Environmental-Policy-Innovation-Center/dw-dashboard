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
    dplyr::select(project_number, list, project_type, estimated_project_cost, expecting_funding, funding_amount, principal_forgiveness, disadvantaged, estimated_wif_grant_not_final_3, state_or_federal_appropriation, other_funds)
  
  mn_lsl_fundable <- data.table::fread("year3/MN/data/mn_lsl_fundable.csv",
                                       colClasses = "character", na.strings = "") |> 
    janitor::clean_names() %>%
    dplyr::filter(!is.na(project_number)) %>%
    dplyr::mutate(
      project_type = "Lead",
      expecting_funding = "Yes",
      list = "SFY25 LSL Fundable List",
      estimated_lsl_pf = convert_to_numeric(estimated_dwrf_lsl_pf_grant_3,TRUE),
      estimated_lsl_loan = convert_to_numeric(estimated_dwrf_lsl_loan_4, TRUE),
      estimated_state_lsl_grant = convert_to_numeric(estimated_state_lsl_grant_5, TRUE),
      funding_amount = estimated_lsl_pf + estimated_lsl_loan,
      principal_forgiveness = estimated_lsl_pf,
      disadvantaged = case_when(
        nchar(estimated_dwrf_lsl_pf_grant_3) > 1 ~ "Yes",
        TRUE ~ "No")
    ) %>%
    dplyr::rename(estimated_project_cost = estimated_total_project_cost) %>%
    dplyr::select(project_number, list, project_type, estimated_project_cost, expecting_funding, funding_amount, principal_forgiveness, disadvantaged, estimated_state_lsl_grant_5)
  
  mn_combined <- bind_rows(mn_combined, mn_lsl_fundable) %>%
    dplyr::rename(project_id = project_number)
  
  mn_comp_ppl <- data.table::fread("year3/MN/data/mn_comp_ppl.csv",
                                   colClasses = "character", na.strings = "") |> 
    janitor::clean_names() %>%
    dplyr::filter(!is.na(system))
  
  mn_combined_full <- mn_comp_ppl %>%
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
        grepl("lsl|lead", project_description, ignore.case = TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
        #Do a case sensitive search of Mn
        grepl("Mn", project_description) ~ "Emerging Contaminants",
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
    ) 
  
  mn_clean <- mn_combined_full %>%
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  
####### SANITY CHECKS START #######

#check pwsid length
# sum(stringr::str_count(mn_clean$pwsid) != 9)
  
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
  
# Produce Other Federal and State Funds dataset
  mn_ofsf <- mn_combined_full |>
    dplyr::mutate(
       project_description = dplyr::case_when(
        grepl("LSL Repl", project_description) ~ paste0(project_description, " | FT: LSLR"),
              .default = project_description
       )
      ) |>
    dplyr::mutate(
      estimated_wif_grant_not_final_3 = clean_numeric_string(estimated_wif_grant_not_final_3)
    ) |>
    dplyr::relocate(estimated_wif_grant_not_final_3,state_or_federal_appropriation, other_funds, estimated_state_lsl_grant_5) |>
    dplyr::filter(
      rowSums(!is.na(dplyr::pick(estimated_wif_grant_not_final_3:estimated_state_lsl_grant_5))) >= 1
    ) |>
    dplyr::mutate(
      project_cost_ofsf = as.character(NA),
      requested_amount_ofsf = as.character(NA),
      funding_amount_ofsf = dplyr::case_when(
        list %in% c("SFY25 General & EC Fundable List", "SFY25 Not Eligible List") ~ clean_numeric_string(convert_to_numeric(estimated_wif_grant_not_final_3) + convert_to_numeric(state_or_federal_appropriation) + convert_to_numeric(other_funds)),   
        list == "SFY25 LSL Fundable List" ~ clean_numeric_string(convert_to_numeric(estimated_state_lsl_grant_5)),
        .default = "No Information"
      ),
      expecting_funding_ofsf = dplyr::case_when(
        list %in% c("SFY25 General & EC Fundable List", "SFY25 Not Eligible List") & (convert_to_numeric(estimated_wif_grant_not_final_3) > 0 | convert_to_numeric(state_or_federal_appropriation) > 0 | convert_to_numeric(other_funds) > 0) ~ "Yes", 
        list == "SFY25 LSL Fundable List"  & convert_to_numeric(estimated_state_lsl_grant_5) > 0 ~ "Yes",
        .default = "No Information"
      ) 
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, project_cost_ofsf,
           requested_amount, requested_amount_ofsf, funding_amount, funding_amount_ofsf, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, expecting_funding_ofsf, state, state_fiscal_year, list)
  
  save_update_ofsf(mn_ofsf)

  run_tests(mn_clean)
  
  rm(list=setdiff(ls(), "mn_clean"))
  
  return(mn_clean)
}
