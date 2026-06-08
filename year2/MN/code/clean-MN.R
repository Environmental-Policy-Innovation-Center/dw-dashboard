clean_mn_y2 <- function() {
 
  mn_base_ec_fundable <- data.table::fread("year2/MN/data/mn-base-ec-fundable.csv",
                                           colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::filter(!is.na(project_number)) |>
    dplyr::mutate(
      project_type = case_when(
      # set EC funded projects as EC, fill in remainder with text search downstream
        nchar(estimated_dwrf_emerging_contaminant_pf_grant_not_final_1) > 1 ~ "Emerging Contaminants", 
        TRUE ~ as.character(NA)),
      expecting_funding = "Yes",
      list = "SFY24 General & EC Fundable List",
      estimated_dwrf = convert_to_numeric(estimated_dwrf_loan, TRUE),
      estimated_ec_pf = convert_to_numeric(estimated_dwrf_emerging_contaminant_pf_grant_not_final_1, TRUE),
      estimated_dac_pf = convert_to_numeric(estimated_dwrf_disadvantgd_community_principal_forgiveness_not_final_2, TRUE),
      estimated_wif = convert_to_numeric(estimated_wif_grant_not_final_3, TRUE),
      funding_amount = estimated_dwrf + estimated_ec_pf + estimated_dac_pf,
      principal_forgiveness = ifelse(estimated_ec_pf + estimated_dac_pf > 1, as.character(estimated_ec_pf + estimated_dac_pf), "No Information"),
      disadvantaged = case_when(
        estimated_dac_pf > 0 ~ "Yes",
        estimated_wif > 0 ~ "Yes",
        iup_status == "Part B" ~ "No Information",
        TRUE ~ "No")  
        )
  
  # part of table 1a.
  mn_not_eligible <-  data.table::fread("year2/MN/data/mn-not-eligible.csv",
                                        colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::mutate(
      project_type = case_when(
        # set EC funded projects as EC, fill in remainder with text search downstream
        nchar(estimated_dwrf_emerging_contaminant_pf_grant_not_final_1) > 1 ~ "Emerging Contaminants", 
        TRUE ~ as.character(NA)),
      expecting_funding = "No",
      # all projects have - or NA for relevant columns
      disadvantaged = "No Information",
      list = "SFY24 Not Eligible List",
    ) 
  
  mn_combined <- bind_rows(mn_base_ec_fundable, mn_not_eligible) |>
    dplyr::select(project_number, list, project_type, estimated_project_cost, expecting_funding, funding_amount, principal_forgiveness, disadvantaged, estimated_wif_grant_not_final_3, state_or_federal_appropriation, other_funds )
  
  mn_lead_fundable <- data.table::fread("year2/MN/data/mn-lead-fundable.csv",
                                        colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    filter(!is.na(project_number)) |>
    dplyr::mutate(
      project_type = "Lead",
      expecting_funding = "Yes",
      list = "SFY24 LSL Fundable List",
      estimated_lsl_pf = convert_to_numeric(estimated_dwrf_lsl_pf_grant,TRUE),
      estimated_lsl_loan = convert_to_numeric(estimated_dwrf_lsl_loan, TRUE),
      funding_amount = estimated_lsl_pf + estimated_lsl_loan,
      principal_forgiveness = clean_numeric_string(estimated_lsl_pf),
      disadvantaged = case_when(
        nchar(estimated_dwrf_lsl_pf_grant) > 1 ~ "Yes",
        TRUE ~ "No")
      ) |>
    dplyr::rename(estimated_project_cost = estimated_total_project_cost) |>
    dplyr::select(project_number, list, project_type, estimated_project_cost, expecting_funding, funding_amount, principal_forgiveness, disadvantaged, estimated_state_lsl_grant)
  
  mn_combined <- bind_rows(mn_combined, mn_lead_fundable) |>
    dplyr::rename(project_id = project_number)
  
  mn_comp_ppl <- data.table::fread("year2/MN/data/mn-comp-ppl.csv",
                                   colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::filter(points != "GRAND TOTAL")

  mn_combined_full <- mn_comp_ppl |>
    dplyr::left_join(mn_combined, by='project_id') |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = stringr::str_squish(system),
      pwsid = paste0("MN", str_remove(project_id, "-.*")),
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
      state_fiscal_year = "2024",
      list = replace_na(list, "SFY24 Comprehensive PPL")
      ) 
  
  mn_clean <- mn_combined_full |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
  requested_amount, funding_amount, principal_forgiveness, population, project_description,
  disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)

####### SANITY CHECKS START #######

#check pwsid length
# sum(stringr::str_count(mn_clean$pwsid) != 9)
  
# Hone in on project id duplication
# mn_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision: 1 duplicate:  1610007-7, both expecting funding, kept the one with funding amount.      

mn_clean <- mn_clean |>
  dplyr::arrange(desc(funding_amount))|>
  dplyr::group_by(project_id) |>
  dplyr::slice_head(n=1) |>
  dplyr::ungroup()

# Check for disinfection byproduct in description
# mn_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
####### Decision: No change, classified as expected

# Check for lead subtypes
# mn_clean |>
#   dplyr::filter(project_type=="Lead") |>
#   dplyr::mutate(
#   lead_type = dplyr::case_when(
#   stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
#   stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
#   stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
# # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
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
### Decision: 16 unknowns --> LSLR
  
   mn_clean <- mn_clean |>
      dplyr::mutate(
       project_description = dplyr::case_when(
              project_id %in% c("1050004-5", "1050004-6", "1270024-18", "1270024-19","1270024-20","1270024-21","1270024-22","1690011-12","1690011-14","1690011-15","1690011-16", "1690011-17","1690011-18","1690011-19","1690011-20","1730027-27") ~ paste0(project_description, " | FT: LSLR"),
              .default = project_description
       )
      )

####### SANITY CHECKS END #######
  
# Produce Other Federal and State Funds dataset
mn_ofsf <- mn_combined_full |>
  dplyr::arrange(desc(funding_amount))|>
  dplyr::group_by(project_id) |>
  dplyr::slice_head(n=1) |>
  dplyr::ungroup() |>
  dplyr::mutate(
      project_description = dplyr::case_when(
            project_id %in% c("1050004-5", "1050004-6", "1270024-18", "1270024-19","1270024-20","1270024-21","1270024-22","1690011-12","1690011-14","1690011-15","1690011-16", "1690011-17","1690011-18","1690011-19","1690011-20","1730027-27") ~ paste0(project_description, " | FT: LSLR"),
            .default = project_description
      )
  )  |>
  dplyr::relocate(estimated_wif_grant_not_final_3,state_or_federal_appropriation, other_funds, estimated_state_lsl_grant ) |>
  dplyr::filter(
    rowSums(!is.na(dplyr::pick(estimated_wif_grant_not_final_3:estimated_state_lsl_grant))) >= 1
  ) |>
  dplyr::mutate(
    project_cost_ofsf = as.character(NA),
    requested_amount_ofsf = as.character(NA),
    funding_amount_ofsf = ifelse(
      list %in% c("SFY24 General & EC Fundable List", "SFY24 Not Eligible List"),
      convert_to_numeric(estimated_wif_grant_not_final_3) + convert_to_numeric(state_or_federal_appropriation) + convert_to_numeric(other_funds),   
      convert_to_numeric(estimated_state_lsl_grant)
    ),
    funding_amount_ofsf = clean_numeric_string(funding_amount_ofsf),
    expecting_funding_ofsf = dplyr::case_when(
      list %in% c("SFY24 General & EC Fundable List", "SFY24 Not Eligible List") & (convert_to_numeric(estimated_wif_grant_not_final_3) > 0 | convert_to_numeric(state_or_federal_appropriation) > 0 | convert_to_numeric(other_funds) > 0) ~ "Yes", 
      list == "SFY24 LSL Fundable List"  & convert_to_numeric(estimated_state_lsl_grant) > 0 ~ "Yes",
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

