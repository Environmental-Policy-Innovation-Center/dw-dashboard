clean_mn_y2 <- function() {
 
  mn_base_ec_fundable <- data.table::fread("year2/MN/data/mn-base-ec-fundable.csv",
                                           colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::filter(!is.na(project_number)) |>
    dplyr::mutate(project_type = case_when(
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
      principal_forgiveness = ifelse( estimated_ec_pf + estimated_dac_pf > 1, as.character(estimated_ec_pf + estimated_dac_pf), "No Information"),
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
    dplyr::mutate(project_type = case_when(
      # set EC funded projects as EC, fill in remainder with text search downstream
      nchar(estimated_dwrf_emerging_contaminant_pf_grant_not_final_1) > 1 ~ "Emerging Contaminants", 
      TRUE ~ as.character(NA)),
      expecting_funding = "No",
      # all projects have - or NA for relevant columns
      disadvantaged = "No Information",
      list = "SFY24 Not Eligible List",
    ) 
  
  
  mn_combined <- bind_rows(mn_base_ec_fundable, mn_not_eligible) |>
    dplyr::select(project_number, list, project_type, estimated_project_cost, expecting_funding, funding_amount, principal_forgiveness, disadvantaged)
  
  
  mn_lead_fundable <- data.table::fread("year2/MN/data/mn-lead-fundable.csv",
                                        colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    filter(!is.na(project_number)) |>
    dplyr::mutate(project_type = "Lead",
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
    dplyr::select(project_number, list, project_type, estimated_project_cost, expecting_funding, funding_amount, principal_forgiveness, disadvantaged)
  
  
  mn_combined <- bind_rows(mn_combined, mn_lead_fundable) |>
    dplyr::rename(project_id = project_number)
    
  
  mn_comp_ppl <- data.table::fread("year2/MN/data/mn-comp-ppl.csv",
                                   colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::filter(points != "GRAND TOTAL")

  
  mn_clean <- mn_comp_ppl |>
    dplyr::left_join(mn_combined, by='project_id') |>
    dplyr::mutate(community_served = as.character(NA),
           borrower = stringr::str_squish(system),
           pwsid = paste0("MN", str_remove(project_id, "-.*")),
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
           state_fiscal_year = "2024",
           list = replace_na(list, "SFY24 Comprehensive PPL")
           ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
  requested_amount, funding_amount, principal_forgiveness, population, project_description,
  disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  
  
  run_tests(mn_clean)
  
  rm(list=setdiff(ls(), "mn_clean"))
  
  return(mn_clean)
}

####### SANITY CHECKS START #######

# Hone in on project id duplication
mn_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
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
# ####### Decision: No lead projects classified as both

####### SANITY CHECKS END #######