clean_mn_y1 <- function() {
  
  mn_comp_ppl <- data.table::fread("year1/MN/data/mn_comp_ppl.csv",
                      colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::filter(rank != "GRAND TOTAL")
  
  
  mn_not_eligible <- data.table::fread("year1/MN/data/mn_not_eligible.csv",
                           colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::filter(!is.na(project_number)) |>
    dplyr::mutate(list = "SFY23 Not Eligible List",
           expecting_funding = "No")
  
  
  # following amendment 1, the not-fundable table got added to the fundable list, part B of the original IUP
  mn_part_b <- data.table::fread("year1/MN/data/mn_not_fundable.csv",
                           colClasses = "character", na.strings = "") |> 
    janitor::clean_names()
  
  # part A of the original IUP
  mn_part_a <- data.table::fread("year1/MN/data/mn_fundable.csv",
                       colClasses = "character", na.strings = "") |> 
    janitor::clean_names()
  
  
  mn_fundable <- bind_rows(mn_part_a, mn_part_b) |> 
    dplyr::mutate(expecting_funding = "Yes",
           list = "SFY23 Fundable List") |>
    dplyr::filter(!is.na(project_number))
    
    mn_combined <- bind_rows(mn_fundable, mn_not_eligible) |>
    # all subsequent mutations apply to fundable projects and not eligible projects to then join on comp project list
      dplyr::mutate(,
           project_type = dplyr::case_when(
                                    grepl("LSL", status) ~ "Lead",
                                    grepl("EC", status) ~ "Emerging Contaminants",
                                    grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants", 
                                    nchar(estimated_dwrf_emerging_contaminant_principal_forgiveness_not_final_2) > 1 ~ "Emerging Contaminants",
                                    grepl("lsl|lead", project_description, ignore.case = TRUE) ~ "Lead",
                                    nchar(estimated_dwrf_lead_service_line_replacement_principal_forgiveness_not_final_1) > 1 ~ "Lead",
                                    TRUE ~ "General"),
           estimated_dwrf = convert_to_numeric(estimated_dwrf_loan, TRUE),
           estimated_lsl_pf = convert_to_numeric(estimated_dwrf_lead_service_line_replacement_principal_forgiveness_not_final_1, TRUE),
           estimated_ec_pf = convert_to_numeric(estimated_dwrf_emerging_contaminant_principal_forgiveness_not_final_2, TRUE),
           estimated_dac_pf = convert_to_numeric(estimated_dwrf_disadvantgd_community_principal_forgiveness_not_final_3, TRUE),
           estimated_wif = convert_to_numeric(estimated_wif_grant_not_final_4, TRUE),
           funding_amount = estimated_dwrf + estimated_lsl_pf + estimated_ec_pf + estimated_dac_pf,
           # add up PF for fundable projects, No Info for Not Eligible list projects
           principal_forgiveness = dplyr::case_when(
             list == "SFY23 Fundable List" ~ as.character(estimated_lsl_pf + estimated_ec_pf + estimated_dac_pf),
             TRUE ~ "No Information"),
           disadvantaged = dplyr::case_when(
             list == "SFY23 Not Eligible List" ~ "No Information",
             nchar(estimated_dwrf_lead_service_line_replacement_principal_forgiveness_not_final_1) > 1 ~ "Yes",
             nchar(estimated_dwrf_disadvantgd_community_principal_forgiveness_not_final_3) > 1 ~ "Yes",
             nchar(estimated_wif_grant_not_final_4) > 1 ~ "Yes",
             TRUE ~ "No Information"
             )
           ) |>
      dplyr::rename(project_id = project_number) |>
      dplyr::select(project_id, expecting_funding, list, estimated_project_cost, project_type, funding_amount, principal_forgiveness, disadvantaged)
  

  mn_clean <- mn_comp_ppl |>
    dplyr::left_join(mn_combined, by='project_id') |>
    dplyr::mutate(community_served = as.character(NA),
           borrower = stringr::str_squish(system),
           pwsid = paste0("MN", str_remove(project_id, "-.*")),
           project_id = stringr::str_squish(project_id),
           project_name = as.character(NA),
           project_description = stringr::str_squish(project),
           project_type = dplyr::case_when(!is.na(project_type) ~ project_type,
                                    grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
                                    grepl("lsl|lead", project_description, ignore.case = TRUE) ~ "Lead",
                                    TRUE ~ "General"),
           project_cost = dplyr::case_when(!is.na(estimated_project_cost) ~ clean_numeric_string(estimated_project_cost),
                                    TRUE ~ clean_numeric_string(project_cost)),
           requested_amount = as.character(NA),
           population = clean_numeric_string(population),
           funding_amount = clean_numeric_string(funding_amount),
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           disadvantaged = replace_na(disadvantaged, "No Information"),
           project_rank = stringr::str_squish(rank),
           project_score = stringr::str_squish(points),
           expecting_funding = replace_na(expecting_funding, "No"),
           state = "Minnesota",
           state_fiscal_year = "2023",
           list = replace_na(list, "SFY23 Comprehensive List")
    ) |> 
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
  ### Decision: 9 unknowns --> LSLR
  
   mn_clean <- mn_clean |>
      dplyr::mutate(
       project_description = dplyr::case_when(
              project_id %in% c("1050004-5","1690011-12","1690011-14" , "1690011-15", "1690011-16", "1270024-18", "1270024-19", "1270024-20", "1270024-21") ~ paste0(project_description, " | FT: LSLR"),
              .default = project_description
       )
      )

  ####### SANITY CHECKS END #######
  
  run_tests(mn_clean)
  rm(list=setdiff(ls(), "mn_clean"))
  
  return(mn_clean)
}
