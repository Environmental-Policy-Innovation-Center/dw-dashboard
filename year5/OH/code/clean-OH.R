clean_oh_y5 <- function() {
  
  base_path <- "year5/OH/data"
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table
  
oh_comprehensive_nef <-  data.table::fread(file.path(base_path, "OH_Y5_nef.csv")) |>
    janitor::clean_names() 

oh_comprehensive <-  data.table::fread(file.path(base_path, "OH_Y5_comprehensive.csv")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    expecting_funding = ifelse(epic_id %in% oh_comprehensive_nef$epic_id, "No", "Yes"),
    disadvantaged = ifelse(grepl("DIS", rate), "Yes", "No"),
    list = "SFY27 Fundable List and Comprehensive List"
  )  

oh_dac_reg <-  data.table::fread(file.path(base_path, "OH_Y5_DAC_Regionalization.csv")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    project_score = clean_numeric_string(project_score),
    list = "SFY27 DAC + Regionalization PPL",
    est_principal_forgiveness_dac_reg = principal_forgiveness,
    expecting_funding = ifelse(est_principal_forgiveness_dac_reg == "Bypass 3", "No", NA_character_),
  ) |>
  dplyr::select(epic_id, project_score, list, est_principal_forgiveness_dac_reg, expecting_funding )

oh_comp_dac_reg <- oh_comprehensive |>
  dplyr::left_join(oh_dac_reg, by = "epic_id") |>
  dplyr::mutate(
    expecting_funding = dplyr::coalesce(expecting_funding.y, expecting_funding.x),
    principal_forgiveness =ifelse(expecting_funding == "Yes",  est_principal_forgiveness_dac_reg, NA_character_),
    list = dplyr::coalesce(list.y, list.x)
  )|>
  dplyr::select(-c(expecting_funding.y, expecting_funding.x, list.x, list.y))
  

oh_ec <-  data.table::fread(file.path(base_path, "OH_Y5_EC.csv")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    project_type = "Emerging Contaminants",
    expecting_funding = ifelse(grepl("BYPASS 2|BYPASS 3", est_ec_principal_forgiveness), "No", NA_character_),
    project_score = clean_numeric_string(project_score),
    list = "SFY27 EC PPL"
  ) |>
  dplyr::select(epic_id, project_type, expecting_funding, project_score, list, est_ec_principal_forgiveness)

oh_comp_dac_reg_ec <- oh_comp_dac_reg |>
  dplyr::left_join(oh_ec, by = "epic_id") |>
  dplyr::mutate(
    expecting_funding = dplyr::coalesce(expecting_funding.y, expecting_funding.x),
    list = dplyr::coalesce(list.y, list.x),
    principal_forgiveness = dplyr::case_when(
      (list == "SFY27 EC PPL" & expecting_funding == "Yes") ~ est_ec_principal_forgiveness,
      .default = principal_forgiveness
    ),
    project_score = dplyr::coalesce(project_score.y, project_score.x)
  )|>
  dplyr::select(-c(expecting_funding.y, expecting_funding.x, list.y, list.x, project_score.y, project_score.x))

  
oh_lead <-  data.table::fread(file.path(base_path, "OH_Y5_Lead.csv")) |>
    janitor::clean_names() |>
  dplyr::mutate(
    project_type = "Lead",
    list = "SFY27 Lead PPL"
  ) |>
  dplyr::select(epic_id, project_type, list)

#there are 12 project ids in lead list overlapping with dac+regionalization list; will default to lead list naming
# oh_comp_dac_reg_ec |>
#    dplyr::filter(epic_id %in%  oh_lead$epic_id) |>
#   dplyr::filter(!list == "SFY27 Fundable List and Comprehensive List") |>
#   dplyr::select(epic_id, list) |>
#   dplyr::pull(epic_id)
# [1] 226 230 278 307 308 353 396 405 424 437 460 512  

oh_comp_dac_reg_ec_lead <- oh_comp_dac_reg_ec |>
  dplyr::left_join(oh_lead, by = "epic_id" ) |>
  dplyr::mutate(
    project_type = dplyr::coalesce(project_type.y, project_type.x),
    list = dplyr::coalesce(list.y, list.x),
    principal_forgiveness = dplyr::case_when(
      list == "SFY27 Lead PPL" & grepl("LSL PF", rate) ~ "No Information" ,
      .default = principal_forgiveness
    )
  )
  
oh_discount <-  data.table::fread(file.path(base_path, "OH_Y5_discount.csv")) |>
    janitor::clean_names() 

oh_ec_sdc <-  data.table::fread(file.path(base_path, "OH_Y5_EC_SDC.csv")) |>
    janitor::clean_names() 

oh_clean <- oh_comp_dac_reg_ec_lead |>
  dplyr::mutate(
    list = dplyr::case_when(
      epic_id %in% oh_discount$epic_id ~ "SFY27 Discount PPL",
      epic_id %in% oh_ec_sdc$epic_id ~ "SFY27 EC SDC",
      .default = list
    ),
    expecting_funding = ifelse(list == "SFY27 EC SDC", "No", expecting_funding )
  ) |>
  dplyr::mutate(
    community_served = county, 
    borrower = entity, 
    pwsid = pws_id, 
    project_id = as.character(NA), 
    project_name = as.character(NA), 
    project_type = dplyr::case_when(
      !is.na(project_type) ~ project_type,
      grepl("LSL", rate, ignore.case = TRUE) ~ "Lead",
      grepl("lsl|lead", project, ignore.case=TRUE) ~ "Lead",
      grepl("EC", rate, ignore.case = TRUE) ~ "Emerging Contaminants", 
      grepl(ec_str, project, ignore.case=TRUE) ~ "Emerging Contaminants", 
      .default =  "General"
    ),
    project_cost = as.character(NA), 
    requested_amount = clean_numeric_string(estimated_loan_amount), 
    funding_amount = as.character(NA),
    #amendment
    principal_forgiveness = ifelse(
      grepl("BYPASS 1|BYPASS 2|BYPASS 3", est_ec_principal_forgiveness) | grepl("Bypass 1|Bypass 2|Bypass 3", est_principal_forgiveness_dac_reg  ),
      "0",
       principal_forgiveness
    ),
    principal_forgiveness = replace_na(principal_forgiveness, "0"),
    principal_forgiveness = ifelse(principal_forgiveness=="", "0", principal_forgiveness),
    project_description = project, 
    population = clean_numeric_string(sdwis_population),
    disadvantaged = disadvantaged,
    project_rank = as.character(NA),
    project_score = replace_na(project_score, "No Information"),
    state = "Ohio",
    state_fiscal_year = "2027"
  )
    
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  # oh_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
  
  # Check for lead subtypes: Both
  # oh_clean |>
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
  #   dplyr::filter(lead_type == "both")

  ####### Decision: No lead projects classified as both
  
  # Check for lead subtypes: Unknown
  # oh_clean |>
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
  #   dplyr::filter(lead_type == "unknown") |>
  #   dplyr::pull(epic_id)
  #   dplyr::select(epic_id, community_served, borrower, pwsid, requested_amount, 
  #     principal_forgiveness, population, 
  #     disadvantaged, project_score, expecting_funding, list, project_description, project_type, lead_type) |>
  #   readr::write_csv("~/Desktop/OH_Y5_lead_subtypes.csv")

  #Decision: 26 projects classified as unknown; Lauren reviewed and reclassified 2026/07/17


  oh_clean <- oh_clean |>
    dplyr::mutate(
      project_description = dplyr::case_when(
        epic_id %in% c("124", "125", "126", "128", "129", "130", "131", "132", "134", "136", "137", "138", "139", "140", "226", "230", "278", "307", "308", "353", "396", "405", "424", "437", "438", "477", "484", "508", "512") ~ paste0(project_description, " | FT: LSLR"),
        epic_id %in% c("181", "214") ~ paste0(project_description, " | FT: LSLI"),
        .default = project_description
      )
    )
  ####### SANITY CHECKS END #######

  # Produce Other Federal and State Funds dataset

  oh_ofsf <- oh_clean |>
    dplyr::filter(
      grepl("BYPASS 2|BYPASS 3", est_ec_principal_forgiveness) | est_principal_forgiveness_dac_reg == "Bypass 3"
    ) |>
    dplyr::mutate(
      project_cost_ofsf = as.character(NA), 
      requested_amount_ofsf = clean_numeric_string(estimated_loan_amount),
      funding_amount_ofsf = as.character(NA),
      expecting_funding_ofsf = "Yes"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, project_cost_ofsf,
           requested_amount, requested_amount_ofsf, funding_amount, funding_amount_ofsf, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, expecting_funding_ofsf, state, state_fiscal_year, list)
  
  save_update_ofsf(oh_ofsf)

  oh_clean <-  oh_clean |>
      dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  # Run validation tests
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}