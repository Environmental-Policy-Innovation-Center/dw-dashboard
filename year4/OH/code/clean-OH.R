clean_oh_y4 <- function() {
  
  base_path <- "year4/OH/data"
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table
  
  not_ef_list <- c("254", "265", "453", "523", "NF1")
  
  ec_sdc_list <- c("209", "334", "282", "290", "462", "247", "508")

  # fundable project List
  oh_fundable <- data.table::fread(file.path(base_path, "oh_comp_ppl.csv")) |>
    janitor::clean_names() |>
    # all of these are expecting funding
    dplyr::mutate(
      expecting_funding = ifelse(epic_project_id %in% not_ef_list, "No", "Yes")
          ) |>
    dplyr::select(-c(estimated_award_date, loan_type, district_office))
  
  
  # DAC & regionalization list
  oh_dac <- data.table::fread(file.path(base_path, "oh_dac_ppl.csv")) |>
    janitor::clean_names() |>
    # these are all disadvantaged 
    dplyr::mutate(disadvantaged = "Yes",
           list = "SFY26 DAC + Regionalization PPL",
           # based on PPLs from past years, these should be "no info" but I 
           # need to remove the number for the clean_numeric_string function. 
           # Also note some are just $0.00 
           dac_pf = ifelse(estimated_principal_forgiveness %in% 
                             c("Bypass1", "Bypass2", "Bypass 3"), "Bypass", 
                           estimated_principal_forgiveness)) |>
    dplyr::select(epic_project_id, dac_pf, project_score, rate, disadvantaged, list)
  
  # [keep] projects eligible for regionalizaton discount - this doesn't really provide 
  # any info that isn't already captured by our lists
  # oh_reg_disc <- data.table::fread(file.path(base_path, "oh_regionalization_ppl.csv")) |>
  #   janitor::clean_names()
  
  # merging fundable and dac lists: 
  oh_fund_dac <- merge(oh_fundable, oh_dac, by = "epic_project_id", all = TRUE) |>
    # pasting the rates together for string matching
    dplyr::mutate(rate = paste(rate.x, rate.y, sep = " "), 
           # string matching based on rate or project 
           project_type = case_when(grepl("LSL", rate, ignore.case = TRUE) ~ "Lead",
                                    grepl("LSL|lead", project, ignore.case=TRUE) ~ "Lead",
                                    grepl("HAB|PFAS|EC", rate, ignore.case = TRUE) ~ "Emerging Contaminants", 
                                    grepl(ec_str, project, ignore.case=TRUE) ~ "Emerging Contaminants", 
                                    TRUE ~ "General")) |>
    dplyr::select(-c("rate.x", "rate.y"))

  # EC list: 
  oh_ec <- data.table::fread(file.path(base_path, "oh_ec_ppl.csv")) |>
    janitor::clean_names() |>
    dplyr::mutate(project_type = "Emerging Contaminants",
           list = "SFY26 EC PPL",
           disadvantaged = ifelse(grepl("DIS", rate), "Yes", as.character(NA)),
           # removing numbers from bypass columns 
           ec_pf = ifelse(estimated_ec_principal_forgiveness %in% 
                             c("Bypass1", "Bypass2", "Bypass 3"), "Bypass", 
                          estimated_ec_principal_forgiveness)) |>
    dplyr::select(epic_project_id, project_type, ec_pf, project_score, disadvantaged, list)
  
  
  # merging with oh_fund_dac to handle principal forgiveness columns: 
  oh_fund_dac_ec <- merge(oh_fund_dac, oh_ec, by = "epic_project_id", all = T) |>
    # note there are some HAB projects that don't show up in the EC list, but 
    # are captured by our string matching 
    dplyr::mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    TRUE ~ project_type.x), 
           # project scores don't overlap, so just combining columns here
           project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                     TRUE ~ project_score.x),
           # pf cols don't overlap, so just combining columns here
           principal_forgiveness = case_when(!is.na(dac_pf) ~ dac_pf, 
                                             !is.na(ec_pf) ~ ec_pf, 
                                             TRUE ~ as.character(NA)),
           # after storing PF column, set known but empty columns to 0, then string
           principal_forgiveness = clean_numeric_string(convert_to_numeric(principal_forgiveness, T)),
           disadvantaged = ifelse(!is.na(disadvantaged.y), disadvantaged.y, disadvantaged.x),
           list = ifelse(!is.na(list.y), list.y, list.x)
           ) |>
    dplyr::select(-c(project_type.y, project_type.x, dac_pf, ec_pf, project_score.x, 
              project_score.y, disadvantaged.x, disadvantaged.y, list.x, list.y)) 
  
  # This is no longer applicable (ie. the dataset passed to the mutate does not have bypass or * in principal_forgiveness, since clean_numeric_string is converting PF to 0)
  # |>
  #   # fixing the expecting funding columns based on the presence of "Bypass" 
  #   # or "*" in the PF columns 
  #   # link to thread where we decided this: https://enviropolicyinno.slack.com/archives/C08LXGF02AE/p1762974431302769?thread_ts=1759336213.263239&cid=C08LXGF02AE
  #   dplyr::mutate(expecting_funding = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "No", 
  #                                        TRUE ~ expecting_funding),
  #          principal_forgiveness = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "0", 
  #                                            TRUE ~ principal_forgiveness))
  
  # lead list: 
  oh_lead <- data.table::fread(file.path(base_path, "oh_lead_ppl.csv")) |>
    janitor::clean_names() |>
    # these are all lead projects
    dplyr::mutate(project_type = "Lead",
           list = "SFY26 Lead PPL",
           principal_forgiveness = ifelse(grepl("LSL PF", rate), "No Information", as.character(NA)),
           disadvantaged = ifelse(grepl("DIS", rate), "Yes", as.character(NA))) |>
    dplyr::select(epic_project_id, project_type, principal_forgiveness, disadvantaged, list)
  
  
  oh_fund_dac_ec_lead <-  merge(oh_fund_dac_ec, oh_lead, by = "epic_project_id", all = T) |>
    dplyr::mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    TRUE ~ project_type.x), 
           disadvantaged = ifelse(!is.na(disadvantaged.y), disadvantaged.y, disadvantaged.x),
           list = ifelse(!is.na(list.y), list.y, list.x),
           principal_forgiveness = ifelse(!is.na(principal_forgiveness.y), principal_forgiveness.y, principal_forgiveness.x)
    ) |>
    dplyr::select(-c(project_type.y, project_type.x, disadvantaged.x, disadvantaged.y, list.x, list.y, principal_forgiveness.x, principal_forgiveness.y))
  
  
  # [keep] import one project that wasn't already in the PPL from list of not expecting funding projects
  missing_not_funded_projects <- data.table::fread("year4/OH/data/oh-missing-not-funded.csv",
                                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY26 Comprehensive List",
           expecting_funding = "No",
           project_type = "General",
           ) |>
    dplyr::select(entity, pws_id, county, list, project_type, expecting_funding, project)
  
    oh_clean <- bind_rows(oh_fund_dac_ec_lead, missing_not_funded_projects) |>
    # resolving project type overlaps 
      dplyr::mutate(
           # process numeric cols:
           population = clean_numeric_string(sdwis_population), 
           project_score = clean_numeric_string(project_score), 
           requested_amount =  clean_numeric_string(estimated_loan_amount), 
           principal_forgiveness = replace_na(principal_forgiveness, "0"),
           # ensure PF is 0 if project not expecting funding, even if listed elsewhere
           principal_forgiveness = ifelse(epic_project_id %in% not_ef_list, 0, principal_forgiveness),
           # process character cols: 
           pwsid = pws_id, 
           project_description = project, 
           community_served = county, 
           borrower = entity, 
           funding_amount = as.character(NA),
           project_id = as.character(NA), 
           project_name = as.character(NA), 
           project_cost = as.character(NA), 
           project_rank = as.character(NA), 
           disadvantaged = ifelse(is.na(disadvantaged), "No", disadvantaged),
           list = replace_na(list, "SFY26 Fundable List and Comprehensive List"),
           state = "Ohio",
           state_fiscal_year = "2026")
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
  #   dplyr::filter(lead_type == "unknown")

  #Decision: 26 projects classified as unknown

    oh_clean <- oh_clean |>
      dplyr::left_join(
        data.table::data.table(
          community_served = c("Ross","Harrison", "Hamilton","Hamilton","Hamilton","Hamilton", "Hamilton","Hamilton","Hamilton","Hamilton", "Hamilton","Pickaway","Allen","Jefferson","Jefferson", "Hardin","Hardin","Williams","Meigs","Ottawa", "Ottawa","Portage","Perry","Clark","Trumbull", "Huron"),
          borrower = c("Bainbridge", "Bowerston","Cincinnati","Cincinnati","Cincinnati", "Cincinnati","Cincinnati","Cincinnati","Cincinnati", "Cincinnati","Cincinnati","Circleville","Delphos", "Jefferson County","Jefferson County","Kenton", "Kenton","Montpelier","Pomeroy","Port Clinton", "Port Clinton","Portage County","Somerset", "Springfield","Warren","Willard"),
          requested_amount = c("4975000","1000000","1100000","2253355","4999139","5200676","3101870","3557872","1524764","600000","570000","1701000","1169773","7500000","7500000","4839599","1453797","2561200", "3219700","5982189","325000","3500000","849500","986253","826090","2100000"),
          project_description = c("Waterline Replacement Project",
                                "Distribution System and Meter Replacement","Branch Only- Jonathan, Ruth, Woodburn LSL",
                                "Southern Hawthorne Water Main Replacement",
                                "Monastery - Mt. Adams Water Main Replacement",
                                "McHenry - Wooster Area Water Main Replacement",
                                "Lyon - Wheeler Area Water Main Replacement",
                                "Fire Flow 23 Water Main Replacement","Baker Water Main Replacement",
                                "Branch Only - Fairmount LSL",
                                "Harrison Cora - Fairmount LSL","Utility Improvements for Walnut Street",
                                "Pierce Street Waterline Replacement","Amsterdam",
                                "Bergholz Water System Improvements",
                                "Downtown Waterline Replacement Phase 2B",
                                "Detroit Street Waterline and LSL Phase 3","Main Street Waterline LSL",
                                "Breezy Heights Tank/ New Wells / Water Line Replacement",
                                "Water and Sanitary Sewer Infrastructure Improvements","Laurel Street Reconstruction",
                                "Village of Mantua Distribution Replacement Ph 2",
                                "Water Meter Replacement","East High St Water Services",
                                "2022 Waterline Replacement Program (Area C)",
                                "Waterline Replacement Project"),
            list = c("SFY26 Lead PPL",
                                "SFY26 Lead PPL","SFY26 Lead PPL","SFY26 Lead PPL",
                                "SFY26 Lead PPL","SFY26 Lead PPL","SFY26 Lead PPL",
                                "SFY26 Lead PPL","SFY26 Lead PPL",
                                "SFY26 Lead PPL","SFY26 Lead PPL","SFY26 Lead PPL",
                                "SFY26 Lead PPL","SFY26 Lead PPL","SFY26 Lead PPL",
                                "SFY26 Lead PPL","SFY26 Lead PPL","SFY26 Lead PPL",
                                "SFY26 Lead PPL","SFY26 Lead PPL","SFY26 Lead PPL",
                                "SFY26 Lead PPL","SFY26 Lead PPL","SFY26 Lead PPL",
                                "SFY26 Lead PPL","SFY26 Lead PPL"),
            new_lead_type = c("lslr","lslr",
                                "lslr","lslr","lslr","lslr","lslr","lslr","lslr",
                                "lslr","lslr","lslr","lslr","unknown","lslr",
                                "lslr","lslr","lslr","lslr","lslr","lslr","lslr",
                                "lslr","unknown","lslr","lslr")
          ),
      by = c("community_served", "borrower", "project_description", "requested_amount", "list")
    ) |>
    dplyr::mutate(
      project_description = dplyr::case_when(
        !is.na(new_lead_type) ~ paste0(project_description, " | FT: ", stringr::str_to_upper(new_lead_type)),
        .default = project_description
      )
    ) |>
    dplyr::select(-new_lead_type)
    # Decision: 2 were left as unknown

  ####### SANITY CHECKS END #######

  # Produce Other Federal and State Funds dataset
  ec_sdc_table <- data.table::fread(file.path(base_path, "OH_Y4_SFY26_State_Funds.csv"),
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names()

  oh_ofsf <- oh_clean |>
    dplyr::mutate(epic_project_id = as.character(epic_project_id)) |>
    dplyr::filter(epic_project_id %in% ec_sdc_list) |>
    dplyr::left_join(ec_sdc_table |> dplyr::select(epic_project_id,sdc_ec_grant), by= "epic_project_id") |>
    dplyr::mutate(
      project_cost_ofsf = as.character(NA), #full column is No Information, keeping consistency to core dataset 
      requested_amount_ofsf = as.character(NA),
      funding_amount_ofsf = sdc_ec_grant,
      expecting_funding_ofsf = "Yes"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, project_cost_ofsf,
           requested_amount, requested_amount_ofsf, funding_amount, funding_amount_ofsf, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, expecting_funding_ofsf, state, state_fiscal_year, list)
  
  save_update_ofsf(oh_ofsf)


  oh_clean <-   oh_clean |>
      dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  

  # Run validation tests
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}