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
    dplyr::mutate(expecting_funding = ifelse(epic_project_id %in% not_ef_list, "No", "Yes"),
           expecting_funding = ifelse(epic_project_id %in% ec_sdc_list, "Yes", expecting_funding)) |>
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
  
  # projects eligible for regionalizaton discount - this doesn't really provide 
  # any info that isn't already captured by our lists
  # oh_reg_disc <- data.table::fread(file.path(base_path, "oh_regionalization_ppl.csv")) |>
  #   janitor::clean_names()
  
  # merging fundable and dac lists: 
  oh_fund_dac <- merge(oh_fundable, oh_dac, by = "epic_project_id", all = TRUE) |>
    # pasting the rates together for string matching
    dplyr::mutate(rate = paste0(rate.x, rate.y), 
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
              project_score.y, disadvantaged.x, disadvantaged.y, list.x, list.y)) |>
    # fixing the expecting funding columns based on the presence of "Bypass" 
    # or "*" in the PF columns 
    # link to thread where we decided this: https://enviropolicyinno.slack.com/archives/C08LXGF02AE/p1762974431302769?thread_ts=1759336213.263239&cid=C08LXGF02AE
    dplyr::mutate(expecting_funding = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "No", 
                                         TRUE ~ expecting_funding),
           principal_forgiveness = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "0", 
                                             TRUE ~ principal_forgiveness))
  
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
  
  
  # import one project that wasn't already in the PPL from list of not expecting funding projects
  missing_not_funded_projects <- data.table::fread("year4/OH/data/oh-missing-not-funded.csv",
                                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY24 Comprehensive List",
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
           state_fiscal_year = "2026") |>
      dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  oh_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
  
  # Check for lead subtypes: Both
  oh_clean |>
    dplyr::filter(project_type=="Lead") |>
    dplyr::mutate(
      lead_type = dplyr::case_when(
        stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
        stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
        stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
        # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
        stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
        stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::filter(lead_type == "both")

  ####### Decision: No lead projects classified as both
  
  # Check for lead subtypes: Unknown
  oh_clean |>
    dplyr::filter(project_type=="Lead") |>
    dplyr::mutate(
      lead_type = dplyr::case_when(
        stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
        stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
        stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
        # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
        stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
        stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::filter(lead_type == "unknown")


  ####### SANITY CHECKS END #######

  # Run validation tests
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}