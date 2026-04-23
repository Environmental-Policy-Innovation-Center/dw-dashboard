clean_oh_y3 <- function() {
  
  base_path <- "year3/OH/data"
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table
  
  not_ef_list <- c("44", "59", "77", "101", "103", "110", "215", "227", "247", "258", "276", "294",
                   "334", "367", "451", "461", "468", "479", "504", "NF1")
  
  ec_sdc_list <- c("132", "59", "93", "109", "478", "383", "30", "443", "421", "506", "491", "SDC1", "SDC2")

  oh_missing_not_funded <- data.table::fread(file.path(base_path, "oh-missing-not-funded.csv"),
                                 colClasses = "character", na.strings = "") |>
    janitor::clean_names()
  
  # import SDC-funded EC projects, keep two projects not in fundable list to add in
  oh_ec_sdc <- data.table::fread(file.path(base_path, "oh-sdc-ec.csv"),
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::filter(entity %in% c("Beverly", "Oxford")) |>
    dplyr::rename(estimated_loan_amount = sdc_ec_grant)
  
  # Comprehensive Project List
  oh_fundable <- data.table::fread(file.path(base_path, "oh-ppl-base.csv"),
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names()
  
    oh_fundable <- bind_rows(oh_fundable, oh_missing_not_funded, oh_ec_sdc) |>
      dplyr::mutate(borrower = stringr::str_squish(entity),
           project_description = stringr::str_squish(project),
           population = clean_numeric_string(sdwis_population),
           pwsid = stringr::str_squish(pws_id),
           community_served = stringr::str_squish(county),
           expecting_funding = ifelse(epic_project_id %in% not_ef_list, "No", "Yes"),
           # SDC-funded list over-rides not expecting funding list
           expecting_funding = ifelse(epic_project_id %in% ec_sdc_list, "Yes", expecting_funding),
           requested_amount = clean_numeric_string(estimated_loan_amount)
           ) |>
      dplyr::select(epic_project_id, borrower, project_description, requested_amount,
           population, pwsid, community_served, expecting_funding,
           rate)
  
    #manually update an estimated loan amount from ec sdc per comment from policy team
    oh_fundable <- oh_fundable |>
      dplyr::mutate(requested_amount = ifelse(epic_project_id == "421", "888500", requested_amount))

  
  

  # DAC Principal Forgiveness List
  oh_dac <- data.table::fread(file.path(base_path, "oh-ppl-pf.csv"),
                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # removing numeric strings from estimated pf for the clean_numeric_string
    # function later
    dplyr::mutate(dac_pf = ifelse(estimated_principal_forgiveness %in% 
                             c("Bypass 1", "Bypass 2"), "Bypass", estimated_principal_forgiveness), 
           # these are all disadvantaged 
           disadvantaged = "Yes",
           list = "SFY25 DAC PPL") |> 
    dplyr::select(epic_project_id, disadvantaged, project_score, rate, 
           dac_pf, list)
  
  # Regionalization Principal Forgiveness List
  oh_reg <- data.table::fread(file.path(base_path, "oh-ppl-regional-pf.csv"),
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # NOTE - there was one project "Phillipsburg - Drinking Water PFAS Remediation"
    # that didn't have a project ID but ends up on the ec_extra list, so it's safe
    # to filter here
    filter(!is.na(epic_project_id)) |>
    dplyr::mutate(reg_pf = ifelse(estimated_principal_forgiveness %in% 
                             c("Bypass 1", "Bypass 2"), "Bypass", estimated_principal_forgiveness),
           list = "SFY25 Regionalization PPL"
           ) |>
    dplyr::select(epic_project_id, reg_pf, project_score, rate, list)
  
  # discount list - NOTE - this table is not really mentioned in the DD (except
  # for project_type) and the regionalization list already has that information, 
  # so opting to skip adding this table. 
  # oh_dis <- data.table::fread(file.path(base_path, "oh-discount.csv"),
  #                 colClasses = "character", na.strings = "") |>
  #   janitor::clean_names()
  
  # merging DAC & regionalization PPLs to handle pf cols: 
  oh_dac_reg <- merge(oh_dac, oh_reg, by = "epic_project_id", all = T) |>
    # pasting rates together for string matching 
    dplyr::mutate(rate = paste0(rate.x, rate.y), 
           # can confirm project scores are the same where the lists overlap
           project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                     TRUE ~ project_score.x), 
           # handling pf overlaps 
           principal_forgiveness = case_when(!is.na(reg_pf) & !(reg_pf %in% c("Bypass", "EC List")) ~ reg_pf, 
                                             !is.na(dac_pf) & !(dac_pf %in% c("Bypass", "EC List", "REG List")) ~ dac_pf),
           list = ifelse(!is.na(list.y), list.y, list.x)
           ) |>
    # trimming extra cols: 
    dplyr::select(-c(rate.x, dac_pf, reg_pf, project_score.y, project_score.x, rate.y, list.x, list.y))
  
  
  # merging these together for project types:
  oh_main <- merge(oh_fundable, oh_dac_reg, by = "epic_project_id", all = T) |>
    dplyr::mutate(rate = paste0(rate.x, rate.y),
           project_type = case_when(grepl("LSL", rate, ignore.case = T) ~ "Lead", 
                                    grepl("HAB|EC|PFAS", rate, ignore.case = T) ~ "Emerging Contaminants", 
                                    grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
                                    grepl("lead|lsl", project_description, ignore.case=TRUE) ~ "Lead",
                                    TRUE ~ "General")) |>
    # trim extra columns
    select(-c(rate.x, rate.y, rate))

  # Emerging Contaminants List
  oh_ec <- data.table::fread(file.path(base_path, "oh-ppl-ecr.csv"),
                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # these are all not on the fundable list but will be added later
    dplyr::filter(grepl("$", estimated_ec_amount)) |>
    dplyr::mutate(project_type = "Emerging Contaminants",
           list = "SFY25 EC PPL",
           ec_pf = ifelse(est_ec_principal_forgiveness %in% 
                            c("Bypass 1", "Bypass 2"), "Bypass", est_ec_principal_forgiveness),
           project_score = stringr::str_squish(score_total_points)) |>
    dplyr::select(epic_project_id, ec_pf, project_score, project_type, list)

  # Get extra EC projects that don't match fundable list & prepping for 
  # a bind_rows later: 
  oh_ec_extra <- data.table::fread(file.path(base_path, "oh-ppl-ecr.csv"),
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::filter(!grepl("$", estimated_ec_amount)) |>
    dplyr::mutate(borrower = str_squish(entity),
           project_description = stringr::str_squish(project),
           pwsid = stringr::str_squish(pws_id),
           community_served = stringr::str_to_sentence(county),
           project_score = stringr::str_squish(score_total_points),
           project_type = "Emerging Contaminants",
           list = "SFY25 EC PPL",
           requested_amount = clean_numeric_string(estimated_loan_amount),
           disadvantaged = "No",
           expecting_funding = "No",
           # principal_forgiveness = clean_numeric_string(est_ec_principal_forgiveness),
           principal_forgiveness = "No Information",
           population = "No Information") |>
    dplyr::select(borrower, project_description, pwsid, community_served,
           project_score, project_type, disadvantaged,requested_amount,
           expecting_funding, principal_forgiveness, population, list)
  
  # merging ec list and our main list to handle pf cols: 
  oh_main_ec <- merge(oh_main, oh_ec, by = "epic_project_id", all = T) |>
    # fixing project types
    dplyr::mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                   TRUE ~ project_type.x), 
           # can confirm pf does not overlap
           principal_forgiveness = case_when(!is.na(ec_pf) ~ ec_pf, 
                                             TRUE ~ principal_forgiveness), 
           # note the is a project w/ the desscription "PFAS Well Mitigation" 
           # that has a different project score from the DAC PPL. Confirmed 
           # that it should receive the project score on the EC list
           project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                     TRUE ~ project_score.x),
           list = ifelse(!is.na(list.y), list.y, list.x)) |>
    dplyr::select(-c(project_type.x, project_type.y, project_score.x, project_score.y, 
              ec_pf, list.x, list.y))

  # Lead Service Line List  
  oh_lead <- data.table::fread(file.path(base_path, "oh-lslr.csv"),
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # these are all lead projects
    dplyr::mutate(project_type = "Lead",
           list = "SFY25 Lead PPL",
           disadvantaged = ifelse(grepl("PF", rate), "Yes", "No"),
           # if PF present, eligible for PF, but unknown how much, so No Info
           principal_forgiveness = ifelse(grepl("PF", rate), "No Information", as.character(NA))) |>
    dplyr::select(epic_project_id, project_type, disadvantaged, principal_forgiveness, list)

  # Combine fundable, dc, regionalization, and discount ppls: 
  oh_comp <- merge(oh_main_ec, oh_lead, by = "epic_project_id", all = T) |>
    # default to project type for lead or ec lists: 
    dplyr::mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y,
                                    TRUE ~ project_type.x),
           disadvantaged = ifelse(!is.na(disadvantaged.y), disadvantaged.y, disadvantaged.x),
           list = ifelse(!is.na(list.y), list.y, list.x),
           principal_forgiveness = ifelse(!is.na(principal_forgiveness.y), principal_forgiveness.y, principal_forgiveness.x)
           )  |>
    # remove extra columns
    dplyr::select(-c("project_type.x", "project_type.y", "disadvantaged.x", "disadvantaged.y",
              "principal_forgiveness.x", "principal_forgiveness.y", "list.x", "list.y"))
  
  # Add extra EC projects and final cleanup
  oh_clean <- bind_rows(oh_comp, oh_ec_extra) |>
    dplyr::mutate(
           principal_forgiveness = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "0", 
                                             TRUE ~ principal_forgiveness)) |>
    dplyr::mutate(state = "Ohio",
           state_fiscal_year = "2025",
           # cleaning numeric strings: 
           principal_forgiveness = replace_na(principal_forgiveness, "0"),
           # set PF to 0 if not expecting funding
           principal_forgiveness = ifelse(epic_project_id %in% not_ef_list, "0", principal_forgiveness),
           project_score = clean_numeric_string(project_score),
           # fill in extra cols: 
           funding_amount = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           disadvantaged = ifelse(is.na(disadvantaged), "No", disadvantaged),
           project_rank = as.character(NA),
           list = replace_na(list, "SFY25 Fundable List and Comprehensive List")) |>
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