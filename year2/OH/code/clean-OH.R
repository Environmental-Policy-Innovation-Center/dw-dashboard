clean_oh_y2 <- function() {
  
  base_path <- file.path("year2", "OH", "data")
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table
  
  not_ef_list <- c("56", "90", "195", "347", "461", "NF1", "NF2")
  
  # Comprehensive Project List
  oh_fundable <- data.table::fread(file.path(base_path, "oh-comprehensive-ppl.csv"),
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(borrower = stringr::str_squish(entity),
           project_description = stringr::str_squish(project),
           requested_amount = clean_numeric_string(estimated_loan_amount),
           population = clean_numeric_string(sdwis_population),
           pwsid = stringr::str_squish(pws_id),
           # these are all expecting funding
           expecting_funding = ifelse(epic_project_id %in% not_ef_list, "No", "Yes"),
           list = "SFY24 Fundable List and Comprehensive List",
           community_served = stringr::str_squish(county)) |>
    dplyr::select(epic_project_id, borrower, project_description, 
           population, pwsid, community_served, rate, requested_amount, expecting_funding, list)

  
  # DAC Principal Forgiveness List
  oh_dac_ppl <- data.table::fread(file.path(base_path, "oh-ppl-pf.csv"),
                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::filter(epic_project_id != "503") |>
    # rename(dac_pf = estimated_principal_forgiveness) |>
    dplyr::mutate(dac_pf = ifelse(estimated_principal_forgiveness == "BYPASS 1", "BYPASS", estimated_principal_forgiveness), 
           disadvantaged = "Yes", 
           list = "SFY24 DAC PPL",
           project_score = stringr::str_squish(project_score)) |> 
    dplyr::select(epic_project_id, dac_pf, disadvantaged, project_score, rate, list)
  
  # there is one project that just doesn't show up on the fundable list in 
  # Muskingum county - prepping for a bind_rows later: 
  extra_dac <- data.table::fread(file.path(base_path, "oh-ppl-pf.csv"),
                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::filter(epic_project_id == "503") |>
    # this project isn't expecting funding but is DAC
    dplyr::mutate(project_type = "General", #single project with known General type
           expecting_funding = "No", 
           disadvantaged = "Yes", 
           community_served = stringr::str_squish(county), 
           principal_forgiveness = "No Information",
           list = "SFY24 DAC PPL") |>
    dplyr::rename(borrower = entity, 
           project_description = project, 
           pwsid = pws_id) |>
    dplyr::select(-c(loan_type, estimated_award_date, readiness_to_proceed, 
              district_office, county, rate, list))
  
  
  # Regional Principal Forgiveness List
  oh_reg_ppl <- data.table::fread(file.path(base_path, "oh-ppl-regional-pf.csv"),
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(reg_pf = ifelse(estimated_principal_forgiveness == "BYPASS 1", "BYPASS", estimated_principal_forgiveness), 
           project_score = stringr::str_squish(project_score),
           list = "SFY24 Regionalization PPL") |>
    dplyr::select(epic_project_id, reg_pf, project_score, rate, list)
  
  # combining dac and regional 
  oh_dac_reg <- merge(oh_dac_ppl, oh_reg_ppl, all = T) |>
    # fixing pf overlaps: 
    dplyr::mutate(principal_forgiveness = case_when(!is.na(reg_pf) & !(reg_pf %in% c("See DIS List", "BYPASS")) ~ reg_pf, 
                                             TRUE ~ dac_pf),
           # convert all '-" entries to 0 specifically, then convert to string
           principal_forgiveness = clean_numeric_string((convert_to_numeric(principal_forgiveness, TRUE))),
           # PF defined by value unless on not funding list, then 0 (in this case all projects are 0 anyways)
           principal_forgiveness = ifelse(epic_project_id %in% not_ef_list, "0", principal_forgiveness),
           ) |>
    dplyr::select(-c(dac_pf, reg_pf))
  
  
  # combining fundable, dac, and regionalization ppls:
  oh_comp <- merge(oh_fundable, oh_dac_reg, by ="epic_project_id", all = T) |>
    dplyr::mutate(rate = paste0(rate.x, rate.y)) |> 
    dplyr::mutate(project_type = case_when(grepl(ec_str, project_description, ignore.case=TRUE) | grepl("HAB|PFAS|EC", rate, ignore.case = T) ~ "Emerging Contaminants",
                                    grepl("lead|LSL", rate, ignore.case = T) ~ "Lead",
                                    grepl("lead|LSL", project_description, ignore.case=T) ~ "Lead",
                                    TRUE ~ "General")) |> 
    dplyr::select(-c(rate.x, rate.y, rate))
  
  # adding that extra dac project: 
  oh_comp_ext <- bind_rows(oh_comp, extra_dac) |>
    dplyr::mutate(list = ifelse(is.na(list.y), list.x, list.y)) |>
    dplyr::select(-list.x, -list.y)
  

  # Emerging Contaminants List
  oh_ec <- data.table::fread(file.path(base_path, "oh-ppl-ecr.csv"),
                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # these are projects that will join to comp 
    dplyr::filter(grepl("$", estimated_ec_amount)) |>
    # these are all ec projects
    dplyr::mutate(project_type = "Emerging Contaminants",
           project_score = stringr::str_squish(score_total_points),
           # PF defined by value unless on not funding list, then 0 (in this case all projects are 0 anyways)
           principal_forgiveness = clean_numeric_string(convert_to_numeric(est_ec_principal_forgiveness,T)),
           principal_forgiveness = ifelse(epic_project_id %in% not_ef_list, "0", principal_forgiveness),
           list = "SFY24 EC PPL") |>
    dplyr::select(epic_project_id, project_type, 
           principal_forgiveness, project_score, list)
  
  # read in EC list again for the EC projects that didn't match to a project on 
  # the comprehensive list and won't join onto comp,
  # these are added in at the end
  oh_ec_extra <- data.table::fread(file.path(base_path, "oh-ppl-ecr.csv"),
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::filter(!grepl("$", estimated_ec_amount)) |>
    dplyr::mutate(borrower = stringr::str_squish(entity),
           project_description = stringr::str_squish(project),
           pwsid = stringr::str_squish(pws_id),
           community_served = stringr::str_to_sentence(county),
           project_score = stringr::str_squish(score_total_points),
           project_type = "Emerging Contaminants",
           disadvantaged = "No", # can confirm all of these are not on the DAC list
           # all projects had * in estimated amount column, therefore not expecting funding
           expecting_funding = "No",
           # PF defined by value unless on not funding list, then 0 (in this case all projects are 0 anyways)
           principal_forgiveness = clean_numeric_string(convert_to_numeric(est_ec_principal_forgiveness,T)),
           principal_forgiveness = ifelse(epic_project_id %in% not_ef_list, "0", principal_forgiveness),
           population = "No Information",
           list = "SFY24 EC PPL") |>
    dplyr::select(borrower, project_description, pwsid, community_served, 
           project_score, project_type, disadvantaged,
           expecting_funding, principal_forgiveness, population, list)


  # Lead Service Line List
  oh_lsl <- data.table::fread(file.path(base_path, "oh-lslr.csv"),
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(project_type = "Lead",
           # if PF is listed in rate, we know we don't know how much because it's based on a percent value of what's actually funded
           principal_forgiveness = ifelse(grepl("PF", rate), "No Information", as.character(NA)),
           list = "SFY24 Lead PPL") |>
    dplyr::select(epic_project_id, project_type, principal_forgiveness, list)
  
  # combine lead and ec to keep project_type and dac columns from duplicating
  oh_ec_lsl <- bind_rows(oh_ec, oh_lsl)
  
  # add onto comp list with epic id, then process the conditional columns
  oh_comp_almostclean <- merge(oh_comp_ext, oh_ec_lsl, 
                               by = "epic_project_id", all = T) |>
    dplyr::mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    TRUE ~ project_type.x), 
           # these two columns don't overlap 
           project_score = case_when(!is.na(project_score.x) ~ project_score.x, 
                                     !is.na(project_score.y) ~ project_score.y),
           # non-numeric strings have been set to No Info by clean_numeric_string
           principal_forgiveness = case_when(is.na(principal_forgiveness.x) ~ principal_forgiveness.y,
                                             TRUE ~ principal_forgiveness.x),
           principal_forgiveness = replace_na(principal_forgiveness, "0"),
           list = ifelse(is.na(list.y), list.x, list.y)
             ) |>
    dplyr::select(-c(project_type.x, project_type.y, project_score.x, project_score.y,
              -principal_forgiveness.x, -principal_forgiveness.y), -list.x, -list.y) 
  
  missing_not_funded_projects <- data.table::fread("year2/OH/data/oh-missing-not-funded.csv",
                                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::rename(borrower = entity,
           pwsid = pws_id,
           community_served = county) |>
    dplyr::mutate(list = "SFY24 Comprehensive List",
           project_description = str_squish(project),
           expecting_funding = "No",
           project_type = "General",
           requested_amount = clean_numeric_string(estimated_loan_amount)) |>
    dplyr::select(borrower, pwsid, community_served, list, requested_amount, project_type, expecting_funding, project_description)
  
  oh_comp_almostclean <- bind_rows(oh_comp_almostclean, missing_not_funded_projects)
  
  # add extra EC rows back in and finish cleaning up columns
  oh_clean <- bind_rows(oh_comp_almostclean, oh_ec_extra) |>
    # fixing the expecting funding columns based on the presence of "Bypass" 
    # or "*" in the PF columns 
    # link to thread where we decided this: https://enviropolicyinno.slack.com/archives/C08LXGF02AE/p1762974431302769?thread_ts=1759336213.263239&cid=C08LXGF02AE
    # please note there is a project id == "83" serving Cambridge that exists 
    # on the fundable list  & DAC PPL with a "See EC List" in the PF column,
    # but it does not show up on the EC list & therefore PF is no infomration
    dplyr::mutate(funding_amount = as.character(NA),
           principal_forgiveness = case_when(principal_forgiveness %in% c("BYPASS", "*") ~ "0", 
                                             TRUE ~ principal_forgiveness)) |>
    dplyr::mutate(state = "Ohio",
           state_fiscal_year = "2024",
           list = replace_na(list, "SFY24 Fundable List and Comprehensive List"),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           project_rank = as.character(NA),
           # cleaning numeric strings
           project_score = clean_numeric_string(project_score), 
           # all projects that aren't PF values or No Info lead projects are 0
           principal_forgiveness = replace_na(principal_forgiveness, "0"), 
           requested_amount = clean_numeric_string(requested_amount), 
           population = clean_numeric_string(population),
           disadvantaged = ifelse(is.na(disadvantaged), "No", disadvantaged)) |>
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
