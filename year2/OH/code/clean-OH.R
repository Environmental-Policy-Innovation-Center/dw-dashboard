clean_oh_y2 <- function() {
  
  base_path <- file.path("year2", "OH", "data")
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table
  
  # Comprehensive Project List
  oh_fundable <- fread(file.path(base_path, "oh-comprehensive-ppl.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(borrower = str_squish(entity),
           project_description = str_squish(project),
           funding_amount = clean_numeric_string(estimated_loan_amount),
           requested_amount = funding_amount,
           population = clean_numeric_string(sdwis_population),
           pwsid = str_squish(pws_id),
           # these are all expecting funding
           expecting_funding = "Yes",
           community_served = str_squish(county)) %>%
    select(epic_project_id, borrower, project_description, funding_amount, 
           population, pwsid, community_served, rate, requested_amount, expecting_funding)

  
  # DAC Principal Forgiveness List
  oh_dac_ppl <- fread(file.path(base_path, "oh-ppl-pf.csv"),
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(epic_project_id != "503") %>%
    # rename(dac_pf = estimated_principal_forgiveness) %>%
    mutate(dac_pf = ifelse(estimated_principal_forgiveness == "BYPASS 1", "BYPASS", estimated_principal_forgiveness), 
           # population = clean_numeric_string(population),
           disadvantaged = "Yes", 
           project_score = str_squish(project_score)) %>% 
    select(epic_project_id, dac_pf, disadvantaged, project_score, rate)
  
  # there is one project that just doesn't show up on the fundable list in 
  # Muskingum county - prepping for a bind_rows later: 
  extra_dac <- fread(file.path(base_path, "oh-ppl-pf.csv"),
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(epic_project_id == "503") %>%
    # this project isn't expecting funding but is DAC
    mutate(project_type = "General", 
           expecting_funding = "No", 
           disadvantaged = "Yes", 
           community_served = str_squish(county), 
           principal_forgiveness = "No Information", 
           funding_amount = "No Information") %>%
    # rename(principal_forgiveness = estimated_principal_forgiveness, 
    # funding_amount = estimated_loan_amount) %>%
    rename(borrower = entity, 
           project_description = project, 
           pwsid = pws_id) %>%
    select(-c(loan_type, estimated_award_date, readiness_to_proceed, 
              district_office, county, rate))
  
  
  # Regional Principal Forgiveness List
  oh_reg_ppl <- fread(file.path(base_path, "oh-ppl-regional-pf.csv"),
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(reg_pf = ifelse(estimated_principal_forgiveness == "BYPASS 1", "BYPASS", estimated_principal_forgiveness), 
           project_score = str_squish(project_score)) %>%
    select(epic_project_id, reg_pf, project_score, rate)
  
  # combining dac and regional 
  oh_dac_reg <- merge(oh_dac_ppl, oh_reg_ppl, all = T) %>%
    # fixing pf overlaps: 
    mutate(principal_forgiveness = case_when(!is.na(reg_pf) & !(reg_pf %in% c("See DIS List", "BYPASS")) ~ reg_pf, 
                                             TRUE ~ dac_pf)) %>%
    select(-c(dac_pf, reg_pf))
  
  
  # combining fundable, dac, and regionalization ppls:
  oh_comp <- merge(oh_fundable, oh_dac_reg, by ="epic_project_id", all = T) %>%
    mutate(rate = paste0(rate.x, rate.y)) %>% 
    mutate(project_type = case_when(grepl(ec_str, project_description, ignore.case=TRUE) | grepl("HAB|PFAS|EC", rate, ignore.case = T) ~ "Emerging Contaminants",
                                    grepl("lead|LSL", rate, ignore.case = T) ~ "Lead",
                                    TRUE ~ "General")) %>% 
    select(-c(rate.x, rate.y, rate))
  
  # adding that extra dac project: 
  oh_comp_ext <- bind_rows(oh_comp, extra_dac)
  

  # Emerging Contaminants List
  oh_ec <- fread(file.path(base_path, "oh-ppl-ecr.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # these are projects that will join to comp 
    filter(grepl("$", estimated_ec_amount)) %>%
    # these are all ec projects
    mutate(project_type = "Emerging Contaminants",
           project_score = str_squish(score_total_points)) %>%
    select(epic_project_id, project_type, 
           est_ec_principal_forgiveness, project_score)
  
  # read in EC list again for the EC projects that didn't match to a project on 
  # the comprehensive list and won't join onto comp,
  # these are added in at the end
  oh_ec_extra <- fread(file.path(base_path, "oh-ppl-ecr.csv"),
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(!grepl("$", estimated_ec_amount)) %>%
    mutate(borrower = str_squish(entity),
           project_description = str_squish(project),
           pwsid = str_squish(pws_id),
           community_served = str_to_sentence(county),
           project_score = str_squish(score_total_points),
           project_type = "Emerging Contaminants",
           disadvantaged = "No", # can confirm all of these are not on the DAC list
           expecting_funding = "No",
           funding_amount = "No Information",
           principal_forgiveness = "No Information",
           population = "No Information") %>%
    select(borrower, project_description, pwsid, community_served, 
           project_score, project_type, disadvantaged,
           expecting_funding, funding_amount, principal_forgiveness, population)


  # Lead Service Line List
  oh_lsl <- fread(file.path(base_path, "oh-lslr.csv"),
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead") %>%
    select(epic_project_id, project_type)
  
  # combine lead and ec to keep project_type and dac columns from duplicating
  oh_ec_lsl <- bind_rows(oh_ec, oh_lsl)
  
  # add onto comp list with epic id, then process the conditional columns
  oh_comp_almostclean <- merge(oh_comp_ext, oh_ec_lsl, 
                               by = "epic_project_id", all = T) %>%
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    TRUE ~ project_type.x), 
           # these two columns don't overlap 
           project_score = case_when(!is.na(project_score.x) ~ project_score.x, 
                                     !is.na(project_score.y) ~ project_score.y),
           # non-numeric strings have been set to No Info by clean_numeric_string
           principal_forgiveness = case_when(!is.na(est_ec_principal_forgiveness) ~ clean_numeric_string(est_ec_principal_forgiveness),
                                             TRUE ~ principal_forgiveness)) %>%
    select(-c(project_type.x, project_type.y, project_score.x, project_score.y,
              est_ec_principal_forgiveness)) 
  
  # add extra EC rows back in and finish cleaning up columns
  oh_clean <- bind_rows(oh_comp_almostclean, oh_ec_extra) %>%
    # fixing the expecting funding columns based on the presence of "Bypass" 
    # or "*" in the PF columns 
    # link to thread where we decided this: https://enviropolicyinno.slack.com/archives/C08LXGF02AE/p1762974431302769?thread_ts=1759336213.263239&cid=C08LXGF02AE
    mutate(expecting_funding = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "No", 
                                         TRUE ~ expecting_funding), 
           funding_amount = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "No Information", 
                                      TRUE ~ funding_amount),
           principal_forgiveness = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "No Information", 
                                             TRUE ~ principal_forgiveness)) %>%
    mutate(state = "Ohio",
           state_fiscal_year = "2024",
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           project_rank = as.character(NA),
           # cleaning numeric strings
           project_score = clean_numeric_string(project_score), 
           # the warning message here is okay - just weird strings that 
           # got replaced with "No Information"
           principal_forgiveness = clean_numeric_string(principal_forgiveness), 
           requested_amount = clean_numeric_string(requested_amount), 
           funding_amount = clean_numeric_string(funding_amount), 
           population = clean_numeric_string(population),
           disadvantaged = ifelse(is.na(disadvantaged), "No", disadvantaged)) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
    ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  oh_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
    
  ####### SANITY CHECKS END #######

  # Run validation tests
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}
