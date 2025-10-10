clean_oh_y2 <- function() {
  base_path <- file.path("year2", "OH", "data")
  
  # ohio EC string from data dictionary 
  oh_ec_str <- "cyanotoxin|dioxane|emerging contaminant|lithium|manganese|Mn|Perfluoro-n-pentanoic acid|PFPeA|PFAS|PFOA|PFOS|trihalomethanes|THM|Unregulated Contaminant Monitoring Rule|DBP|HAA5|haloacetic acid"
  
  #NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
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
           expecting_funding = "Yes",
           community_served = str_squish(county)) %>%
    select(epic_project_id, borrower, project_description, funding_amount, 
           population, pwsid, community_served, rate, requested_amount, expecting_funding)

  
  # DAC Principal Forgiveness List
  # note there is one epic_project_id == NA in Muskingum County
  oh_dac_ppl <- fread(file.path(base_path, "oh-ppl-pf.csv"),
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(dac_pf = estimated_principal_forgiveness) %>%
    mutate(dac_pf = ifelse(dac_pf == "BYPASS 1", "BYPASS", dac_pf), 
           # population = clean_numeric_string(population),
           disadvantaged = "Yes", 
           project_score = str_squish(project_score)) %>% 
    select(epic_project_id, dac_pf, disadvantaged, project_score)
  
  
  # Regional Principal Forgiveness List
  oh_reg_ppl <- fread(file.path(base_path, "oh-ppl-regional-pf.csv"),
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(reg_pf = estimated_principal_forgiveness) %>%
    mutate(reg_pf = ifelse(reg_pf == "BYPASS 1", "BYPASS", reg_pf), 
           project_score = str_squish(project_score)) %>%
    select(epic_project_id, reg_pf, project_score)
  
  # combining fundable, dac, and regionalization ppls:
  oh_comp <- oh_fundable %>%
    left_join(oh_dac_ppl, by="epic_project_id") %>%
    left_join(oh_reg_ppl, by="epic_project_id") %>%
    mutate(project_type = case_when(grepl(oh_ec_str, project_description) | grepl("HAB|PFAS|EC", rate) ~ "Emerging Contaminants",
                                    grepl("lead|LSL", rate) ~ "Lead",
                                    TRUE ~ "General"), 
           project_score = case_when(!is.na(project_score.x) ~ project_score.x, 
                                     !is.na(project_score.y) ~ project_score.y)) %>%
    select(-c(project_score.x, project_score.y))

  
  # Emerging Contaminants List
  oh_ec <- fread(file.path(base_path, "oh-ppl-ecr.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
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
    select(borrower, project_description, pwsid, community_served, project_score, project_type, disadvantaged,
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
  oh_comp_almostclean <- oh_comp %>%
    left_join(oh_ec_lsl, by="epic_project_id") %>%
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    TRUE ~ project_type.x), 
           project_score = case_when(!is.na(project_score.x) ~ project_score.x, 
                                     !is.na(project_score.y) ~ project_score.y),
           project_score = clean_numeric_string(project_score), 
           disadvantaged = case_when(!is.na(epic_project_id) & epic_project_id %in% oh_dac_ppl$epic_project_id ~ "Yes",
                                     TRUE ~ "No"),
           # before processing PF, get rid of non-numeric strings 
           dac_pf = clean_numeric_string(dac_pf),
           reg_pf = clean_numeric_string(reg_pf),
           # non-numeric strings have been set to No Info by clean_numeric_string
           principal_forgiveness = case_when(dac_pf != "No Information" ~ dac_pf,
                                             reg_pf != "No Information" ~ reg_pf,
                                             !is.na(est_ec_principal_forgiveness) ~ clean_numeric_string(est_ec_principal_forgiveness),
                                             TRUE ~ "No Information")) %>%
    select(-c(project_type.x, project_type.y, project_score.x, project_score.y,
              dac_pf, reg_pf, est_ec_principal_forgiveness)) 
  
  # add extra EC rows back in and finish cleaning up columns
  oh_clean <- bind_rows(oh_comp_almostclean, oh_ec_extra) %>%
    mutate(
      state = "Ohio",
      state_fiscal_year = "2024",
      project_id = as.character(NA),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = as.character(NA),
      project_rank = as.character(NA),
      # project_score = replace_na(project_score, "No Information")
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
    
  # Run validation tests
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}
