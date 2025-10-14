clean_oh_y3 <- function() {
  base_path <- "year3/OH/data"

  # ohio EC string from data dictionary 
  oh_ec_str <- "cyanotoxin|dioxane|emerging contaminant|lithium|manganese|Mn|Perfluoro-n-pentanoic acid|PFPeA|PFAS|PFOA|PFOS|trihalomethanes|THM|Unregulated Contaminant Monitoring Rule|DBP|HAA5|haloacetic acid"
  
  # Comprehensive Project List
  oh_fundable <- fread(file.path(base_path, "oh-ppl-base.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(borrower = str_squish(entity),
           project_description = str_squish(project),
           requested_amount = clean_numeric_string(estimated_loan_amount),
           funding_amount = clean_numeric_string(estimated_loan_amount),
           population = clean_numeric_string(sdwis_population),
           pwsid = str_squish(pws_id),
           community_served = str_squish(county), 
           expecting_funding = "Yes") %>%
    select(epic_project_id, borrower, project_description, requested_amount,
           funding_amount, population, pwsid, community_served, expecting_funding, 
           rate)

  # DAC Principal Forgiveness List
  oh_dac <- fread(file.path(base_path, "oh-ppl-pf.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(dac_pf = estimated_principal_forgiveness) %>%
    mutate(dac_pf = ifelse(dac_pf %in% c("Bypass 1", "Bypass 2"), "Bypass", dac_pf), 
           disadvantaged = "Yes", 
           project_score = clean_numeric_string(project_score)) %>%
    mutate(dac_clean_pf = clean_numeric_string(dac_pf)) %>%
    select(epic_project_id, disadvantaged, project_score, rate, 
           dac_clean_pf)

  # Regional Principal Forgiveness List
  oh_reg <- fread(file.path(base_path, "oh-ppl-regional-pf.csv"),
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # NOTE - there was one project "Phillipsburg - Drinking Water PFAS Remediation"
    # that didn't have a project ID but ends up on the ec_extra list, so it's safe
    # to filter here
    filter(!is.na(epic_project_id)) %>%
    rename(reg_pf = estimated_principal_forgiveness) %>%
    mutate(reg_pf = ifelse(reg_pf %in% c("Bypass 1", "Bypass 2"), "Bypass", reg_pf), 
           reg_clean_pf = clean_numeric_string(reg_pf),
           project_score = clean_numeric_string(project_score)) %>%
    select(epic_project_id, reg_clean_pf, project_score, rate)
  
  # discount list - NOTE - this table is not really mentioned in the DD (except
  # for project_type) and the regionalization list already has that information, 
  # so opting to skip adding this table. 
  # oh_dis <- fread(file.path(base_path, "oh-discount.csv"),
  #                 colClasses = "character", na.strings = "") %>%
  #   clean_names()
  
  # combingng regionalization and those eligible for regionalization discount: 
  # oh_reg_dis <- merge(oh_reg, oh_dis, by = c("epic_project_id", "entity", 
  #                                            "project", "pws_id", 
  #                                            "estimated_loan_amount"), all = T)
  
  # merging these four together for project types:
  oh_main <- merge(oh_fundable, oh_dac, by = "epic_project_id", all = T)
  oh_main_f <- merge(oh_main, oh_reg, by = "epic_project_id", all = T) %>%
    mutate(rates_all = paste0(rate, rate.x, rate.y), 
           project_type = case_when(grepl("LSL", rates_all) ~ "Lead", 
                                    grepl("HAB|EC|PFAS", rates_all) ~ "Emerging Contaminants", 
                                    grepl(oh_ec_str, project_description) ~ "Emerging Contaminants",
                                    TRUE ~ "General"), 
           project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                     is.na(project_score.y) & !is.na(project_score.x) ~ project_score.x, 
                                     TRUE ~ "No Information"), 
           principal_forgiveness = case_when(dac_clean_pf == "No Information" | is.na(dac_clean_pf) & !is.na(reg_clean_pf) ~ reg_clean_pf, 
                                             reg_clean_pf == "No Information" | is.na(reg_clean_pf) & !is.na(dac_clean_pf)~ dac_clean_pf, 
                                             # these will all get transformed to "no information" later
                                             TRUE ~ NA)) %>%
    select(-c(project_score.x, project_score.y, rate.y, rate.x,
              rate, rates_all, dac_clean_pf, reg_clean_pf))
  
  
  # Emerging Contaminants List
  oh_ec <- fread(file.path(base_path, "oh-ppl-ecr.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           est_ec_principal_forgiveness = ifelse(est_ec_principal_forgiveness %in% c("Bypass 1", "Bypass 2"), "Bypass", est_ec_principal_forgiveness),
           project_score = str_squish(score_total_points), 
           principal_forgiveness = clean_numeric_string(est_ec_principal_forgiveness))

  # Get extra EC projects that don't match fundable list
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
           requested_amount = clean_numeric_string(estimated_loan_amount),
           disadvantaged = "No",
           expecting_funding = "No",
           funding_amount = "No Information",
           principal_forgiveness = clean_numeric_string(est_ec_principal_forgiveness),
           population = "No Information") %>%
    select(borrower, project_description, pwsid, community_served,
           project_score, project_type, disadvantaged,requested_amount,
           expecting_funding, funding_amount, principal_forgiveness, population)

  # Lead Service Line List  
  oh_lead <- fread(file.path(base_path, "oh-lslr.csv"),
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead")

  # Combine Lead and EC
  oh_ec_lsl <- bind_rows(oh_ec, oh_lead) %>%
    select(epic_project_id, principal_forgiveness, project_score, project_type)
  
  # Combine fundable, dc, regionalization, and discount ppls: 
  oh_comp <- merge(oh_main_f, oh_ec_lsl, by = "epic_project_id", all = T) %>%
    # default to project type for lead or ec lists: 
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y,
                                    TRUE ~ project_type.x), 
           # FLAG - project scores are different for "Putnam Community Water Corp 
           # PFAS Well Mitigation" - asked the DD group for insight
           # PWSID for Water Connection to Barnesville is just "OH"
           project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                     TRUE ~ project_score.x)) %>%
    # this is messy, but need to capture where we have pf info across different 
    # tables 
    mutate(principal_forgiveness = case_when(principal_forgiveness.x == "No Information" | is.na(principal_forgiveness.x) & !is.na(principal_forgiveness.y) ~ principal_forgiveness.y, 
                                             principal_forgiveness.y == "No Information" | is.na(principal_forgiveness.y) & !is.na(principal_forgiveness.x) ~ principal_forgiveness.x, 
                                             TRUE ~ "No Information"))%>%
    mutate(principal_forgiveness = clean_numeric_string(principal_forgiveness)) %>%
    # remove extra columns
    select(-c("project_type.x", "project_type.y", 
              "project_score.x", "project_score.y", "principal_forgiveness.y",
              "principal_forgiveness.x")) 
  
  # Add extra EC projects and final cleanup
  oh_clean <- bind_rows(oh_comp, oh_ec_extra) %>%
    mutate(state = "Ohio",
           state_fiscal_year = "2025",
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           disadvantaged = ifelse(is.na(disadvantaged), "No", disadvantaged),
           project_rank = as.character(NA)) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  # Run validation tests
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}