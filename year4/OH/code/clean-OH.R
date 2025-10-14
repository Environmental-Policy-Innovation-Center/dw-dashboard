clean_oh_y4 <- function() {
  base_path <- "year4/OH/data"

  # ohio EC string from data dictionary 
  oh_ec_str <- "cyanotoxin|dioxane|emerging contaminant|lithium|manganese|Mn|Perfluoro-n-pentanoic acid|PFPeA|PFAS|PFOA|PFOS|trihalomethanes|THM|Unregulated Contaminant Monitoring Rule|DBP|HAA5|haloacetic acid"
  
  # Comprehensive Project List
  oh_fundable <- fread(file.path(base_path, "oh_comp_ppl.csv")) %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(-c(estimated_award_date, loan_type, district_office))
  
  # DAC & regionalization list
  oh_dac <- fread(file.path(base_path, "oh_dac_ppl.csv")) %>%
    clean_names() %>%
    mutate(disadvantaged = "Yes",
           # based on PPLs from past years, these should be "no info" but I 
           # need to remove the number for the clean_numeric_string function
           dac_pf = ifelse(estimated_principal_forgiveness %in% 
                             c("Bypass1", "Bypass2", "Bypass 3"), "Bypass", 
                           estimated_principal_forgiveness)) %>%
    select(epic_project_id, dac_pf, project_score, rate, disadvantaged)
  
  # projects eligible for regionalizaton discount - this doesn't really provide 
  # any info that isn't already captured by our lists
  # oh_reg_disc <- fread(file.path(base_path, "oh_regionalization_ppl.csv")) %>%
  #   clean_names() 
  
  # merging fundable and dac lists: 
  oh_fund_dac <- merge(oh_fundable, oh_dac, by = "epic_project_id", all = T) %>%
    mutate(rate = paste0(rate.x, rate.y), 
           project_type = case_when(grepl("LSL", rate) ~ "Lead",
                                    grepl("HAB|PFAS|EC", rate) ~ "Emerging Contaminants", 
                                    grepl(oh_ec_str, project) ~ "Emerging Contaminants", 
                                    TRUE ~ "General")) %>%
    select(-c("rate.x", "rate.y"))

  
  # EC list: 
  oh_ec <- fread(file.path(base_path, "oh_ec_ppl.csv")) %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           # TODO - flag the stars - seems like projects to be funded by EC-SDC 
           # grant, should be no info?
           ec_pf = ifelse(estimated_ec_principal_forgiveness %in% 
                             c("Bypass1", "Bypass2", "Bypass 3"), "Bypass", 
                          estimated_ec_principal_forgiveness)) %>%
    select(epic_project_id, project_type, ec_pf, project_score)
  
  # merging with oh_fund_dac to handle principal forgiveness columns: 
  oh_ec_fund_dac <- merge(oh_fund_dac, oh_ec, by = "epic_project_id", all = T) %>%
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    TRUE ~ project_type.x), 
           project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                    TRUE ~ project_score.x),
           principal_forgiveness = case_when(!is.na(dac_pf) ~ dac_pf, 
                                             !is.na(ec_pf) ~ ec_pf, 
                                             TRUE ~ NA)) %>%
    select(-c(project_type.y, project_type.x, dac_pf, ec_pf, project_score.x, 
              project_score.y))
  
  # lead list: 
  oh_lead <- fread(file.path(base_path, "oh_lead_ppl.csv")) %>%
    clean_names() %>%
    mutate(project_type = "Lead") %>%
    select(epic_project_id, project_type)
  
  # bring it back now y'all
  oh_clean <- merge(oh_ec_fund_dac, oh_lead, by = "epic_project_id", all = T) %>%
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                             TRUE ~ project_type.x), 
           # process numeric cols:
           population = clean_numeric_string(sdwis_population), 
           project_score = clean_numeric_string(project_score), 
           funding_amount = clean_numeric_string(estimated_loan_amount), 
           requested_amount = funding_amount, 
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           # process character cols: 
           pwsid = pws_id, 
           project_description = project, 
           community_served = county, 
           borrower = entity, 
           project_id = as.character(NA), 
           project_name = as.character(NA), 
           project_cost = as.character(NA), 
           project_rank = as.character(NA), 
           disadvantaged = ifelse(is.na(disadvantaged), "No", disadvantaged),
           state = "Ohio",
           state_fiscal_year = "2026") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  # Run validation tests
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}