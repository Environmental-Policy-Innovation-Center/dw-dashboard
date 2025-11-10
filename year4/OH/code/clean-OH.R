clean_oh_y4 <- function() {
  
  base_path <- "year4/OH/data"
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table

  # fundable project List
  oh_fundable <- fread(file.path(base_path, "oh_comp_ppl.csv")) %>%
    clean_names() %>%
    # all of these are expecting funding
    mutate(expecting_funding = "Yes") %>%
    select(-c(estimated_award_date, loan_type, district_office))
  
  # DAC & regionalization list
  oh_dac <- fread(file.path(base_path, "oh_dac_ppl.csv")) %>%
    clean_names() %>%
    # these are all disadvantaged 
    mutate(disadvantaged = "Yes",
           # based on PPLs from past years, these should be "no info" but I 
           # need to remove the number for the clean_numeric_string function. 
           # Also note some are just $0.00 
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
    # pasting the rates together for string matching
    mutate(rate = paste0(rate.x, rate.y), 
           # string matching based on rate or project 
           project_type = case_when(grepl("LSL", rate) ~ "Lead",
                                    grepl("HAB|PFAS|EC", rate) ~ "Emerging Contaminants", 
                                    grepl(ec_str, project, ignore.case=TRUE) ~ "Emerging Contaminants", 
                                    TRUE ~ "General")) %>%
    select(-c("rate.x", "rate.y"))

  # EC list: 
  oh_ec <- fread(file.path(base_path, "oh_ec_ppl.csv")) %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           # removing numbers from bypass columns 
           ec_pf = ifelse(estimated_ec_principal_forgiveness %in% 
                             c("Bypass1", "Bypass2", "Bypass 3"), "Bypass", 
                          estimated_ec_principal_forgiveness)) %>%
    select(epic_project_id, project_type, ec_pf, project_score)
  
  
  # merging with oh_fund_dac to handle principal forgiveness columns: 
  oh_ec_fund_dac <- merge(oh_fund_dac, oh_ec, by = "epic_project_id", all = T) %>%
    # note there are some HAB projects that don't show up in the EC list, but 
    # are captured by our string matching 
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    TRUE ~ project_type.x), 
           # project scores don't overlap, so just combining columns here
           project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                     TRUE ~ project_score.x),
           # pf cols don't overlap, so just combining columns here
           principal_forgiveness = case_when(!is.na(dac_pf) ~ dac_pf, 
                                             !is.na(ec_pf) ~ ec_pf, 
                                             TRUE ~ NA)) %>%
    select(-c(project_type.y, project_type.x, dac_pf, ec_pf, project_score.x, 
              project_score.y)) %>%
    # fixing the expecting funding columns based on the presence of "Bypass" 
    # or "*" in the PF columns 
    # TODO - figure out how this affects the funding amount column, asked Q at 
    # 2pm Nov 10th and awaiting consensus from FT team
    mutate(expecting_funding = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "No", 
                                         TRUE ~ expecting_funding))
  
  # lead list: 
  oh_lead <- fread(file.path(base_path, "oh_lead_ppl.csv")) %>%
    clean_names() %>%
    # these are all lead projects
    mutate(project_type = "Lead") %>%
    select(epic_project_id, project_type)
  
  # bring it back now y'all
  oh_clean <- merge(oh_ec_fund_dac, oh_lead, by = "epic_project_id", all = T) %>%
    # resolving project type overlaps 
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