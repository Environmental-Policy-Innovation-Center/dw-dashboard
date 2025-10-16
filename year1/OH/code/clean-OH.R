clean_oh_y1 <- function() {
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table
  
  ## Base fundable list 
  oh_fundable <- fread("year1/OH/data/oh-ppl-base-id.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes", 
           funding_amount = estimated_loan_amount) %>%
    select(-c(loan_type, estimated_award_date))

  ## DAC PF
  oh_dac_ppl <- fread("year1/OH/data/oh-ppl-pf.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # these are all disadvantaged
    mutate(disadvantaged = "Yes") %>%
    select(epic_project_id, project_score, disadvantaged, rate, 
           estimated_principal_forgiveness)
  
  ## Regional PF
  oh_reg_ppl <- fread("year1/OH/data/oh-ppl-regional-pf.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(epic_project_id, estimated_principal_forgiveness, project_score, rate)
  
  
  # merging dac and reg together: 
  oh_dac_reg <- merge(oh_dac_ppl, oh_reg_ppl, all = T) 
  
  # merging this back to base: 
  oh_base_dac_reg <- merge(oh_fundable, oh_dac_reg, by = "epic_project_id", 
                           all = T) %>%
    mutate(rate = paste0(rate.x, rate.y), 
           project_type = case_when(grepl("LSL", rate) ~ "Lead", 
                                    grepl("HAB|PFAS", rate) ~ "Emerging Contaminants", 
                                    grepl(ec_str, project, ignore.case=TRUE) ~ "Emerging Contaminants", 
                                    # this project gets incorrectly categorized in the string match 
                                    project == "Watermain Imps Bun. 1 - Grange Hall Booster Station Wtr Mns" ~ "General", 
                                    TRUE ~ "General")) 
  
  
  # adding EC ppl:(9, 7)
  oh_pfas_ppl <- fread("year1/OH/data/oh-ppl-hab-pfas.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants") %>%
    select(epic_project_id, project_type)
  

  # adding lead ppl: (59, 8)
  oh_lead_ppl <- fread("year1/OH/data/oh-lslr.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>% 
    mutate(project_type = "Lead") %>%
    select(epic_project_id, project_type)
  
  # there is one project that does not appear on the fundable list - prepping 
  # it for a bind_rows
  oh_lead_extra <- fread("year1/OH/data/oh-lslr.csv",
                                         colClasses = "character", na.strings = "") %>%
    clean_names() %>% 
    filter(epic_project_id == "441") %>%
    mutate(project_type.y = "Lead", 
           expecting_funding = "No", 
           pwsid = "No Information", 
           population = "No Information",
           diadvantaged = "No") %>%
    select(-c(estimated_lsl_eligible_costs:rate))
  
  
  # binding the lead and ec together: 
  oh_lead_ec <- bind_rows(oh_lead_ppl, oh_pfas_ppl)
  
  # merging with the final list 
  oh_full <- merge(oh_base_dac_reg, oh_lead_ec, by = "epic_project_id", 
                    all = T) %>%
    bind_rows(., oh_lead_extra) 

  # final cleaning 
  oh_clean <- oh_full %>%
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    TRUE ~ project_type.x), 
           borrower = str_squish(entity),
           project_id = as.character(NA),
           project_name = as.character(NA),
           principal_forgiveness = clean_numeric_string(estimated_principal_forgiveness),
           requested_amount = clean_numeric_string(estimated_loan_amount),
           funding_amount = requested_amount,
           project_description = str_squish(project),
           community_served = str_squish(county),  
           project_cost = as.character(NA),
           population = clean_numeric_string(population),
           disadvantaged = case_when(is.na(disadvantaged) ~ "No", 
                                     TRUE ~ disadvantaged), 
           project_rank = as.character(NA), 
           project_score = clean_numeric_string(project_score),
           expecting_funding = case_when(is.na(expecting_funding) ~ "No", 
                                         TRUE ~ expecting_funding), 
           state = "Ohio", 
           state_fiscal_year = "2023") %>%
    # there was an extra column in there from the bind_rows
    filter(!is.na(borrower)) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year)

    ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  oh_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
    
  ####### SANITY CHECKS END #######

  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}

