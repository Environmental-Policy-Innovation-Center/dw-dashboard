clean_oh_y0 <- function() {
  
  # ohio EC string from data dictionary 
  oh_ec_str <- "cyanotoxin|dioxane|emerging contaminant|lithium|manganese|Mn|Perfluoro-n-pentanoic acid|PFPeA|PFAS|PFOA|PFOS|trihalomethanes|THM|Unregulated Contaminant Monitoring Rule|DBP|HAA5|haloacetic acid"
  
  # dac PPL: 
  oh_dac <- fread("./year0/OH/data/oh_dac_ppl.csv") %>%
    janitor::clean_names() %>%
    mutate(disadvantaged = "Yes") %>%
           # principal_forgiveness = clean_numeric_string(principal_forgiveness)) %>%
    select(epic_project_id, disadvantaged, 
           principal_forgiveness, project_score, rate)
  
  # regionalization projects: 
  oh_reg <- fread("./year0/OH/data/oh_regionalization_ppl.csv") %>%
    janitor::clean_names() %>%
    # mutate(principal_forgiveness = clean_numeric_string(estimated_principal_forgiveness)) %>%
    select(epic_project_id, project_score, rate, estimated_principal_forgiveness)
  
  # merging dac and reg ppls together and handling merged columns: 
  oh_dac_reg <- merge(oh_dac, oh_reg, by = "epic_project_id", all = T) %>%
    mutate(project_score = case_when(is.na(project_score.y) ~ project_score.x, 
                                     is.na(project_score.x) ~ project_score.y, 
                                     TRUE ~ NA), 
           rate = paste0(rate.x, rate.y), 
           principal_forgiveness = case_when(!is.na(principal_forgiveness) ~ principal_forgiveness, 
                                             TRUE ~ estimated_principal_forgiveness)) %>%
    select(-c("project_score.x", "project_score.y", 
              "rate.x", "rate.y", "estimated_principal_forgiveness"))
  
  # projects eligible for funding: 
  oh_fundable <- fread("./year0/OH/data/oh_comp_ppl.csv") %>%
    janitor::clean_names() %>%
    # all of these are expecting funding: 
    mutate(expecting_funding = "Yes", 
           funding_amount = estimated_loan_amount) %>%
    # removing some extra cols we don't need
    select(!c("loan_type", "estimated_award_date", "district_office"))
  
  # merging fundable and dac_reg: 
  oh_comp <- merge(oh_fundable, oh_dac_reg, by = "epic_project_id", all = T) %>%
    mutate(rate = paste0(rate.x, rate.y)) %>%
    mutate(project_type = case_when(grepl("HAB", rate) ~ "Emerging Contaminants", 
                                    grepl("LSL", rate) ~ "Lead", 
                                    grepl(oh_ec_str, project) ~ "Emerging Contaminants", 
                                    TRUE ~ "General")) %>%
    select(-c("rate.x", "rate.y", "rate"))
    
  
  # HAB/PFAS list: 
  oh_ec <- fread("./year0/OH/data/oh_pfas_ppl.csv") %>%
    janitor::clean_names() %>%
    mutate(project_type = "Emerging Contaminants") %>%
    select(epic_project_id, project_type)
  
  # LSL list: note there is one project that does not appear on the fundable 
  # list 
  oh_lead_all <- fread("./year0/OH/data/oh_lead_ppl.csv") %>%
    janitor::clean_names() %>%
    mutate(project_type = "Lead")
  
  # these are the fundable ones
  oh_lead_fundable <- oh_lead_all %>%
    filter(epic_project_id %in% oh_fundable$epic_project_id) %>%
    select(epic_project_id, project_type)
  
  # not funable ones but prepping for an bind_rows later
  oh_lead_not_fundable <- oh_lead_all %>%
    filter(!(epic_project_id %in% oh_fundable$epic_project_id)) %>%
    mutate(disadvantaged = "No") %>%
    select(-c("loan_type", "estimated_award_date", "district_office"))
  
  # combing ec and lead: 
  oh_lead_ec <- bind_rows(oh_ec, oh_lead_fundable)
  
  # bring everything back together
  oh_clean_lead_ec <- merge(oh_comp, oh_lead_ec, by = "epic_project_id", all = T) %>%
    # handle project types: 
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    is.na(project_type.y) ~ project_type.x, 
                                    is.na(project_type.x) ~ project_type.y))%>%
    select(-c("project_type.x", "project_type.y"))
  
  # bringing back rogue lead project: 
  oh_clean <- bind_rows(oh_clean_lead_ec, oh_lead_not_fundable) %>%
    # process numeric cols: 
    mutate(principal_forgiveness = clean_numeric_string(principal_forgiveness), 
           requested_amount = clean_numeric_string(estimated_loan_amount), 
           funding_amount = clean_numeric_string(funding_amount), 
           population = clean_numeric_string(population),
           project_score = clean_numeric_string(project_score), 
           # process ifelse cols: 
           disadvantaged = ifelse(is.na(disadvantaged), "No", disadvantaged), 
           expecting_funding = ifelse(is.na(expecting_funding), "No", expecting_funding), 
           # tidy string cols: 
           pwsid = pws_id, 
           borrower = entity, 
           community_served = county,
           project_id = as.character(NA), 
           project_name = as.character(NA),
           project_cost = as.character(NA), 
           project_description = project, 
           project_rank = as.character(NA), 
           state = "Ohio", 
           state_fiscal_year = "2022") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}