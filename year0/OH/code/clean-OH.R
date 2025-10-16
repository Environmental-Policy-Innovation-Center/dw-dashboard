clean_oh_y0 <- function() {
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table
  
  # dac PPL: 
  oh_dac <- fread("./year0/OH/data/oh_dac_ppl.csv") %>%
    janitor::clean_names() %>%
    # these are all disadvantaged
    mutate(disadvantaged = "Yes") %>%
    # selecting just the columns we need
    select(epic_project_id, disadvantaged, 
           principal_forgiveness, project_score, rate)
  
  # regionalization projects: 
  oh_reg <- fread("./year0/OH/data/oh_regionalization_ppl.csv") %>%
    janitor::clean_names() %>%
    select(epic_project_id, project_score, rate, 
           estimated_principal_forgiveness)
  
  # merging dac and reg ppls together since they may overlap: 
  oh_dac_reg <- merge(oh_dac, oh_reg, by = "epic_project_id", all = T) %>%
    # fill in gaps for project score - can projects on both lists have the
    # same project score 
    mutate(project_score = case_when(is.na(project_score.y) ~ project_score.x, 
                                     is.na(project_score.x) ~ project_score.y, 
                                     TRUE ~ NA), 
           # paste the rates together, which will be string matched to identify 
           # project type 
           rate = paste0(rate.x, rate.y), 
           # fill in gaps for PF
           principal_forgiveness = case_when(!is.na(principal_forgiveness) ~ principal_forgiveness, 
                                             TRUE ~ estimated_principal_forgiveness)) %>%
    # remove extra columns from the merge
    select(-c("project_score.x", "project_score.y", 
              "rate.x", "rate.y", "estimated_principal_forgiveness"))
  
  # projects eligible for funding (all projects will appear on 
  # this list, aside from one lead project)
  oh_fundable <- fread("./year0/OH/data/oh_comp_ppl.csv") %>%
    janitor::clean_names() %>%
    # all of these are expecting funding & should have a funding amount
    mutate(expecting_funding = "Yes", 
           funding_amount = estimated_loan_amount) %>%
    # removing some extra cols we don't need
    select(!c("loan_type", "estimated_award_date", "district_office"))
  
  # merging fundable, and the dac-regionalization lists
  oh_comp <- merge(oh_fundable, oh_dac_reg, by = "epic_project_id", all = T) %>%
    # adding the rates together for string matching
    mutate(rate = paste0(rate.x, rate.y)) %>%
    # finding the presence of HAB or LSL in the rate columns: 
    mutate(project_type = case_when(grepl("HAB", rate) ~ "Emerging Contaminants", 
                                    grepl("LSL", rate) ~ "Lead", 
                                    # and the other strings listed for the 
                                    # fundbale list
                                    grepl(ec_str, project, ignore.case=TRUE) ~ "Emerging Contaminants", 
                                    TRUE ~ "General")) %>%
    # trimming extra cols
    select(-c("rate.x", "rate.y", "rate"))
    
  
  # HAB/PFAS list: 
  oh_ec <- fread("./year0/OH/data/oh_pfas_ppl.csv") %>%
    janitor::clean_names() %>%
    # these are all EC
    mutate(project_type = "Emerging Contaminants") %>%
    select(epic_project_id, project_type)
  
  
  # LSL list: note there is one project that does not appear on the fundable 
  # list 
  oh_lead_all <- fread("./year0/OH/data/oh_lead_ppl.csv") %>%
    janitor::clean_names() %>%
    # these are all lead
    mutate(project_type = "Lead")
  
  # these are the lead projects that appear on the fundable list
  oh_lead_fundable <- oh_lead_all %>%
    filter(epic_project_id %in% oh_fundable$epic_project_id) %>%
    select(epic_project_id, project_type)
  
  # the not fundable project but prepping for a bind_rows later
  oh_lead_not_fundable <- oh_lead_all %>%
    filter(!(epic_project_id %in% oh_fundable$epic_project_id)) %>%
    mutate(disadvantaged = "No", 
           expecting_funding = "No") %>%
    select(-c("loan_type", "estimated_award_date", "district_office"))
  
  
  # combing ec and lead: 
  oh_lead_ec <- bind_rows(oh_ec, oh_lead_fundable)
  
  
  # bring everything back together
  oh_clean_lead_ec <- merge(oh_comp, oh_lead_ec, 
                            by = "epic_project_id", all = T) %>%
    # handle project types - note it seems like the state does not consider 
    # THM projects as EC. They do not appear on the EC list but are captured 
    # by our sting matching
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    is.na(project_type.y) ~ project_type.x, 
                                    is.na(project_type.x) ~ project_type.y))%>%
    select(-c("project_type.x", "project_type.y"))
  
  # bringing back the lead project that is not eligible for funding, and 
  # finish standardizing columns: 
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