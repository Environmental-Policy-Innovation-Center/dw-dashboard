source("resources.R")

clean_ca_y2 <- function() {
  
  
  # (146, 13) -> (146, 3) 
  # this is base/Supp PPL in the data dictionary 
  fund_list <- read.csv("./year2/CA/data/tabula-REVISED-dwsrf-fundable-list.csv") %>%
    clean_names() %>%
    mutate(principal_forgiveness = clean_numeric_string(estimated_pf_grant_amount), 
           expecting_funding = "Yes", 
           project_id = case_when(
             # fixing mismatched project numbers before merge- can confirm the 
             # characteristics of these projects match with comp_list
             project_number == "EDWG-1000187-001C" ~ "1000187-001C", 
             project_number == "EDWG-5400903-001C" ~ "5400903-001C", 
             TRUE ~ project_number)) %>%
    select(project_id, principal_forgiveness, expecting_funding)
  
  
  # (352, 11) -> (352, 18)
  comp_list <- read.csv("./year2/CA/data/tabula-REVISED-dwsrf-comprehensive-list.csv") %>%
    clean_names() %>% 
    mutate(borrower = str_squish(applicant), 
           pwsid = as.character(NA),
           project_id = str_squish(project_number), 
           project_type = as.character(NA), 
           project_cost = as.character(NA), 
           requested_amount = clean_numeric_string(estimated_required_project_costs), 
           project_description = str_squish(project_title_description), 
           # there are three projects w/o populations 
           population = clean_numeric_string(population), 
           # TODO: fix this when Janet gets back re: disadvantaged coding status
           disadvantaged = case_when(
             degree_of_disadvantaged %in% c("DAC", "SDAC") ~ "DAC", 
             TRUE ~ "No"
           ))
  

  # (23, 14) -> (22, 10) - there was one row that was a total
  ec <- read.csv("./year2/CA/data/tabula-2023-24-supp-iup-ec-p13-14.csv") %>%
    clean_names() %>%
    filter(project_number != "Total Emerging Contaminant Fundable List") %>%
    mutate(borrower = str_squish(applicant), # TODO <- flag for Janet
           project_id = str_squish(project_number),
           project_name = as.character(NA),     
           project_type = "Emerging Contaminants",
           project_cost = clean_numeric_string(estimated_total_project_costs), 
           principal_forgiveness = clean_numeric_string(estimated_maximum_pf_grant_amount5), 
           project_description = str_squish(project_title_description),
           # TODO - disadvantaged status?
           disadvantaged = case_when(
             degree_of_disadvantage %in% c("Severely Disadvantaged", "Disadvantaged") ~ "DAC", 
             TRUE ~ "No"
           ), 
           population = clean_numeric_string(population), 
           expecting_funding = "Yes") %>%
    select(borrower, project_id, project_name, project_type, project_cost, 
           principal_forgiveness, project_description, disadvantaged, population,
           expecting_funding)
  
  
  # (15, 9) -> (15, 6)
  # this is the replacement inventory list 
  lead_table_1 <- read.csv("./year2/CA/data/tabula-2023-24-supp-iup-lslr-p16.csv") %>%
    clean_names() %>%
    mutate(population = clean_numeric_string(population), 
           project_cost = clean_numeric_string(estimated_cost_to_replace_2), 
           project_description = "LSL Replacement") %>%
    select(water_system_id, water_system_name, project_cost, 
           project_description, population, degree_of_disadvantaged1)
  
  
  # (101, 9) -> (101, 11)
  # this is the investigation inventory list
  lead_table_2 <- read.csv("./year2/CA/data/tabula-2023-24-supp-iup-lslr-p17-20.csv") %>%
    clean_names() %>%
    mutate(project_cost = convert_to_numeric(estimated_costs_to_investigate_2) + convert_to_numeric(estimated_costs_to_replace_25_3),
           project_cost = clean_numeric_string(project_cost), 
           project_description = "LSL Investigation", 
           population = clean_numeric_string(population)) %>%
    select(water_system_id, water_system_name, project_cost, 
           project_description, population, degree_of_disadvantaged1)
  
  # -> (114, 11) - combining both lead tables together: 
  lead_ppl <- bind_rows(lead_table_1, lead_table_2) %>%
    filter(water_system_id != "Total") %>%
    mutate(borrower = str_squish(water_system_name), 
           pwsid = str_squish(water_system_id),
           project_name = as.character(NA), 
           project_type = "Lead", 
           requested_amount = as.character(NA), 
           funding_amount = as.character(NA), 
           principal_forgiveness = as.character(NA), 
           disadvantaged = case_when(
             degree_of_disadvantaged1 %in% c("DAC", "SDAC") ~ "DAC", 
             TRUE ~ "No"
           ), 
           expecting_funding = "No") %>%
    select(borrower, pwsid, project_name, project_type, project_cost, 
           requested_amount, funding_amount, principal_forgiveness,
           project_description, disadvantaged, expecting_funding)
  
  
  # -> (488, 18)
  ca_clean <- comp_list %>%
    left_join(fund_list, by = "project_id") %>%      
    mutate(
      project_type = case_when(
        expecting_funding == "Yes" ~ "General", 
        TRUE ~ "No Information"),
      funding_amount = case_when(
        expecting_funding == "Yes" ~ requested_amount, 
        TRUE ~ "No Information"),
      expecting_funding = case_when(
        # projects on comp list that are not on base/supp ppl or EC ppl are not fundable, but 
        # the ec list should not be merged w/ fundable list because the fundable 
        # list has a different project type based on the data dictionary 
        is.na(expecting_funding) & !(project_number %in% ec$project_number) ~ "No", 
        is.na(expecting_funding) & (project_number %in% ec$project_number) ~ "Yes", 
        TRUE ~ expecting_funding
      )
    ) %>%
    # bind with lead and ec data
    bind_rows(., lead_ppl, ec) %>%
    mutate(
      # TODO - replacing w/ "no information" could go earlier in this script 
      community_served = as.character(NA), 
      pwsid = replace_na(pwsid, "No Information"),
      project_id = replace_na(project_id, "No Information"),
      project_name = as.character(NA),       
      project_cost = clean_numeric_string(project_cost),
      requested_amount = clean_numeric_string(requested_amount),
      funding_amount = clean_numeric_string(funding_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      population = clean_numeric_string(population),
      project_rank = as.character(NA), 
      project_score = as.character(NA), 
      state = "California", 
      state_fiscal_year = "SFY24"
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(ca_clean)
  rm(list=setdiff(ls(), "ca_clean"))
  
  return(ca_clean)
}
