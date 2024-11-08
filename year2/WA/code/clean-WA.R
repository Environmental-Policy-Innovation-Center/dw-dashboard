source("resources.R")

clean_wa_y2 <- function() {
  
  # (25, 13) --> (25, 14) 
  iup_b <- read.csv("year2/WA/data/tabula-Washington State IUP-appendixB.csv") %>%
    clean_names() %>%
    select(-starts_with("x")) %>%
    mutate(project_type = case_when(
      grepl("EC", comments) ~ "Emerging Contaminants", 
      TRUE ~ "General"))
  
  # (3, 12) --> (3, 13) 
  iup_ec <- read.csv("year2/WA/data/tabula-Washington State IUP-appendixC.csv") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants")
  
  # (4, 9)  --> (4, 11) - also known as preconstruction loans in data dictionary 
  iup_e <- read.csv("year2/WA/data/tabula-Washington State IUP-appendixE.csv") %>%
    clean_names() %>%
    # There isn't a comments column for this appendix, but based on project 
    # descriptions they don't appear to be EC 
    mutate(project_type = "General", 
           full_time_residential_population = population)
  
  
  # (1, 11) 
  lead_b <- read.csv("year2/WA/data/tabula-331-724-leadIUP-appendixB.csv") %>%
    clean_names()
  
  # (3, 10)
  lead_c <- read.csv("year2/WA/data/tabula-331-724-leadIUP-appendixC.csv") %>%
    clean_names() %>%
    # fixing a misspelled col name: 
    rename(health_application = heath_application)
  
  # --> (4, 13)
  lead_iup <- bind_rows(lead_b, lead_c) %>%
    mutate(project_type = "Lead", 
           # this is a temporary fix and would not work with larger datasets
           final_score = c("4", "1", "2", "3"))
  
  # --> (36, 18)
  wa_clean <- bind_rows(iup_b, iup_ec, iup_e) %>%
    mutate(final_score = as.character(final_score)) %>%
    bind_rows(lead_iup) %>%
    mutate(community_served = str_squish(county), 
           #fix typo
           community_served = case_when(
             community_served == "Snohomi sh" ~ "Snohomish",
             TRUE ~ community_served
           ),
           borrower = str_squish(applicant_name), 
           pwsid = case_when(
             # some of the leading 0s got dropped
             nchar(water_system_id) == 4 ~ paste0("WA5300", water_system_id), 
             TRUE ~ paste0("WA530", water_system_id)),
           project_id = str_squish(health_application), 
           project_name = as.character(NA), 
           project_cost = as.character(NA), 
           requested_amount = clean_numeric_string(loan_request_amount), 
           funding_amount = case_when(
             project_type %in% c("General", "Lead") ~ clean_numeric_string(loan_award_total),
             project_type == "Emerging Contaminants" ~ clean_numeric_string(convert_to_numeric(loan_total_w_1_0_loan_fee_if_applicable) + 
                                                                              convert_to_numeric(loan_request_amount))),
           principal_forgiveness = clean_numeric_string(subsidy_award), 
           population = clean_numeric_string(full_time_residential_population), 
           project_description = str_squish(project_description), 
           disadvantaged = case_when(
             principal_forgiveness == "0" ~ "No", 
             principal_forgiveness == "No Information" ~ "No Information", 
             TRUE ~ "Yes"), 
           project_rank = as.character(NA), 
           project_score = str_squish(final_score), 
           expecting_funding = "Yes", 
           state = "Washington", 
           state_fiscal_year = "2024") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(wa_clean)
  rm(list=setdiff(ls(), "wa_clean"))
  
  return(wa_clean)
}
