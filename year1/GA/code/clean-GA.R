source("resources.R")

clean_ga_y1 <- function() {
  
  
  ## BASE IUP
  # (60,14)
  ga_raw <- fread("year1/GA/data/10-Georgia_Comp_clean.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # (60,14) -> (60,8)
  ga_base <- ga_raw %>%
    # remove columns
    select(-est_notice_to_proceed, -est_construction_start, -est_construction_completion, -
             est_interst_rate, -est_terms) %>%
    # process numeric columns
    mutate(
      population = clean_numeric_string(population),
      funding_amount = clean_numeric_string(total_disburs),
      project_cost = clean_numeric_string(total_project_cost),
    ) %>%
    # process text columns
    mutate(borrower = str_squish(community),
           project_description = str_squish(project_description),
           project_score = str_squish(project_score),
           project_type = case_when(
             grepl("Lead", project_description, ignore.case=TRUE) ~ "Lead",
             grepl("PFAS", project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           # Affordability Score in Attachment 1: 2022 Comprehensive List in PPL is 29 or above.
           disadvantaged = case_when(
             affordability_score >= 29 ~ "Yes",
             TRUE ~ "No"
           ),
           expecting_funding = case_when(
             funding_amount > 0 ~ "Yes",
             TRUE ~ "No"
           ),
    ) %>% 
    select(project_score, borrower, project_description, funding_amount, project_cost,
           population, disadvantaged, expecting_funding, project_type)
  
  
  
  ## SUPPLEMENTAL IUP
  
  ga_supp_1 <- fread("year1/GA/data/10-Georgia_Supplemental_IUP_A1.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names()
  
  ga_supp_2 <- fread("year1/GA/data/10-Georgia_Supplemental_IUP_A2.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(project, total_disburs) %>%
    # manually standardize names for matching
    mutate(project = str_replace(project, "Clayton County Water Authority", "Clayton County Water Authority (CCWA)"),
           total_disburs = str_squish(total_disburs)) %>%
    rename(community = project)
  
  ga_supp <- ga_supp_1 %>%
    left_join(ga_supp_2) %>%
    # remove columns
    select(-est_notice_to_proceed, -est_construction_start, -est_construction_completion, -
             est_interst_rate, -est_terms) %>%
    # process numeric columns
    mutate(
      population = clean_numeric_string(population),
      funding_amount = clean_numeric_string(total_disburs),
      project_cost = clean_numeric_string(total_project_cost),
    ) %>%
    # process text columns
    mutate(borrower = str_squish(community),
           project_description = str_squish(project_description),
           project_score = str_squish(project_score),
           project_type = case_when(
             grepl("Lead", project_description, ignore.case=TRUE) ~ "Lead",
             grepl("PFAS", project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           # Affordability Score in Attachment 1: 2022 Comprehensive List in PPL is 29 or above.
           disadvantaged = case_when(
             affordability_score >= 29 ~ "Yes",
             TRUE ~ "No"
           ),
           expecting_funding = case_when(
             funding_amount > 0 ~ "Yes",
             TRUE ~ "No"
           )
    ) %>% 
    select(project_score, borrower, project_description, funding_amount, project_cost,
           population, disadvantaged, expecting_funding, project_type)
  
  
  
  ## LEAD IUP
  ga_lead_1 <- fread("year1/GA/data/10-Georgia_Lead_IUP_A1.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names()
  
  ga_lead_2 <- fread("year1/GA/data/10-Georgia_Lead_IUP_A2.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(project, total_disburs) %>%
    # manually standardize names for matching
    mutate(
      project = str_squish(str_replace(project, "Bartow County Water", "Bartow County Water Department")),
      project = str_squish(str_replace(project, "Lee County Water Authority", "Lee County Utility Authority")),
      project = str_squish(str_replace(project, "City of Monroe", "Monroe County")),
      project = str_squish(str_replace(project, "Cobb County Water Authority", "Cobb County Water System")),
      project = str_squish(str_replace(project, "Jackson County Water Authority", "Jackson County Water and Sewerage Authority")),
      total_disburs = str_squish(total_disburs)) %>%
    rename(community = project)
  
  ga_lead <- ga_lead_1 %>%
    # remove asterisks for joining second table by community name
    mutate(community = str_squish(str_remove_all(community, "\\*"))) %>%
    full_join(ga_lead_2) %>%
    # process numeric columns
    mutate(
      population = clean_numeric_string(x2020_pop),
      funding_amount = clean_numeric_string(total_disburs),
      project_cost = clean_numeric_string(total_project_cost),
    ) %>%
    # process text columns
    mutate(borrower = str_squish(community),
           project_description = str_squish(project_description),
           project_score = clean_numeric_string(affordability_score),
           project_type = "Lead",
           # Affordability Score in Attachment 1: 2022 Comprehensive List in PPL is 29 or above.
           disadvantaged = case_when(
             as.numeric(project_score) >= 29 ~ "Yes",
             TRUE ~ "No"
           ),
           expecting_funding = case_when(
             funding_amount > 0 ~ "Yes",
             TRUE ~ "No"
           ),
    ) %>% 
    select(project_score, borrower, project_description, funding_amount, project_cost,
           population, disadvantaged, expecting_funding, project_type)
  
  
  ## EC IUP
  # (2,10)
  ga_ec_1 <- fread("year1/GA/data/10-Georgia_EC_IUP_A1.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # (2,10) -> (2,2)
  ga_ec_2 <- fread("year1/GA/data/10-Georgia_EC_IUP_A2.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(project, total_disburs) %>%
    # prep for merging with appendix 1 table
    rename(community = project) %>%
    mutate(community = str_squish(str_remove_all(community, "\\*")))
  
  # process and merge GA EC tables
  # -> (2,10)
  ga_ec <- ga_ec_1 %>%
    left_join(ga_ec_2) %>%
    # process numeric columns
    mutate(population = clean_numeric_string(x2019_pop),
           project_cost = clean_numeric_string(total_project_cost),
           funding_amount = clean_numeric_string(total_disburs),
    ) %>%
    # process text columns
    mutate(borrower = str_squish(community),
           project_description = str_squish(project_description),
           project_type = "Emerging Contaminants",
           project_score = clean_numeric_string(affordability_score),
           # Affordability Score in Attachment 1: 2022 Comprehensive List in PPL is 29 or above.
           disadvantaged = case_when(
             as.numeric(project_score) >= 29 ~ "Yes",
             TRUE ~ "No"
           ),
           expecting_funding = case_when(
             funding_amount > 0 ~ "Yes",
             TRUE ~ "No"
           )
    ) %>%
    select(project_score, borrower, project_description, funding_amount, project_cost,
           population, disadvantaged, expecting_funding, project_type)
  
  
  # join four tables together
  # NOTE: for funding_amount we would sum together values from base and supplemental, but there are no projects in common on both lists, so summing is not necessary, we can just bind the rows together.
  # -> (323,10)
  ga_clean <- bind_rows(ga_base, ga_supp, ga_lead, ga_ec) %>%
    mutate(state = "Georgia",
           state_fiscal_year = "2023",
           project_description = replace_na(project_description, "No Information"),
           community_served = as.character(NA),
           pwsid = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_rank = as.character(NA),
          ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(ga_clean)
  rm(list=setdiff(ls(), "ga_clean"))

return(ga_clean)
}