library(tidyverse)
library(data.table)
library(janitor)

clean_oh <- function() {
  
  ## Base, p25-35
  # -> (440,9)
  oh_base <- fread("year1/OH/data/oh-ppl-base.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-v1) %>%
    # reset all column names and drop column name row
    rename(entity = v2, project = v3, pwsid = v4, population = v5, county = v6, 
           estimated_loan_amount = v7, loan_type = v8, estimated_award_date = v9, rate = v10) %>%
    filter(entity != "Entity")
  
  
  ## PF
  oh_pf <- fread("year1/OH/data/oh-ppl-pf.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(!is.na(estimated_principal_forgiveness)) %>%
    select(entity, estimated_loan_amount, estimated_principal_forgiveness)
    
  
  ## Regional PF
  #
  oh_reg_pf <- fread("year1/OH/data/oh-ppl-regional-pf.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(!is.na(estimated_principal_forgiveness)) %>%
    select(entity, estimated_loan_amount, estimated_principal_forgiveness)
  
  oh_pf <- bind_rows(oh_pf, oh_reg_pf) %>%
    filter(estimated_principal_forgiveness != "BYPASS")
  
  
  # merge PF into the base table
  oh_base <- oh_base %>%
    left_join(oh_pf) %>%
    mutate(estimated_principal_forgiveness = case_when(
      # fill in gaps where multiple projects or funding_amount discrepancies cause mismatches
      entity == "Walnut Creek Water Company" & estimated_loan_amount == "$3,950,000" ~ "$2,070,000",
      entity == "Rittman" ~ "$2,173,483",
      entity == "La Rue" ~ "$54,000",
      entity == "Piketon" & loan_type == "Construction" ~ "$3,994,717",
      entity == "Nelsonville" & loan_type == "Construction" ~ "$2,759,300",
      # replace PF where it attached to two projects
      project == "Village of Mantua Water Treatment Plant Liquid Chlorine" ~ as.character(NA),
      TRUE ~ estimated_principal_forgiveness
    ))

  
  # -> (440,12)
  oh_clean <- oh_base %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]","")),
           funding_amount = as.numeric(str_replace_all(estimated_loan_amount,"[^0-9.]","")),
           principal_forgiveness_amount = as.numeric(str_replace_all(estimated_principal_forgiveness,"[^0-9.]","")),
           principal_forgiveness_amount = replace_na(principal_forgiveness_amount, 0)
    ) %>%
    # process text columns
    mutate(borrower = str_squish(entity),
           project_description = str_squish(project),
           pwsid = str_squish(pwsid),
           cities_served = str_squish(county),
           state = "Ohio",
           category = "3",
           funding_status = "Funded",
           project_type = case_when(
             grepl("HAB", rate) | grepl("PFAS", rate)  ~ "Emerging Contaminants",
             grepl("Lead", project_description) | grepl("LSL", project_description) | grepl("LSL", rate) ~ "Lead",
             TRUE ~ "General"),
           disadvantaged = case_when(
             grepl("DIS", rate) ~ "Yes",
             TRUE ~ "No")
    ) %>%
    select(cities_served, borrower, pwsid, project_type, funding_amount, principal_forgiveness_amount, project_description, population,
           disadvantaged, funding_status, state, category)
  
  
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(NULL)
}