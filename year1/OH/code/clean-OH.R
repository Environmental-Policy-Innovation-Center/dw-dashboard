library(tidyverse)
library(data.table)
library(janitor)

clean_oh <- function() {
  
  ## Base
  # -> (440,9)
  oh_base <- fread("year1/OH/data/35-Ohio_PPL_Base.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(-v1) %>%
    # reset all column names and drop column name row
    rename(entity = v2, project = v3, pwsid = v4, population = v5, county = v6, 
           estimated_loan_amount = v7, loan_type = v8, estimated_award_date = v9, rate = v10) %>%
    filter(entity != "Entity")
  
  
  
  ## HAB Regional PF
  #
  oh_pf <- fread("year1/OH/data/35-Ohio_PPL_RegionalPF.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(!is.na(estimated_principal_forgiveness)) %>%
    select(entity, estimated_loan_amount, estimated_principal_forgiveness)
  
  
  # merge PF into the base table
  oh_base <- oh_base %>%
    left_join(oh_pf) %>%
    # manually fix one instance where est. PF is assigned to a single project where two projects are listed in base,
    # assume that it goes to the larger portion of funds for construction
    mutate(estimated_principal_forgiveness = case_when(
      entity == "Walnut Creek Water Company" & estimated_loan_amount == "$3,950,000" ~ "$2,070,000",
      TRUE ~ estimated_principal_forgiveness
    ))
  
  
  ## Lead
  #
  oh_lead <- fread("year1/OH/data/35-Ohio_LSLR.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(entity, project, estimated_loan_amount) %>%
    mutate(project_type = "Lead")
  
  
  ## Emerging Contaminants
  # 
  oh_ec <- fread("year1/OH/data/35-Ohio_PPL_HAB_PFAS.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(entity, project, estimated_loan_amount) %>%
    mutate(project_type = "Emerging Contaminants")
  
  
  # combined lead and ec for single left join to bring in project_type
  oh_lead_ec <- bind_rows(oh_lead, oh_ec)
  
  oh_combined <- oh_base %>%
    left_join(oh_lead_ec, by=c("entity", "estimated_loan_amount"))
  
  # -> (440,9)
  oh_clean <- oh_combined %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]","")),
           project_cost = as.numeric(str_replace_all(estimated_loan_amount,"[^0-9.]","")),
    ) %>%
    # process text columns
    mutate(borrower = str_squish(entity),
           project_description = str_squish(project.x),
           pwsid = str_squish(pwsid),
           cities_served = str_squish(county),
           state = "Ohio",
           category = "",
           funding_status = "Not Funded",
           project_type = case_when(
             # fill in a few gaps from the joins
             is.na(project_type) & grepl("HAB", rate) ~ "Emerging Contaminants",
             grepl("Lead", project_description) | grepl("LSL", project_description) | grepl("LSL", rate) ~ "Lead",
             is.na(project_type) ~ "General",
             TRUE ~ project_type),
           disadvantaged = case_when(
             grepl("DIS", rate) ~ "Yes",
             TRUE ~ "No")
    ) %>%
    select(borrower, pwsid, project_description, project_cost,
           cities_served, population, disadvantaged, state, project_type, funding_status, category) %>%
    # remove duplicates from potential one:many relationship from the left_join for project_type
    distinct()
  
  
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}