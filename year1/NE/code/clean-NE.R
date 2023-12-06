library(tidyverse)
library(data.table)
library(janitor)

clean_ne <- function() {
  
  
  # read in base, all non-lead or ec projects, (14,9)
  ne_base <- fread("year1/NE/data/27-Nebraska_ppl_base.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type="General")
  
  # read in bil, all non-lead or ec projects, (13,9)
  ne_bil <- fread("year1/NE/data/27-Nebraska_ppl_bil.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type="General")
  
  # read in ec projects, (7,9)
  ne_ec <- fread("year1/NE/data/27-Nebraska_ec.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type="Emerging Contaminants")
  
  # read in lead projects (19,9)
  ne_lsl <- fread("year1/NE/data/27-Nebraska_lslr.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type="Lead")
  
  # merge data, (53,9)
  ne_merged <- bind_rows(ne_base, ne_bil, ne_ec, ne_lsl)
  
  # -> (53,9)
  ne_clean <- ne_merged %>%
    # drop columns
    select(-forgiveness_percent) %>%
    # format numeric columns
    mutate(
      population = as.numeric(str_replace_all(population, "[^0-9.]", "")),
      funding_amount = as.numeric(str_replace_all(project_est_cost, "[^0-9.]", "")),
      principal_forgiveness_amount = as.numeric(str_replace_all(forgiveness_amount, "[^0-9.]", "")),
    ) %>%
    # format text columns
    mutate(funding_status = "Funded",
           state_score = str_squish(priority_points),
           disadvantaged = case_when(
             principal_forgiveness_amount > 0 ~ "Yes",
             TRUE ~ "No")
    ) %>%
    rename(borrower = community,
           pwsid = pws_number) %>%
    select(borrower, pwsid, state_score, project_description, funding_amount, principal_forgiveness_amount, project_type,
           disadvantaged, funding_status)
  
  ## APPLICANT
  # Appendix B2 
  
  ne_app <- fread("year1/NE/data/27-Nebraska_AppB2.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population, "[^0-9.]", "")),
           project_cost = as.numeric(str_replace_all(estimated_total_cost, "[^0-9.]", "")),
    ) %>%
    # process text columns
    mutate(state_score = str_squish(priority_points),
           borrower = str_squish(community),
           pwsid = str_squish(pws_number),
           project_description = str_squish(project_description),
           project_type = case_when(
             grepl("LSL", project_description) ~ "Lead",
             TRUE ~ "General"
           )
    ) %>%
    select(state_score, borrower, pwsid, project_description, population, project_cost, project_type)
  
  ### Merge Fundable & Applicant ###
  
  ne_app_population <- ne_app %>%
    select(pwsid, state_score, population)
  
  ne_clean <- ne_clean %>%
    left_join(ne_app_population, by=c("pwsid", "state_score"))
  
  ne_clean_sub <- ne_clean %>%
    select(pwsid, state_score, funding_status)
  
  ne_app <- ne_app %>%
    left_join(ne_clean_sub, by=c("pwsid", "state_score")) %>%
    filter(is.na(funding_status)) %>%
    mutate(funding_status = "Not Funded")
  
  ## TODO: This currently misses population for Funded projects, figure out how to keep that column, but not the split project_cost (see data dictionary for details on this issue)
  ne_clean <- bind_rows(ne_clean, ne_app) %>%
    mutate(state = "Nebraska",
           category = "1")

  
  rm(list=setdiff(ls(), "ne_clean"))
  
  return(ne_clean)
}