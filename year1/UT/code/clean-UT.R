library(tidyverse)
library(data.table)
library(janitor)

clean_ut <- function() {
  
  
  ut_ppl <- fread("year1/UT/data/44-Utah_Comprehensive.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  ut_clean <- ut_ppl %>%
    # drop columns
    select(-green_project, -green_amount, -equivalency_project,
           -project_segments_sour, -project_segments_treat, -project_segments_stor, -project_segments_dist) %>%
    # format numeric columns
    mutate(
      population = as.numeric(str_replace_all(pop,"[^0-9.]","")),
      # TODO: determine if ARPA needs to be subtracted from it
      funding_amount = as.numeric(str_replace_all(srf_assistance,"[^0-9.]","")),
      principal_forgiveness = as.numeric(str_replace_all(principal_forgiveness,"[^0-9.]","")),
      project_cost = as.numeric(str_replace_all(project_total,"[^0-9.]","")),
    ) %>%
    # format non-numeric columns
    mutate(
      pwsid = str_squish(pwsid),
      state_score = str_squish(priority_points),
      borrower = str_squish(system_name),
      cities_served = str_squish(county),
      project_description = str_squish(project_title),
      disadvantaged = case_when(disadvantaged == "Y" ~ "Yes",
                                TRUE ~ "No"),
      project_type = case_when(
        grepl("lead", project_description, ignore.case=TRUE) ~ "Lead",
        TRUE ~ "General"),
      state = "Utah",
      category = ""
      # TODO: Add funding_status depending on interpretation
    ) %>%
    select(state_score, borrower, pwsid, project_description, funding_amount, principal_forgiveness,
           project_type, cities_served, disadvantaged, population, state, category)
  
  
  rm(list=setdiff(ls(), "ut_clean"))
  
  return(ut_clean)
}