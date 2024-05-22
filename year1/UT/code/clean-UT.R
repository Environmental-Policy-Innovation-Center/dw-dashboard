library(tidyverse)
library(data.table)
library(janitor)
source("cleaning-functions.R")

clean_ut <- function() {
  
  
  ut_ppl <- fread("year1/UT/data/44-Utah_Comprehensive.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # (58,22) -> (58,13)
  ut_clean <- ut_ppl %>%
    # drop columns
    select(-green_project, -green_amount, -equivalency_project,
           -project_segments_sour, -project_segments_treat, -project_segments_stor, -project_segments_dist) %>%
    # format numeric columns
    mutate(
      population = convert_to_numeric(pop),
      funding_amount = convert_to_numeric(funds_authorized),
      principal_forgiveness_amount = convert_to_numeric(principal_forgiveness),
      project_cost = convert_to_numeric(project_total),
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
      funding_status = case_when(
        funding_amount > 0 | principal_forgiveness_amount > 0 ~ "Funded",
        TRUE ~ "Not Funded"),
      state = "Utah",
      category = "3"
    ) %>%
    select(cities_served, borrower, pwsid, project_cost, funding_amount, principal_forgiveness_amount,
           population, project_description, disadvantaged, state_score, funding_status, state, category)
  
  
  rm(list=setdiff(ls(), "ut_clean"))
  
  return(NULL)
}