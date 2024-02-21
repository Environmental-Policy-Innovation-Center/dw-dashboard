library(tidyverse)
library(data.table)
library(janitor)

clean_ms <- function() {
  
  # (48,9)
  ms_raw <- fread("year1/MS/data/24-Mississippi_PPL.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # -> (42,8)
  ms_clean <- ms_raw %>%
    # drop category rows and funding line row
    filter(!grepl("Category", project) & project_description != "NA") %>%
    # format numeric columns
    mutate(
      population = as.numeric(str_replace_all(service_area_population,"[^0-9.]","")),
      requested_amount = as.numeric(str_replace_all(loan_amount_requested,"[^0-9.]","")),
      # use this to separate funding/applicant projects, but don't keep in standardized data
      state_cumulative = as.numeric(str_replace_all(statewide_cum, "[^0-9.]","")),
      # if above the threshold, funding amount is requested amount. otherwise 0
      funding_amount = ifelse(
        state_cumulative < 42500000, as.numeric(str_replace_all(loan_amount_requested,"[^0-9.]","")), 0),
      ) %>%
    # format text columns
    mutate(borrower = str_squish(project),
           project_description = str_squish(project_description),
           state_score = str_replace_all(priority_points,"[^0-9.]",""),
           disadvantaged = ifelse(
             as.numeric(str_replace_all(eligible_pf_amount,"[^0-9.]","")) > 0, "Yes", "No"),
           state = "Mississippi",
           category = "3",
           funding_status = case_when(
             # funding line set at 42,500,000 in PPL
             state_cumulative < 42500000 ~ "Funded",
             TRUE ~ "Not Funded")) %>%
    select(borrower, requested_amount, funding_amount, project_description, population, disadvantaged,
           state_score, funding_status, state, category)
  
  
  rm(list=setdiff(ls(), "ms_clean"))
  
  return(NULL)
}