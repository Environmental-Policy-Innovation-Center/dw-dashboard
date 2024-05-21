library(tidyverse)
library(data.table)
library(janitor)
source("cleaning-functions.R")



clean_tn <- function() {
  
  tn_ppl <- fread("year1/TN/data/tn-fy22-ppl.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() 
  
  tn_clean <- tn_ppl %>%
    # process numeric columns
    mutate(project_cost = convert_to_numeric(total_project_amount),
           population = convert_to_numeric(pop_served)) %>%
    # process text columns 
    mutate(cities_served = str_squish(county),
           borrower = str_squish(local_government),
           pwsid = str_squish(pwsid_number),
           project_type = "General",
           project_description = str_squish(project_description),
           disadvantaged = case_when(
             grepl("\\*", local_government) ~ "Yes",
             TRUE ~ "No"),
           # with dac define, remove extra characters referencing table footnotes
           borrower = str_replace_all(borrower, "\\*", ""),
           borrower = str_replace_all(borrower, "\\+", ""),
           state_rank = str_squish(rank_order),
           state_score = str_squish(priority_points),
           funding_status = "Not Funded",
           state = "Tennessee",
           category = "2"
           ) %>%
    select(cities_served, borrower, pwsid, project_type, project_cost,
           population, project_description, disadvantaged, state_rank, state_score,
           funding_status, state, category)
  
  rm(list=setdiff(ls(), "tn_clean"))
  
  return(tn_clean)
}