library(tidyverse)
library(data.table)
library(janitor)

clean_ia <- function() {
  
  ia_ppl <- read.csv("year1/IA/data/iowa-ppl-q4.csv") %>%
    clean_names()
  
  ia_clean <- ia_ppl %>%
    filter(project_status != "") %>%
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
           requested_amount = as.numeric(str_replace_all(current_funding_request,"[^0-9.]", "")),
           funding_amount = as.numeric(str_replace_all(loan_amount,"[^0-9.]", ""))) %>%
    mutate(borrower = str_squish(project_name),
           project_name = str_squish(dwsrf_no),
           project_type = case_when(
             grepl("LSL", lf_eligible) ~ "Lead",
             grepl("EC", lf_eligible) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           project_description = str_squish(project_description),
           state_score = str_squish(priority_points),
           funding_status = case_when(
             project_status == "L" | project_status == "R" ~ "Funded",
             TRUE ~ "Not Funded"),
           state = "Iowa",
           category = "3") %>%
    select(borrower, project_name, project_type, requested_amount, funding_amount, project_description,
           population, state_score, funding_status, state, category)
  
  rm(list=setdiff(ls(), "ia_clean"))
  
  return(ia_clean)
}