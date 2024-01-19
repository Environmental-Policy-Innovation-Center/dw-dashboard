library(tidyverse)
library(data.table)
library(janitor)

clean_id <- function() {
  
  # (81, 9)
  id_ppl <- fread("year1/ID/data/12-Idaho_PPL.csv", 
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # (13,11) -> (13,4)
  # reduce fundable projects to only their rank (a UID in this case), 
  # principal forgiveness amount, and loan amount
  id_fnd <- fread("year1/ID/data/12-Idaho_Fundable.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(rank = str_replace_all(rank,"[^0-9.]", ""),
           loan_amt = as.character(map(strsplit(loan_amt_est_loan_date, split = "  "), 1)),
           proposed_funding_terms = str_squish(proposed_funding_terms),
           # see NOTE for disadv definition
           disadvantaged = case_when(
             grepl("30 years", proposed_funding_terms) ~ "Yes",
             grepl("principal forgiveness", proposed_funding_terms, ignore.case = TRUE) ~ "Yes",
             grepl("PF", proposed_funding_terms) ~ "Yes",
             TRUE ~ "No"),
           # extract PF value and format as numeric
           principal_forgiveness_amount = as.character(map(strsplit(proposed_funding_terms, split = "with"), 2)),
           principal_forgiveness_amount = as.numeric(str_replace_all(principal_forgiveness_amount, "[^0-9.]", "")),
           principal_forgiveness_amount = replace_na(principal_forgiveness_amount, 0)
    ) %>%
    select(rank, loan_amt, principal_forgiveness_amount, disadvantaged)
  
  # (81,12)
  id_merged <- merge(id_ppl, id_fnd, by=c("rank"), all.x=TRUE)
  
  # -> (81,13)
  id_clean <- id_merged %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(pop_served, "[^0-9.]", "")),
           project_cost = as.numeric(str_replace_all(project_cost, "[^0-9.]", "")),
           funding_amount = as.numeric(str_replace_all(loan_amt, "[^0-9.]", "")),
    ) %>%
    # process text columns
    mutate(project = str_squish(project),
           # remove excess spaces from scrape
           regional_office = str_squish(regional_office),
           state_score = str_replace_all(rating_points, "[^0-9.]", ""),
           state_rank = str_replace_all(rank, "[^0-9.]", ""),
           project_description = str_squish(project_description),
           # custom touch-up squishing doesn't fix
           project_description = str_replace(project_description, "C onstruction", "Construction"),
           # if from fundable table, else Applicant
           funding_status = case_when(
             !is.na(funding_amount) ~ "Funded",
             TRUE ~ "Not Funded"),
           state = "Idaho",
           category = "3",
           # only project with Lead is the "All System fund"
           project_type = case_when(
             grepl("Lead", project) ~ "Lead",
             TRUE ~ "General"),
           pwsid = str_replace(system_number, "Unknown", as.character(NA)),
           pwsid = str_replace(pwsid, "All", as.character(NA))
    ) %>%
    rename(borrower = project) %>%
    select(state_rank, state_score, borrower, pwsid, project_description, population, project_cost,
           disadvantaged, funding_amount, funding_status, principal_forgiveness_amount, project_type, state, category)
  
  rm(list=setdiff(ls(), "id_clean"))
  
  return(id_clean)
}