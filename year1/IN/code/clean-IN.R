library(tidyverse)
library(data.table)
library(janitor)

clean_in <- function() {
 
  ## PPL
  # (73,17)
  in_ppl <- fread("year1/IN/data/14-Indiana_Q4Final_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  ## Lead
  # (9,17)
  in_lead_ppl <- fread("year1/IN/data/14-Indiana_Q4Final_LeadPPL.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (82,17)
  in_combined <- bind_rows(in_ppl, in_lead_ppl)
  
  
  # -> (82,12)  
  in_clean <- in_combined %>%  
    # drop non-project rows
    filter(participant != "NA") %>%
    # process numeric columns
    mutate(
      population = as.numeric(str_replace_all(population_served, "[^0-9.]", "")),
      requested_amount = as.numeric(str_replace_all(requested_funds, "[^0-9.]", "")),
    ) %>%
    # process text column
    mutate(
      borrower = str_squish(participant),
      pwsid = case_when(
        pwsid_no_s == "TBD" ~ "No Information",
        TRUE ~ paste0("IN", pwsid_no_s)),
      project_name = str_squish(srf_project_no),
      project_description = str_squish(project_description),
      state_score = str_replace_all(ppl_score, "[^0-9.]", ""),
      state_rank = str_replace_all(ppl_rank, "[^0-9.]", ""),
      disadvantaged = str_squish(disadvantaged_community),
      project_type = case_when(
        emerging_contaminants == "Yes" ~ "Emerging Contaminants",
        as.numeric(str_replace_all(lead_service_line_replacement_cost, "[^0-9.]", "")) > 0 ~ "Lead",
        TRUE ~ "General"
      ),
      state = "Indiana",
      category = "2",
      funding_status = as.character(NA),
    ) %>%
    select(borrower, pwsid, state_rank, state_score, project_name, project_description,
           project_type, requested_amount, population, disadvantaged, project_type, state, category, funding_status)
  
  rm(list=setdiff(ls(), "in_clean"))
  
  return(in_clean)
}