library(tidyverse)
library(data.table)
library(janitor)

clean_nj <- function() {
  
  # (676, 19)
  nj_raw <- fread("year1/NJ/data/30-NewJersey_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (676,10)
  nj_clean <- nj_raw %>%
    select(-cat_a, -cat_b, -cat_c_a, -cat_c_b, -cat_c_c, -cat_c_d, -cat_d, -cat_e) %>%
    # process numeric columns
    mutate(
      population = as.numeric(str_replace_all(population, "[^0-9.]", "")),
      project_cost = as.numeric(str_replace_all(estimated_cost, "[^0-9.]", "")),
    ) %>%
    # process text columns
    mutate(
      borrower = str_squish(project_sponsor),
      project_name = str_squish(project_number),
      pwsid = str_squish(pwsid),
      project_description = str_squish(project_name),
      state_score = str_squish(rank_points),
      state_rank = str_replace_all(rank, "[^0-9.]", ""),
      project_type = case_when(
        grepl("EC", bil_eligibility) ~ "Emerging Contaminants",
        grepl("LSL", bil_eligibility) ~ "Lead",
        TRUE ~ "General"),
      state = "New Jersey",
      category = "2",
      funding_status = "No Information"
    ) %>%
    select(borrower, pwsid, state_rank, state_score, project_name, project_description, project_cost, 
           population, state, category, funding_status)
  
  
  rm(list=setdiff(ls(), "nj_clean"))
  
  return(nj_clean)
}