library(tidyverse)
library(data.table)
library(janitor)

clean_ny <- function() {
  
  ## Base
  # project annual list, p33 of FFY 2023 Final IUP
  # (558,10) -> (558,12)
  ny_base <- fread("year1/NY/data/32-NewYork_base_ppl.csv",
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "General",
           disadvantaged = case_when(
             score == "H" ~ "Yes",
             TRUE ~ "No"))
  
  ## BIL
  # (120,10) -> (120,11)
  ny_bil <- fread("year1/NY/data/32-NewYork_bil_ppl.csv",
                  colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-v1) %>%
    mutate(project_type = "General",
           disadvantaged = "Yes")
  
  ## Lead - Amendment 4
  # (109,11) -> (109,12)
  ny_lead <- fread("year1/NY/data/32-NewYork_lead.csv",
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-v1) %>%
    mutate(project_type = "Lead",
           disadvantaged = case_when(
             dac == "DAC" ~ "Yes",
             TRUE ~ "No"
           ))
  
  ## Emerging Contaminants - Amendment 2
  # (37,10) -> (37,11)
  ny_ec <- fread("year1/NY/data/32-NewYork_ec.csv",
                 colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-pfas) %>%
    mutate(project_type = "Emerging Contaminants",
           disadvantaged = case_when(
             dac == "DAC" ~ "Yes",
             TRUE ~ "No")
    )
  
  ## Combine & Clean
  # -> (824, 13)
  ny_combined <- bind_rows(ny_base, ny_bil, ny_lead, ny_ec)
  
  # -> (824,12)
  ny_clean <- ny_combined %>%
    select(-dac, -cumulative_total, -code) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(pop, "[^0-9.]", "")),
           project_cost = as.numeric(str_replace_all(project_cost, "[^0-9.]", "")),
    ) %>%
    # process text columns
    mutate(project_name = str_squish(project_number),
           cities_served = str_squish(county),
           borrower = paste(str_squish(system_name), "/", str_squish(borrower)),
           state_score = str_squish(score),
           project_description = str_squish(description),
           state = "New York",
           category = "2",
           funding_status = "Not Funded") %>%
    select(borrower, state_score, project_name, project_description, project_cost,
           population, cities_served, disadvantaged, project_type, state, category, funding_status)
  
  rm(list=setdiff(ls(), "ny_clean"))
  
  return(ny_clean)
}