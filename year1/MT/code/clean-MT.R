library(tidyverse)
library(data.table)
library(janitor)

clean_mt <- function() {
  
  ## FUNDABLE
  # (37,4)
  # Table 1. DWSRF Projects Anticipated to Receive Funding SFY 2023 from 
  # https://deq.mt.gov/files/Water/TFAB/DWSRF/IUP-PPL/2023_DWSRF_%20IUP_FINAL.pdf
  mt_raw <- fread("year1/MT/data/26-Montana_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() 
  
  # -> (36,9)
  mt_clean <- mt_raw %>%
    # drop rows
    filter(!is.na(priority_rank)) %>%
    # process numeric columns
    mutate(# split project_information and transform to numbers
      population = as.character(map(strsplit(project_information, split = "\\."), 1)),
      population = as.numeric(str_replace_all(population,"[^0-9.]","")),
      funding_amount = as.character(map(strsplit(srf_cost, split = " "), 1)),
      funding_amount = as.numeric(str_replace_all(funding_amount,"[^0-9.]","")),
      # extract whether funding includes prinicipal forgiveness or not
      funding_P = str_squish(as.character(map(strsplit(srf_cost, split = " "), 2))),
      # assume that principal forgiveness is 75% of loan, up to 750k per page 5 of IUP
      principal_forgiveness_amount = case_when(
        # if PF included, store PF as 75% of loan amount
        funding_P == "P" ~ round(funding_amount * 0.75,0),
        funding_P != "P" ~ 0,
        TRUE ~ 0),
      # if PF is greater than 750k, set to max of 750k
      principal_forgiveness_amount = case_when(
        principal_forgiveness_amount > 750000 ~ 750000,
        TRUE ~ principal_forgiveness_amount)
    ) %>%
    # process text columns
    mutate(project_description = as.character(map(strsplit(project_information, split = "\\."), 2)),
           project_description = str_squish(project_description),
           state_rank = str_replace_all(priority_rank,"[^0-9.]",""),
           borrower = str_squish(project),
           disadvantaged = case_when(
             funding_P == "P" ~ "Yes",
             TRUE ~ "No"),
           # all projects are fundable based on title name
           funding_status = "Funded",
           # No Lead or EC projects found
           project_type = "General",
           state = "Montana",
           category = ""
    ) %>%
    select(state_rank, borrower, project_description, funding_amount, 
           principal_forgiveness_amount, population, project_type, funding_status, state, category)
  
  ## APPLICANT
  # Appendix 2: DWSRF COMPREHENSIVE PROJECT LIST—SFY 2023 from
  # https://deq.mt.gov/files/Water/TFAB/DWSRF/IUP-PPL/2023_DWSRF_%20IUP_FINAL.pdf
  

  rm(list=setdiff(ls(), "mt_clean"))
  
  return(mt_clean)
}