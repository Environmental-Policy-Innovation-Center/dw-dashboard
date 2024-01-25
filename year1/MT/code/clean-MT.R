library(tidyverse)
library(data.table)
library(janitor)

clean_mt <- function() {
  
 
  ## FUNDABLE
  # (37,4)
  # Table 1. DWSRF Projects Anticipated to Receive Funding SFY 2023 from 
  # https://deq.mt.gov/files/Water/TFAB/DWSRF/IUP-PPL/2023_DWSRF_%20IUP_FINAL.pdf
  mt_ppl_fund <- fread("year1/MT/data/montana-ppl-table1.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()  %>%
    mutate(project_type = "General") %>%
    filter(!is.na(priority_rank))
  
  ## APPLICANT
  # Appendix 2: DWSRF COMPREHENSIVE PROJECT LIST—SFY 2023 from
  # https://deq.mt.gov/files/Water/TFAB/DWSRF/IUP-PPL/2023_DWSRF_%20IUP_FINAL.pdf
  mt_ppl_app <- fread("year1/MT/data/montana-ppl-appendix2.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()  %>%
    mutate(project_type = "General") %>%
    filter(! rank_no %in% mt_ppl_fund$priority_rank)
  
  ## LEAD FUNDALBE
  # Table 1
  mt_lsl_fund <- fread("year1/MT/data/montana-lsl-table1.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names()  %>%
    mutate(project_type = "Lead") 
  
  ## LEAD APPLICANT
  # Appendix 2
  mt_lsl_app <- fread("year1/MT/data/montana-lsl-appendix2.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead",
           # correct one rank that is off between fund and app list
           rank_no = case_when(
             rank_no == "6" ~ "5",
             rank_no == "5" & system_name == "Butte Silver Bow" ~ "6",
             TRUE ~ rank_no),
           ) %>%
    rename(project_name = system_name,
           description = project_description,
           total_points = ranking) %>%
    filter(! rank_no %in% mt_lsl_fund$priority_rank) %>%
    # change the rank of Wibaux back to 6 after filtering out the funded projects
    mutate(rank_no = case_when(
      rank_no == "5" ~ "6",
      TRUE ~ rank_no
    ))
  
  ## EC FUNDABLE
  # Table 1
  mt_ec_fund <- fread("year1/MT/data/montana-ec-table1.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names()  %>%
    mutate(project_type = "Emerging Contaminants")
  
  ## EC APPLICANT
  # Appendix 2
  mt_ec_app <- fread("year1/MT/data/montana-ec-appendix2.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants") %>%
    rename(project_name = system_name,
           description = project_description,
           total_points = ranking) %>%
    filter(! rank_no %in% mt_ec_fund$priority_rank)
  
  # Join all applicant and fundable projects
  mt_fund <- bind_rows(mt_ppl_fund, mt_ec_fund, mt_lsl_fund)
  mt_app <- bind_rows(mt_ppl_app, mt_ec_app, mt_lsl_app)
  
  # -> (36,9)
  mt_fund_clean <- mt_fund %>%
    # drop rows
    filter(!is.na(priority_rank)) %>%
    # process numeric columns
    mutate(# split project_information and transform to numbers
      population = as.character(map(strsplit(project_information, split = "\\."), 1)),
      population = as.numeric(str_replace_all(population,"[^0-9.]","")),
      project_cost = as.character(map(strsplit(srf_cost, split = " "), 1)),
      project_cost = as.numeric(str_replace_all(project_cost,"[^0-9.]","")),
      # extract whether funding includes prinicipal forgiveness or not
      funding_P = str_squish(as.character(map(strsplit(srf_cost, split = " "), 2))),
      # assume that principal forgiveness is 75% of loan, up to 750k per page 5 of IUP
      principal_forgiveness_amount = case_when(
        # if PF included, store PF as 75% of loan amount
        funding_P == "P" ~ round(project_cost * 0.75,0),
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
           #TODO: review DAC definition based on EC and lead data dictionary review, 
           # as "P" does not appear in any of their funding details
           disadvantaged = case_when(
             funding_P == "P" ~ "Yes",
             TRUE ~ "No"),
           # all projects are fundable based on title name
           funding_status = "Funded",
    ) %>%
    select(state_rank, borrower, project_description, project_cost, disadvantaged,
           principal_forgiveness_amount, population, project_type, funding_status)
  

  mt_app_clean <- mt_app %>%
    mutate(
      population = as.numeric(str_replace_all(population,"[^0-9.]","")),
      project_cost = as.numeric(str_replace_all(amount,"[^0-9.]","")),
    ) %>%
    mutate(
      state_rank = str_squish(rank_no),
      state_score = str_squish(total_points),
      borrower = str_squish(project_name),
      project_description = str_squish(description),
      funding_status = "Not Funded"
    ) %>%
    select(borrower, project_type, project_cost, population, project_description, state_rank, 
           state_score, funding_status)
  
  
  mt_clean <- bind_rows(mt_fund_clean, mt_app_clean) %>%
    mutate(state="Montana",
           category="1")

  rm(list=setdiff(ls(), "mt_clean"))
  
  return(NULL)
}