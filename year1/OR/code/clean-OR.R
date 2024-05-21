library(tidyverse)
library(data.table)
library(janitor)
source("cleaning-functions.R")



clean_or <- function() {

  # read in larger tables and clean up their column names for easier parsing
  or_hcc <- fread("year1/OR/data/or-hcc-ppl.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(applicant = applicant_loi_sd_number_1_county_rdo_rpm_2_population,
           primary_focus = primary_project_focus_e_g_treat_dist_storage_4,
           disadvantaged = disadvantaged_community_7,
           project_rating = project_rating_120_8
           )
  
  or_gen <- fread("year1/OR/data/or-gen-infra-res-ppl.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(applicant = applicant_loi_sd_number_1_county_rdo_rpm_2_population,
           primary_focus = primary_project_focus_e_g_treat_dist_storage_4,
           disadvantaged = disadvantaged_community_7,
           project_rating = project_rating_30_8)

  # combine similar tables, then extract multi-column information then cleaning as normal
  or_comb <- bind_rows(or_hcc, or_gen) 
  
  or_comb <- separate(or_comb, applicant, into = paste0("applicant_", 1:5), sep = "\n")
  
  or_comb <- or_comb %>%
    rename(borrower = applicant_1,
           project_name = applicant_2,
           cities_served = applicant_3,
           population = applicant_5) %>%
    mutate(population = convert_to_numeric(population),
           requested_amount = convert_to_numeric(amount_req),
           project_description = str_squish(str_replace_all(primary_focus, "\n", " ")),
           state_rank = str_squish(rank),
           state_score = str_squish(project_rating),
           ) %>%
    select(cities_served, borrower, project_name, requested_amount, project_description,
           population, disadvantaged, state_rank, state_score)
  
  # only one entry, but different enough columns to clean and then merge together
  or_em <-  fread("year1/OR/data/or-emergency-ej-ppl.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(disadvantaged = disadvantaged_community,
           borrower = applicant,
           project_name = applicant_number,
           cities_served = county) %>%
    mutate(population = convert_to_numeric(population),
           requested_amount = convert_to_numeric(amount_req)) %>%
    select(cities_served, borrower, project_name, requested_amount, population)
    
  # combine final tables and add common features
  or_clean <- bind_rows(or_comb, or_em) %>%
    mutate(state = "Oregon",
           funding_status = "Not Funded",
           category = "2")
  
  rm(list=setdiff(ls(), "or_clean"))
  
  return(or_clean)
}