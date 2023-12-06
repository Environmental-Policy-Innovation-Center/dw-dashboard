library(tidyverse)
library(data.table)
library(janitor)

clean_mi <- function() {
  
  # (112,24)
  mi_raw <- fread("year1/MI/data/22-Michigan_PPL.csv",
                  colClasses = "character", na.strings = "")
  # -> (110,12)
  mi_clean <- mi_raw %>%
    ## clean names
    clean_names() %>%
    ## get rid of rows without projects
    filter(!is.na(project_number)) %>%
    ## rowwise operations for sums
    rowwise() %>%
    ## make relevant columns numbers
    mutate(state_rank = as.numeric(gsub(",", "", rank)),
           population = as.numeric(gsub(",", "", population)),
           ## sum of columns, getting rid of $ and comma and replacing blanks with zero
           funding_amount = sum(as.numeric(gsub("\\$|,", "", dwsrf_loan)),
                                as.numeric(gsub("\\$|,", "", dwsrf_pf)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_supplemental_loan)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_supplemental_pf)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_emerging_contaminants_pf)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_loan)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_pf)),
                                na.rm = T),
           principal_forgiveness_amount = sum(as.numeric(str_replace_all(total_pf_grant,"[^0-9.]", "")),
                                              -as.numeric(str_replace_all(arp_grant, "[^0-9.]", "")),
                                              na.rm = T)) %>%
    ungroup() %>%
    ## standardize names
    rename(borrower = applicant_name,
           cities_served = project_location) %>%
    ## create project type column
    mutate(project_type = case_when(lslr_costs != "" ~ "Lead",
                                    pfas_costs != "" ~ "Emerging Contaminants"),
           project_type = replace_na(project_type, "General"),
           funding_status = case_when(state_rank <= 70 ~ "Funded",
                                      state_rank > 70 ~ "Not Funded"),
           # with state_rank used for specifying funding, return to string
           state_rank = as.character(state_rank),
           state_score = str_squish(total_points),
           disadvantaged = replace_na(disadvantaged, "No"),
           state = "Michigan",
           category = "1",
    ) %>%
    ## keep relevant columns
    select(borrower, cities_served, project_description, project_type,
           state_rank, state_score, funding_amount, principal_forgiveness_amount,
           disadvantaged, population, funding_status, state, category)
  
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}