library(tidyverse)
library(data.table)
library(janitor)

clean_mi <- function() {
  
  ## Work in progress as a result of working matching Michigan projects to Districts for Funding Navigator team
  #TODO: update funding_status and confirm all other columns upon data dictionary being made and reviewed
  
 
  mi_raw <- fread("year2/MI/data/mi-sfy24-iup.csv",
                  colClasses = "character", na.strings = "")

  mi_clean <- mi_raw %>%
    ## clean names
    clean_names() %>%
    ## get rid of rows without projects
    filter(!is.na(project_number)) %>%
    ## rowwise operations for sums
    rowwise() %>%
    ## make relevant columns numbers
    mutate(
           population = as.numeric(gsub(",", "", population)),
           ## sum of columns, getting rid of $ and comma and replacing blanks with zero
           funding_amount = sum(as.numeric(gsub("\\$|,", "", dwsrf_traditional_loan)),
                                as.numeric(gsub("\\$|,", "", dwsrf_traditional_pf)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_supplemental_loan)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_supplemental_pf)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_emerging_contaminants_pf)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_loan)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_pf)),
                                na.rm = T),
           principal_forgiveness_amount = sum(as.numeric(str_replace_all(dwsrf_traditional_pf,"[^0-9.]", "")),
                                              as.numeric(str_replace_all(bil_dwsrf_supplemental_pf,"[^0-9.]", "")),
                                              as.numeric(str_replace_all(bil_dwsrf_emerging_contaminants_pf,"[^0-9.]", "")),
                                              as.numeric(str_replace_all(bil_dwsrf_lslr_pf,"[^0-9.]", "")),
                                              na.rm = T)) %>%
    ungroup() %>%
    ## standardize names
    rename(borrower = applicant,
           cities_served = county) %>%
    ## create project type column
    mutate(project_type = case_when(lead_service_line_costs != "" ~ "Lead",
                                    ec_related_costs != "" ~ "Emerging Contaminants"),
           project_type = replace_na(project_type, "General"),
           # with state_rank used for specifying funding, return to string
           state_score = str_squish(total_points),
           disadvantaged = replace_na(disadvantaged_status, "No"),
           state = "Michigan",
           category = "1",
    ) %>%
    ## keep relevant columns
    select(borrower, cities_served, project_description, project_type,
           state_score, funding_amount, principal_forgiveness_amount,
           disadvantaged, population, state, category)
  
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}