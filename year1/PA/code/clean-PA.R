library(tidyverse)
library(data.table)
library(janitor)

clean_pa <- function() {
  
  # (62,23)
  pa_raw <- fread("year1/PA/data/38-Pennsylvania_manual_fundable_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (62,14)
  pa_clean <- pa_raw %>%
    mutate(population = as.numeric(gsub(",", "", population)),
           funding_amount = as.numeric(str_replace_all(assistance_amount,"[^0-9.]", "")),
           principal_forgiveness_amount = as.numeric(str_replace_all(principal_forgiveness,"[^0-9.]", "")),
    ) %>%
    ## split applicant name to borrower and project name
    ## get rid of wifta- then split at - if there is one
    mutate(applicant = str_to_title(applicant),
           applicant = gsub("wifta-|wifta -|wifta|wifta partial -", "", applicant, ignore.case = T),
           ## remove parenthesis and anything in between
           applicant = str_replace_all(applicant, "\\s*\\([^\\)]+\\)", ""),
           ## borrower is text before dash
           borrower = str_squish(str_extract(applicant, "[^-]+")),
           ## project name is text after dash
           project_name = str_squish(str_extract(applicant, "[^-]+$")),
           ## append PA to beginning of pwsids
           pwsid = paste0("PA", pwsid),
           ## recategorize project type column
           project_type = case_when(grepl("lsl", project_type, ignore.case = T) ~ "Lead",
                                    grepl("lead service", project_type, ignore.case = T) ~ "Lead",
                                    grepl("lead service", proj_description, ignore.case = T) ~ "Lead",
                                    grepl("pfas", proj_description, ignore.case = T) ~ "Emerging Contaminants",
                                    grepl("pfas", applicant, ignore.case = T) ~ "Emerging Contaminants",
                                    grepl("lsl", project_type, ignore.case = T) &
                                      grepl("pfas", proj_description, ignore.case = T) ~ "Lead,
                                                                                        Emerging Contaminants",
                                    TRUE ~ "General"),
           project_description = str_squish(proj_description),
           ## fundable column may need to be done manually because no column to join on
           funding_status = case_when(fundable == "yes" ~ "Funded",
                                      TRUE ~ "Not Funded"),
           cities_served = str_to_title(city),
           state_rank = gsub(",", "", projrank),
           state_score = gsub(",", "",pv_rating),
           # only three EC projects have a disadvantaged distinction
           disadvantaged = case_when(
             borrower == "Susquehanna Area Reg. Airport Authority" ~ "No",
             project_name == "Pa American Water Company Frackville" ~ "Yes",
             project_name == "Saegertown Borough" ~ "No",
             TRUE ~ as.character(NA)
           ),
           state = "Pennsylvania",
           category = "1"
    ) %>%
    ## standardize names
    ## keep relevant columns
    select(borrower, pwsid, cities_served, project_name, project_description, project_type,
           state_rank, state_score, funding_amount, principal_forgiveness_amount, disadvantaged, 
           population, funding_status, state, category)
  
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}