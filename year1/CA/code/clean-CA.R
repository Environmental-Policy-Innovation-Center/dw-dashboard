library(tidyverse)
library(data.table)
library(janitor)

clean_ca <- function() {
  
  ### FUNDED LIST - Updated
  
  # (111,13) -> (111,11)
  # intended to be funded projects that are not EC
  ca_fund <- fread("year1/CA/data/california-updated-fundable-list.csv",
                        colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # get rid of non-project columns
    filter(!grepl("Total", project_number)) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
           requested_amount = as.numeric(str_replace_all(estimated_project_costs,"[^0-9.]", "")),
           #TODO: Confirm this matches up with the comprehensive list
           funding_amount = as.numeric(str_replace_all(estimated_project_costs,"[^0-9.]", "")),
           principal_forgiveness_amount = as.numeric(str_replace_all(estimated_pf_grant_amount,"[^0-9.]", "")),    
           ) %>%
    # process text columns
    # pwsid is CA + first seven characters of project number
    mutate(pwsid = paste0("CA", as.character(map(strsplit(project_number, split = "-"), 1))),
           borrower = str_squish(applicant),
           project_name = str_squish(project_number),
           project_description = str_squish(project_title_description),
           disadvantaged = case_when(
             degree_of_disadvantaged == "DAC" | degree_of_disadvantaged == "SDAC" ~ "Yes",
             TRUE ~ "No"),
           project_type = "General",
           funding_status = "Funded",
           ) %>%
    select(borrower, pwsid, project_name, project_description, funding_amount, requested_amount, 
           disadvantaged, principal_forgiveness_amount, project_type, population, funding_status)
  
  
  # (11,9) -> (11,9)
  # EC projects that have funding data
  ca_ec <- fread("year1/CA/data/california-ec-ppl.csv",
                        colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
           funding_amount = as.numeric(str_replace_all(requested_funding,"[^0-9.]", "")),
           requested_amount = as.numeric(str_replace_all(requested_funding,"[^0-9.]", "")),
           principal_forgiveness_amount = as.numeric(str_replace_all(estimated_maximum_pf_grant_amount,"[^0-9.]", "")),
           ) %>%
    # process text columns
    mutate(pwsid = paste0("CA", as.character(map(strsplit(project_number, split = "-"), 1))),
           borrower = str_squish(applicant),
           project_name = str_squish(project_number),
           project_description = str_squish(project_title_description),
           project_type = "Emerging Contaminants",
           disadvantaged = case_when(
             grepl("Not", degree_of_disadvantaged) ~ "No",
             TRUE ~ "Yes"
           ),
           funding_status = "Funded") %>%
    select(borrower, pwsid, project_name, project_description, funding_amount, principal_forgiveness_amount, 
           project_type, population, disadvantaged, funding_status)
  
  
  # -> (122,11)
  ca_merge <- bind_rows(ca_fund, ca_ec)
  
  
  ### APPLICANT - Comprehensive List, Appendix B of SFY23 IUP
  
  # (345, 11) -> (294, 8) when dropping non-project rows and likely to be funded projects and consolidating columns
  ca_app <- fread("year1/CA/data/california-comprehensive-ppl.csv",
                        colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # drop non-project rows
    filter(!is.na(applicant) & applicant != "Not Funded" & !is.na(project_number2) & applicant != "Applicant") %>%
    # drop fundable projects already processed
    filter(!project_number2 %in% ca_merge$project_name) %>%
    # process numeric columns
    mutate(
      requested_amount = as.numeric(str_replace_all(estimated_requested_project_costs,"[^0-9.]", "")),
      population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
    ) %>%
    # process text columns
    mutate(
      borrower = str_squish(applicant),
      pwsid = paste0("CA", as.character(map(strsplit(project_number2, split = "-"), 1))),
      # replace pwsid where above extraction is wrong
      pwsid = case_when(
        pwsid == "CA7844" ~ "CA5610040",
        TRUE ~ pwsid),
      project_name = str_squish(project_number2),
      project_description = str_squish(project_title_description),
      degree_of_disadvantaged = str_squish(degree_of_disadvantaged),
      disadvantaged = case_when(
        degree_of_disadvantaged == "DAC" | degree_of_disadvantaged == "SDAC" | degree_of_disadvantaged == "Large Disadvantaged" | degree_of_disadvantaged == "Large Severely Disadvantaged" ~ "Yes",
        degree_of_disadvantaged == "non-DAC" | degree_of_disadvantaged == "Not Disadvantaged" ~ "No",
        degree_of_disadvantaged == "Pending" ~ "No Information",
        TRUE ~ "No Information"
      ),
      project_type = "No Information",
      funding_status = "Not Funded",
    ) %>%
    select(borrower, pwsid, project_name, project_type, project_description, requested_amount, population, 
           disadvantaged, funding_status)
  
  ### Lead PPL
  
  # (14,10)
  lead_replacement <- read.csv("year1/CA/data/california-lsl-replacement-inventory-list.csv") %>%
    clean_names() %>%
    mutate(project_cost = as.numeric(str_replace_all(estimated_cost_to_replace,"[^0-9.]", "")),
           project_description = "Lead Service Line Replacement")
  
  # (110,10)
  lead_investigation <- read.csv("year1/CA/data/california-lsl-investigation-inventory-list.csv") %>%
    clean_names() %>%
    mutate(project_cost = as.numeric(str_replace_all(estimated_costs_to_investigate,"[^0-9.]", "")) +
             as.numeric(str_replace_all(estimated_costs_to_replace_25,"[^0-9.]", "")),
           project_description = "Lead Service Line Investigation"
           )
  
  # -> (124, 7)
  ca_lead <- merge(lead_replacement, lead_investigation, all=TRUE) %>%
    mutate(borrower = str_squish(water_system_name),
           pwsid = str_squish(water_system_id),
           project_type = "Lead",
           population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
           disadvantaged = case_when(
             degree_of_disadvantaged == "DAC" ~ "Yes",
             TRUE ~ "No"),
           funding_status = "Not Funded") %>%
    select(borrower, pwsid, project_type, project_description, population, disadvantaged, funding_status)
  
  
  ### Merge Datasets
  
  # (539, 11) -> # (539, 13)
  ca_clean <- bind_rows(ca_merge, ca_app, ca_lead) %>%
    mutate(state = "California",
           category = "1")
  
  rm(list=setdiff(ls(), "ca_clean"))

  return(ca_clean)
}