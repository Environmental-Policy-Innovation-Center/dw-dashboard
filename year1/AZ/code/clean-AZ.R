library(tidyverse)
library(data.table)
library(janitor)
source("cleaning-functions.R")



clean_az <- function() {
  
  # (54,9)
  # ppl is most up to date, but iup has columns not otherwise included - pwsid and project type
  az_iup <- fread("year1/AZ/data/az-sfy23-iup-revision2.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_number = str_replace_all(project_number, "\\r", " "),
           project_type = case_when(
             grepl("2", applicant) ~ "Lead",
             grepl("3", applicant) ~ "Emerging Contaminants",
             TRUE ~ "General")) %>%
    select(project_number, pws_number, project_type)
    
  # -> (49,3)
  az_ppl <- fread("year1/AZ/data/az-ppl-sfy23-revised-ppl.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (54,11)
  az_clean <- az_ppl %>%
    left_join(az_iup, by=c("project_number")) %>%
    mutate(
           # get rid of linebreak character, then for projects with multiple $ values, remove the second
           amount_requested_probable_green_amount = str_replace_all(amount_requested_probable_green_amount, "\\r", " "),
           amount_requested_probable_green_amount = str_replace_all(amount_requested_probable_green_amount, "/.*", ""),
           requested_amount = convert_to_numeric(amount_requested_probable_green_amount),
           population = convert_to_numeric(population),
           funding_amount = case_when(
             grepl("4", applicant) ~ requested_amount,
             TRUE ~ 0)
           ) %>%
    mutate(
      # remove as much of the 1,3 footnotes from the borrowers, then clean up by removing numbers not caught in the regex
      borrower = str_replace_all(applicant, "\\s*\\d+\\s*,\\s*\\d+\\s*", ""),
           borrower = str_replace_all(borrower, "4", ""),
           borrower = str_replace_all(borrower, "1", ""),
           borrower = str_replace_all(borrower, ",", ""),
           borrower = str_squish(borrower),
           # fix applicant that gets chopped up because of above mutations
           borrower = case_when(
             ppl_rank == "1" ~ "Sun Valley Farms Unit VI Water Company, Inc.",
             TRUE ~ borrower),
           pwsid = str_squish(pws_number),
           project_name = str_squish(project_number),
           project_type = case_when(
            is.na(project_type) ~ "General",
            TRUE ~ project_type),
           project_description = str_squish(description),
           disadvantaged = case_when(
             grepl("1", applicant) ~ "Yes",
             TRUE ~ "No"),
           state_rank = str_squish(ppl_rank),
           funding_status = ifelse(funding_amount > 0, "Funded", "Not Funded"),
           state = "Arizona",
           category = "3"
      ) %>%
    select(borrower, pwsid, project_name, project_type, requested_amount, funding_amount, project_description,
           population, disadvantaged, state_rank, funding_status)
  
  rm(list=setdiff(ls(), "az_clean"))
  
  return(az_clean)
}