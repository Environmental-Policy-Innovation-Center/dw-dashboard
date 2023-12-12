library(tidyverse)
library(data.table)
library(janitor)

clean_ct <- function() {
  
  # 199, 16
  ct_comprehensive <- fread("year1/CT/data/CT-iup-attachmentD.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # (107, 15) -> (107,4)
  ct_base_supplemental <- fread("year1/CT/data/CT-iup-attachmentF.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # manually fix error in the PDF data where a project is misidentified by project name
    mutate(project_number = case_when(
      project_number == "SFY 23-45"~ "SFY 23-86",
      TRUE ~ project_number),
          project_name = case_when(
            project_number == "SFY 22-02" ~ "Lead Service Lines - Replacement Phase 1  (Construction)2",
            TRUE ~ project_name
          )
      ) %>%
    # format columns specific to funded data and for merging
    mutate(funding_status = "Funded",
           project_name = str_squish(project_name),
           funding_amount = as.numeric(str_replace_all(amount_requested,"[^0-9.]",""))
             ) %>%
    select(project_number,  project_name, funding_status, funding_amount)
  
  
  ct_clean <- ct_comprehensive %>%
    # pre-emptively fix spaces in ct_comprehensive so it will merge correctly
    mutate(project_name = str_squish(project_name)) %>%
    left_join(ct_base_supplemental, by=c("project_number", "project_name")) %>%
    # process funded and not funded together
    mutate(
      population = as.numeric(str_replace_all(population_served_by_project,"[^0-9.]","")),
      requested_amount =  as.numeric(str_replace_all(amount_requested,"[^0-9.]","")), 
      funding_amount = replace_na(funding_amount, 0)
    ) %>%
    mutate(city_served = str_squish(town_of_pws),
           borrower = str_squish(public_water_system),
           pwsid = case_when(
             pwsid == "none" ~ as.character(NA),
             TRUE ~ str_squish(pwsid)
                             ),
           project_name = str_squish(project_number),
           project_description = str_squish(project_name),
           disadvantaged = case_when(
             project_serves_a_disadvantaged_community == "Yes" ~ "Yes",
             TRUE ~ "No"),
           state_rank = str_squish(rank),
           project_type = case_when(
             lead_service_line_estimated_amount != "$0" ~ "Lead",
             emerg_contam_estimated_amount != "$0" ~ "Emerging Contaminants",
             TRUE ~ "General"),
           funding_status = case_when(
             funding_status == "Funded" ~ "Funded",
             TRUE ~ "Not Funded"),
           state = "Connecticut",
           category = "1"
           ) %>%
    select(city_served, borrower, pwsid, project_name, project_type, requested_amount, funding_amount,
           project_description, population, disadvantaged, rank, funding_status, state, category)
  
  
  rm(list=setdiff(ls(), "ct_clean"))
  
  return(ct_clean)
}