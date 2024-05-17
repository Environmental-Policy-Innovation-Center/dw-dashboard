library(tidyverse)
library(data.table)
library(janitor)

clean_ri <- function() {
  
  # (129, 16)
  ri_comp <- fread("year1/RI/data/39-RhodeIsland_PPL_Comprehensive.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # -> (129, 10)
  ri_clean <- ri_comp %>%
    mutate(
      requested_amount = as.numeric(str_replace_all(funds_requested,"[^0-9.]", "")),
      population = as.numeric(str_replace_all(pop_served,"[^0-9.]", "")),
      
    ) %>%
    mutate(borrower = str_squish(system_name),
           borrower = str_remove(borrower, "\\*"),
           state_score = str_replace_all(scores_total,"[^0-9.]", ""),
           pwsid = paste0("RI", pws_id),
           project_description = str_squish(project_description),
           project_type = case_when(
             grepl("EC", source_fund, ignore.case=TRUE) ~ "Emerging Contaminants",
             grepl("LL", source_fund, ignore.case=TRUE) ~ "Lead",
             grepl("BS", source_fund) | grepl("SS", source_fund) ~ "General",
             TRUE ~ "No Information"),
           state = "Rhode Island",
           category = "2",
           funding_status = "Not Funded"
    ) %>%
    
    select(borrower, pwsid, project_type, requested_amount, project_description, population,
           state_score, funding_status, state, category)
  
  
  rm(list=setdiff(ls(), "ri_clean"))
  
  return(NULL)
}