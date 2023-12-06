library(tidyverse)
library(data.table)
library(janitor)

clean_tx <- function() {
  
  
  # (267,14) -> (265,9)
  # 
  tx_ppl <- fread("year1/TX/data/43-Texas_PPL.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    # drop & rename columns for easier merge
    select(-owner_type, -green_type, -related_pif_number_s, -gpr, -requested_phase_s) %>%
    rename(project_cost = total_project_cost) %>%
    # fix inconsistent formatting for merge
    mutate(project_description = str_squish(project_description),
           project_description = str_replace(project_description, "12- mile", "12-mile")) %>%
    # get rid of total columns
    filter(!grepl("Total", rank, ignore.case=TRUE))
  
  
  # (38,13) -> (38,10)
  # Appendix K: Initial Invited Project List
  tx_k <- fread("year1/TX/data/43-Texas_Fundable_K.csv",
                colClasses= "character", na.strings="") %>% 
    clean_names() %>%
    select(-eligible_phase_s, -green_type, -related_pif_number_s, -gpr) %>%
    mutate(project_description = str_squish(project_description),
           # fix specific typos to prevent merge issues
           pws_id = str_replace(pws_id, "1330135", "TX1330135"),
    )
  
  # (268,10)
  tx_merge <- merge(tx_ppl, tx_k, by=c("pif_number", "rank", "points", "entity", "pws_id", "population",
                                       "project_description", "project_cost", "disadv_percent"), all=TRUE)
  
  # -> (268,11)
  tx_clean <- tx_merge %>%
    # remove extra columns
    select(-pif_number) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]","")),
           funding_amount = as.numeric(str_replace_all(project_cost,"[^0-9.]",""))
    ) %>%
    # process text columns
    mutate(borrower = str_squish(entity),
           pwsid = str_squish(pws_id),
           project_description = str_squish(project_description),
           state_score = str_replace_all(points,"[^0-9.]",""),
           state_rank = str_replace_all(rank,"[^0-9.]",""),
           disadvantaged = case_when(
             is.na(disadv_percent) ~ "No",
             TRUE ~ "Yes"),
           project_type = case_when(
             grepl("lead", project_description, ignore.case=TRUE) ~ "Lead",
             grepl("emerging contaminant", project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           funding_status = "Not Funded",
           state = "Texas",
           category = "2"
    ) %>%
    select(state_rank, state_score, borrower, pwsid, project_description, funding_amount, 
           population, disadvantaged, funding_status, project_type, state, category)
  
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}