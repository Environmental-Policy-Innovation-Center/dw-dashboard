source("resources.R")

clean_tx_y1 <- function() {
  
  
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
           project_description = str_replace(project_description, "12- mile", "12-mile"))
  
  
  # #NOTE: As of now, Appendix K does not provide any new info, but would be added back in if we used it 
  # # to consider invited projects as expecting funding
  # 
  # # (38,13) -> (38,10)
  # # Appendix K: Initial Invited Project List
  # tx_k <- fread("year1/TX/data/43-Texas_Fundable_K.csv",
  #               colClasses= "character", na.strings="") %>% 
  #   clean_names() %>%
  #   select(-eligible_phase_s, -green_type, -related_pif_number_s, -gpr) %>%
  #   mutate(project_description = str_squish(project_description),
  #          # fix specific typos to prevent merge issues
  #          pws_id = str_replace(pws_id, "1330135", "TX1330135"),
  #   )
  # 
  # # (268,10)
  # tx_merge <- merge(tx_ppl, tx_k, by=c("pif_number", "rank", "points", "entity", "pws_id", "population",
  #                                      "project_description", "project_cost", "disadv_percent"), all=TRUE)
  
  # -> (265,11)
  tx_clean <- tx_ppl %>%
    # process numeric columns
    mutate(population = clean_numeric_string(population),
           project_cost = clean_numeric_string(project_cost),
           funding_amount = clean_numeric_string(project_cost),
    ) %>%
    # process text columns
    mutate(borrower = str_squish(entity),
           pwsid = str_squish(pws_id),
           project_id = str_squish(pif_number),
           project_description = str_squish(project_description),
           project_score = str_replace_all(points,"[^0-9.]",""),
           project_rank = str_replace_all(rank,"[^0-9.]",""),
           disadvantaged = case_when(
             is.na(disadv_percent) ~ "No",
             TRUE ~ "Yes"),
           project_type = case_when(
             grepl("lead", project_description, ignore.case=TRUE) ~ "Lead",
             grepl("emerging contaminant", project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           expecting_funding = "No",
           state = "Texas",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           pwsid = replace_na(pwsid, "No Information")
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}