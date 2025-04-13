clean_tx_y1 <- function() {
  
  
  # (267,14)
  # 
  tx_ppl <- fread("year1/TX/data/tx-y1-appendix-j.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    # drop & rename columns for easier merge
    select(-owner_type, -green_type, -related_pif_number_s, -gpr, -requested_phase_s) %>%
    rename(project_cost = total_project_cost) %>%
    # fix inconsistent formatting for merge
    mutate(project_description = str_squish(project_description),
           project_description = str_replace(project_description, "12- mile", "12-mile"),
           rank_test = as.numeric(rank)) 
  
  
  tx_invite <- fread("year1/TX/data/tx-y1-appendix-k.csv",
                     colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(funding_amount = clean_numeric_string(project_cost),
           expecting_funding = "Yes") %>%
    select(pif_number, funding_amount, expecting_funding)
  
  tx_ppl <- tx_ppl %>%
    left_join(tx_invite, by="pif_number")
  
  
  tx_lead <- fread("year1/TX/data/tx-y1-appendix-i-lsl.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(project_type = "Lead",
           disadvantaged = "No Information",
           funding_amount = "No Information",
           expecting_funding = "No Information") %>%
    rename(project_cost = total_project_cost) %>%
    select(-requested_phase_s)
  
  
  tx_ec <- fread("year1/TX/data/tx-y1-appendix-j-ec.csv",
                 colClasses = "character", na.strings = "") %>% 
    clean_names()  %>%
    mutate(project_type = "Emerging Contaminants",
           disadvantaged = "No Information",
           funding_amount = "No Information",
           expecting_funding = "No Information") %>%
    rename(project_cost = total_project_cost) %>%
    select(-requested_phase_s)
  
  tx_clean <- bind_rows(tx_ppl, tx_lead, tx_ec) %>%
    
    mutate(population = clean_numeric_string(population),
           project_cost = clean_numeric_string(project_cost),
           borrower = str_squish(entity),
           pwsid = str_squish(pws_id),
           project_id = str_squish(pif_number),
           project_description = str_squish(project_description),
           project_score = str_squish(points),
           project_rank = str_squish(rank),
           project_type = replace_na(project_type, "General"),
           disadvantaged = case_when(
               project_type == "General" ~ ifelse(is.na(disadv_percent), "No", "Yes"),
               TRUE ~ "Yes"),
           expecting_funding = replace_na(expecting_funding, "No"),
           state = "Texas",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           pwsid = replace_na(pwsid, "No Information"),
           project_id = replace_na(project_id, "No Information"),
           funding_amount = replace_na(funding_amount, "0"),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}