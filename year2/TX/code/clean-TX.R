clean_tx_y2 <- function() {
  base_path <- file.path("year2", "TX", "data")
  
  # this includes all projects
  tx_ppl <- fread(file.path(base_path, "tx-y2-appendix-j.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names()
  
  
  tx_invite <- fread(file.path(base_path, "tx-y2-appendix-k.csv"),
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(pif_number, expecting_funding)
  
  # projects not on appendix j that are EC projects
  tx_ec <- fread(file.path(base_path, "tx-y2-appendix-j-ec.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           disadvantaged = "Yes",
           list="ec")
  
  tx_ppl <- bind_rows(tx_ppl, tx_ec)
  
  # join all by project id and then process for output
  tx_clean <- tx_ppl %>%
    left_join(tx_invite, by="pif_number") %>%
    
    mutate(community_served = as.character(NA),
           borrower = str_squish(entity),
           pwsid = str_squish(pws_id),
           project_id = str_squish(pif_number),
           project_name = as.character(NA),
           project_cost = clean_numeric_string(total_project_cost),
           requested_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           population = clean_numeric_string(population),
           project_description = str_squish(project_description),
           project_rank = str_squish(rank),
           project_score = str_squish(points),
           project_type = case_when(
             # ec and led docs already defined
             !is.na(project_type) ~ project_type,
             # search for keywords from full PPL, otherwise General project
             grepl(lead_str, project_description) ~ "Lead",
             grepl(ec_str, project_description) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           # ec docs already defined - if still NA and disavd_percent from PPL is NA, Not DAC
           disadvantaged = ifelse(is.na(disadv_percent) & is.na(disadvantaged), "No", "Yes"),
           # ec list is No Info, invitation list is already Yes, anything on General PPL that's NA is No
           expecting_funding = case_when(
             is.na(expecting_funding) & list=="ec" ~ "No Information",
             is.na(expecting_funding) ~ "No",
             TRUE ~ expecting_funding),
           funding_amount = as.character(NA),
           pwsid = replace_na(pwsid, "No Information"),
           project_id = replace_na(project_id, "No Information"),
           state = "Texas",
           state_fiscal_year = "2024"
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  

  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}