clean_tx_y2 <- function() {
  base_path <- file.path("year2", "TX", "data")
  
  # this includes all projects
  tx_ppl <- fread(file.path(base_path, "tx-y2-appendix-j.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names()
  
  
  tx_invite <- fread(file.path(base_path, "tx-y2-appendix-k.csv"),
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(funding_amount = clean_numeric_string(project_cost),
           expecting_funding = "Yes") %>%
    select(pif_number, funding_amount, expecting_funding)
  
  # projects not on appendix j that are EC projects
  tx_ec <- fread(file.path(base_path, "tx-y2-appendix-j-ec.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants")
  
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
           disadvantaged = ifelse(is.na(disadv_percent), "No", "Yes"),
           project_rank = str_squish(rank),
           project_score = str_squish(points),
           project_type = ifelse(is.na(project_type), "General", "Emerging Contaminants"),
           expecting_funding = replace_na(expecting_funding, "No"),
           funding_amount = replace_na(funding_amount, "No Information"),
           pwsid = replace_na(pwsid, "No Information"),
           project_id = replace_na(project_id, "No Information"),
           state = "Texas",
           state_fiscal_year = "2024"
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  


  # Run validation tests
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}