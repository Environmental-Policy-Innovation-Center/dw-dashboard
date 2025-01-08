source("resources.R")

clean_tx_y3 <- function() {
  base_path <- file.path("year3", "TX", "data")
  
  # this includes all projects
  tx_ppl <- fread(file.path(base_path, "tx-y3-iup-appendix-j.csv"),
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # expecting funding projects
  tx_invite <- fread(file.path(base_path, "tx-y3-iup-appendix-k.csv"),
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(pif_number, expecting_funding)
  
  tx_ppl <- tx_ppl %>%
    left_join(tx_invite, by="pif_number")
  
  # projects not on appendix j that are EC projects
  tx_ec <- fread(file.path(base_path, "tx-y3-appendix-j-ec.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           expecting_funding = "No Information")
  
  # lead applicant list
  tx_lsl <- fread(file.path(base_path, "tx-y3-appendix-i-lsl.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead")
  
  tx_lsl_invite <- fread(file.path(base_path, "tx-y3-appendix-j-lsl.csv"),
                          colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(pif_no, expecting_funding)
  
  tx_lsl <- tx_lsl %>%
    left_join(tx_lsl_invite, by="pif_no") %>%
    rename(pif_number = pif_no,
           population = population_served)
  
  
  
  # combine general, lead, and ec
  tx_all <- bind_rows(tx_ppl, tx_ec, tx_lsl)
  

  # join invited by project id and then process for output
  tx_clean <- tx_all %>%
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
           project_type = ifelse(is.na(project_type), "General", project_type),
           project_id = replace_na(project_id, "No Information"),
           expecting_funding = replace_na(expecting_funding, "No"),
           funding_amount = as.character(NA),
           pwsid = replace_na(pwsid, "No Information"),
           state = "Texas",
           state_fiscal_year = "2025"
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  
  # Run validation tests
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}