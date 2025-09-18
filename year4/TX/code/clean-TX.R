clean_tx_y4 <- function() {
  base_path <- file.path("year4", "TX", "data")
  
  # this includes all projects
  tx_ppl <- data.table::fread(file.path(base_path, "tx-y4-iup-appendix-j.csv"),
                  colClasses = "character", na.strings = "") |>
    clean_names()
  
  # expecting funding projects
  tx_invite <- data.table::fread(file.path(base_path, "tx-y4-iup-appendix-k.csv"),
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(expecting_funding = "Yes") |>
    dplyr::select(pif_number, expecting_funding)
 
  # join invited by project id and then process for output
  tx_clean <- tx_ppl |>
    dplyr::left_join(tx_invite) |>
    dplyr::mutate(
           community_served = as.character(NA),
           borrower = str_squish(entity),
           pwsid = str_squish(pws_id),
           pwsid = replace_na(pwsid, "No Information"),
           project_id = str_squish(pif_number),
           project_name = as.character(NA),
           project_cost = clean_numeric_string(total_project_cost),
           requested_amount = as.character(NA),
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           population = clean_numeric_string(population),
           project_description = str_squish(project_description),
           project_rank = str_squish(rank),
           project_score = str_squish(points),
           project_type = case_when(
             grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"), 
           disadvantaged = ifelse(is.na(disadv_percent), "No", "Yes"),
           project_id = replace_na(project_id, "No Information"),
           expecting_funding = replace_na(expecting_funding, "No"),
           state = "Texas",
           state_fiscal_year = "2026"
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  
  # Run validation tests
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}