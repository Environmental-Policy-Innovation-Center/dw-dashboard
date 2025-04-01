clean_tx_y0 <- function() {
  
  
  tx_ppl <- read.csv("year0/TX/data/tx-dw-sfy22-ppl-invited.csv") %>%
    clean_names()
  
  tx_clean <- tx_ppl %>%
    mutate(
      community_served = as.character(NA),
      borrower = str_squish(entity),
      pwsid = str_squish(pws_id),
      project_id = str_squish(pif),
      project_name = as.character(NA),
      project_type = "General",
      project_cost = clean_numeric_string(project_cost),
      requested_amount = as.character(NA),
      funding_amount = ifelse(invited == "Yes", project_cost, "No Information"),
      principal_forgiveness = as.character(NA),
      project_description = str_squish(project_description),
      population = clean_numeric_string(population),
      disadvantaged = ifelse(disadv == "70%", "Yes", "No"),
      project_rank = str_squish(rank),
      project_score = str_squish(points),
      expecting_funding = ifelse(invited == "Yes", "Yes", "No"),
      state = "Texas",
      state_fiscal_year = "2022",
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year)
  
  
  
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}
