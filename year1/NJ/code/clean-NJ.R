source("resources.R")

clean_nj <- function() {
  
  # (676, 19)
  nj_raw <- fread("year1/NJ/data/30-NewJersey_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (676,10)
  nj_clean <- nj_raw %>%
    select(-cat_a, -cat_b, -cat_c_a, -cat_c_b, -cat_c_c, -cat_c_d, -cat_d, -cat_e) %>%
    # process numeric columns
    mutate(
      population = clean_numeric_string(population),
      project_cost = clean_numeric_string(estimated_cost),
    ) %>%
    # process text columns
    mutate(
      borrower = str_squish(project_sponsor),
      project_id = str_squish(project_number),
      pwsid = str_squish(pwsid),
      project_description = str_squish(project_name),
      project_score = str_squish(rank_points),
      project_rank = str_replace_all(rank, "[^0-9.]", ""),
      project_type = case_when(
        grepl("EC", bil_eligibility) ~ "Emerging Contaminants",
        grepl("LSL", bil_eligibility) ~ "Lead",
        TRUE ~ "General"),
      state = "New Jersey",
      state_fiscal_year = "2023",
      community_served = as.character(NA),
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      disadvantaged = as.character(NA),
      expecting_funding = as.character(NA)
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(nj_clean)
  rm(list=setdiff(ls(), "nj_clean"))
  
  return(nj_clean)
}