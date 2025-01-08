source("resources.R")

clean_in_y0 <- function() {
  
  in_iup <- read.csv("year0/IN/data/INDIANA-Final-DW-2021-IUP.csv") %>%
    clean_names()
  
  in_clean <- in_iup %>%
    mutate(
    community_served = as.character(NA),
  borrower = str_squish(participant),
  pwsid = str_squish(pwsid_no),
  project_id = str_squish(srf_project_no),
  project_name = as.character(NA),
  project_type = as.character(NA),
  project_cost = clean_numeric_string(estimated_total_project_cost),
  requested_amount = clean_numeric_string(requested_funds),
  funding_amount = as.character(NA),
  principal_forgiveness = as.character(NA),
  population = clean_numeric_string(population_served),
  estimated_post_user_rate = convert_to_numeric(estimated_post_project_user_rate_per_4_000_gallons, TRUE),
  mhi = convert_to_numeric(mhi, TRUE),
  disadvantaged = case_when(
    mhi < 43460 ~ "Yes",
    estimated_post_user_rate > 45 ~ "Yes",
    estimated_post_user_rate > mhi * .01 ~ "Yes",
  TRUE ~ "No"),
  project_rank = str_squish(ppl_rank),
  project_score = str_squish(ppl_score),
  expecting_funding = ifelse(as.numeric(ppl_rank) < 12, "Yes", "No"),
  state = "Indiana",
  state_fiscal_year = "2022",
  project_rank = replace_na(project_rank, "No Information"),
  project_score = replace_na(project_rank, "No Information"),
  expecting_funding = replace_na(expecting_funding, "No")
  ) %>%
  select(community_served, borrower, pwsid, project_id, project_name, project_type,
         project_cost, requested_amount, funding_amount, principal_forgiveness,
         project_description, population, disadvantaged, project_rank, project_score,
         expecting_funding, state, state_fiscal_year)

  run_tests(in_clean)
  rm(list=setdiff(ls(), "in_clean"))
  
  return(in_clean)
}
