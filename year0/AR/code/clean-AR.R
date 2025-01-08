source("resources.R")

clean_ar_y0 <- function() {
  
  ar_ppl <- fread("year0/AR/data/ar-fy22-dwsrf-ppl.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      borrower = str_squish(pws),
      pwsid = str_squish(pws_id),
      project_id = str_squish(no),
      project_cost = clean_numeric_string(project_cost),
      project_description = str_squish(project_description),
      population = clean_numeric_string(population),
      disadvantaged = str_to_title(disadvantaged_y_n),
      project_rank = str_squish(no),
      project_score = str_squish(total_points)
    ) %>%
    select(borrower, pwsid, project_id, project_cost, project_description,
           population, disadvantaged, project_rank, project_score)
  
  ar_c3 <- fread("year0/AR/data/ar-fy22-dwsrf-iup-chart3.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      project_id = str_squish(iup_no),
      expecting_funding = "Yes"
    ) %>%
    select(project_id, expecting_funding) %>%
    distinct()
  
  
  
  ar_c4 <- fread("year0/AR/data/ar-fy22-dwsrf-iup-chart4.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(
      project_id = str_squish(iup_no),
      principal_forgiveness = clean_numeric_string(principal_forgiveness_amt),
      expecting_funding = "Yes"
    ) %>%
    select(project_id, principal_forgiveness) %>%
    group_by(project_id) %>%
    summarize(principal_forgiveness = sum(as.numeric(principal_forgiveness))) %>%
    mutate(principal_forgiveness = clean_numeric_string(principal_forgiveness))
  
  ar_comb <- ar_ppl %>%
    left_join(ar_c3, by="project_id") %>%
    left_join(ar_c4, by="project_id")
  
  ar_clean <- ar_comb %>%
    mutate(
      community_served = as.character(NA),
      project_name = as.character(NA),
      project_type = as.character(NA),
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      expecting_funding = replace_na(expecting_funding, "No"),
      principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
      pwsid = replace_na(pwsid, "No Information"),
      state = "Arkansas",
      state_fiscal_year = "2022"
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year)
  
  
  
  run_tests(ar_clean)
  rm(list=setdiff(ls(), "ar_clean"))
  
  return(ar_clean)
}
