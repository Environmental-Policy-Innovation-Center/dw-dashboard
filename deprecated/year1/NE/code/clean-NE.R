clean_ne_y1 <- function() {
  
  
  # read in base, all non-lead or ec projects, (14,9)
  ne_base <- fread("year1/NE/data/27-Nebraska_ppl_base.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type="General")
  
  # read in bil, all non-lead or ec projects, (13,9)
  ne_bil <- fread("year1/NE/data/27-Nebraska_ppl_bil.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type="General")
  
  # read in ec projects, (7,9)
  ne_ec <- fread("year1/NE/data/27-Nebraska_ec.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type="Emerging Contaminants")
  
  # read in lead projects (19,9)
  ne_lsl <- fread("year1/NE/data/27-Nebraska_lslr.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type="Lead")
  
  # merge data, (53,9)
  ne_merged <- bind_rows(ne_base, ne_bil, ne_ec, ne_lsl)
  
  # -> (53,9)
  ne_clean <- ne_merged %>%
    # drop columns
    select(-forgiveness_percent) %>%
    # format numeric columns
    mutate(
      population = clean_numeric_string(population),
      funding_amount = clean_numeric_string(project_est_cost),
      principal_forgiveness = clean_numeric_string(forgiveness_amount),
    ) %>%
    # format text columns
    mutate(expecting_funding = "Yes",
           project_score = str_squish(priority_points),
           disadvantaged = case_when(
             principal_forgiveness != "0" ~ "Yes",
             TRUE ~ "No")
    ) %>%
    rename(borrower = community,
           pwsid = pws_number) %>%
    select(borrower, pwsid, project_score, project_description, funding_amount, principal_forgiveness, project_type,
           disadvantaged, expecting_funding)
  
  ## APPLICANT
  # Appendix B2 
  
  ne_app <- fread("year1/NE/data/27-Nebraska_AppB2.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # process numeric columns
    mutate(population = clean_numeric_string(population),
           project_cost = clean_numeric_string(estimated_total_cost),
    ) %>%
    # process text columns
    mutate(project_score = str_squish(priority_points),
           borrower = str_squish(community),
           pwsid = str_squish(pws_number),
           project_description = str_squish(project_description),
           project_type = case_when(
             grepl("LSL", project_description) ~ "Lead",
             TRUE ~ "General"
           )
    ) %>%
    select(project_score, borrower, pwsid, project_description, population, project_cost, project_type)
  
  ### Merge Fundable & Applicant ###
  
  ne_app_population <- ne_app %>%
    select(pwsid, project_score, population)
  
  ne_clean <- ne_clean %>%
    left_join(ne_app_population, by=c("pwsid", "project_score"))
  
  ne_clean_sub <- ne_clean %>%
    select(pwsid, project_score, expecting_funding)
  
  ne_app <- ne_app %>%
    left_join(ne_clean_sub, by=c("pwsid", "project_score")) %>%
    filter(is.na(expecting_funding)) %>%
    mutate(expecting_funding = "No")
  
  ## TODO: This currently misses population for Funded projects, figure out how to keep that column, but not the split project_cost (see data dictionary for details on this issue)
  ne_clean <- bind_rows(ne_clean, ne_app) %>%
    mutate(state = "Nebraska",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           project_rank = as.character(NA),
           pwsid = replace_na(pwsid, 'No Information'),
           funding_amount = replace_na(funding_amount, "No Information"),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
           disadvantaged = replace_na(disadvantaged, "No Information"),
           population = replace_na(population, "No Information"),
           project_cost = replace_na(project_cost, "No Information")) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  run_tests(ne_clean)
  rm(list=setdiff(ls(), "ne_clean"))
  
  return(ne_clean)
}