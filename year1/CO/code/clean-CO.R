source("resources.R")

clean_co_y1 <- function() {
  
  
  ## Appendix A
  
  co_a <- fread("year1/CO/data/co-appendix-a.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    
    mutate(id = str_extract(project_number, "^.{6}"))
  
  ## Appendix B
  
  # (165,11) -> 144,10
  co_b <- fread("year1/CO/data/co-appendix-b.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    
    # remove columns with differing loan information that isn't being used for `funding_amount` to make the rows for same project distinct
    select(-approved_loan_amount, -term_yrs, -loan_type, -interest_rate, -estimated_project_cost_2) %>%
    distinct() %>%
    
    # manually remove duplicates that weren't caught by distinct
    # keep projects where the project_description is NOT NA or they are not in the list of projects to delete
    filter(!is.na(project_description) | !project_number %in% c("140521D-Q", "140951D-I", "132321D-Q", "140771D-I", "140401D-M",
                                                                "142361D-Q", "140421D-I")) %>%
    
    # fill in missing project descriptions
    mutate(
      project_description = case_when(
        project_number == "190101D-I" ~ "Construction or Rehabilitation of Distribution and/or Transmission Lines",
        project_number == "170080D" ~ "Construction or Rehabilitation of Distribution and/or Transmission Lines",
        project_number == "230030D" ~ "Construction or Rehabilitation of Distribution and/or Transmission Lines",
        project_number == "230340D" ~ "Construction or Rehabilitation of Distribution and/or Transmission Lines",
        TRUE ~ project_description),
      
      project_type = "General"
    )
    
  
  ## Appendix B.1 and B.2
  
  # (104,13)
  co_b1 <- fread("year1/CO/data/co-appendix-b1.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead")
  
  # (26,13)
  co_b2 <- fread("year1/CO/data/co-appendix-b2.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants")
  
  
  ## Combined Appendix B Tables
  co_b_combined <- bind_rows(co_b, co_b1, co_b2) %>%
    select(-approved_loan_amount, -term_yrs, -loan_type, -interest_rate) %>%
    mutate(id = str_extract(project_number, "^.{6}"),
           expecting_funding = "Yes")
  
  co_clean <- co_a %>%
    left_join(co_b_combined, by="id") %>%
    
    mutate(
      population = clean_numeric_string(population),
      funding_amount = clean_numeric_string(estimated_project_cost),
      project_cost = clean_numeric_string(project_cost),
    ) %>%
    mutate(
      community_served = str_squish(project_city),
      community_served = replace_na(community_served, "No Information"),
      borrower = str_squish(entity),
      pwsid = str_squish(pwsid_number),
      project_id = str_squish(id),
      project_description = str_squish(project_description.x),
      project_description = replace_na(project_description, "No Information"),
      project_type = replace_na(project_type, "No Information"),
      disadvantaged = case_when(
        dac == "Y" ~ "Yes",
        dac == "N" ~ "No",
        TRUE ~ "No Information"),
      project_score = str_squish(pts),
      project_score = replace_na(project_score, "No Information"),
      expecting_funding = case_when(
        is.na(expecting_funding) ~ "Yes",
        TRUE ~ expecting_funding),
      state = "Colorado",
      state_fiscal_year = "2023",
      project_name = as.character(NA),
      project_rank = as.character(NA),
      requested_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  

  run_tests(co_clean)
  rm(list=setdiff(ls(), "co_clean"))
  
  return(co_clean)

}