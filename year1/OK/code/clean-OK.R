source("resources.R")

clean_ok_y1 <- function() {
  
  
  ## base / bil
  # (69,12) -> (69,14)
  ok_b <- fread("year1/OK/data/36-Oklahoma_PPL-AppB.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "General") %>%
    rename(disadvantaged = severly_disadvantged_disadvantged_or_no)
  
  ok_b$expecting_funding <- ""
  ok_b$expecting_funding[1:46] <- "Yes"
  ok_b$expecting_funding[47:69] <- "No"
  
  # for fundable projects, loan amount is funding amount
  ok_b <- ok_b %>%
    mutate(funding_amount = case_when(
      expecting_funding == "Yes" ~ clean_numeric_string(loan_amount),
      TRUE ~ "No Information"
    ))
  
  ## lead
  # (42,11) -> (42,13)
  ok_e <- fread("year1/OK/data/36-Oklahoma_PPL-AppE.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead") %>%
    rename(disadvantaged = disadvantaged_y_or_n)
  
  # only first 24 projects are fundable 
  ok_e$expecting_funding <- ""
  ok_e$expecting_funding[1:24] <- "Yes"
  ok_e$expecting_funding[25:42] <- "No"
  
  # for fundable projects, loan amount is funding amount
  ok_e <- ok_e %>%
    mutate(funding_amount = case_when(
      expecting_funding == "Yes" ~ clean_numeric_string(loan_amount),
      TRUE ~ "No Information"
    ))
  
  
  ## emerging contaminants
  # (3,11) -> (3,13)
  ok_f <- fread("year1/OK/data/36-Oklahoma_PPL-AppF.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           expecting_funding = "Yes",
           # all projects on EC list are funded
           funding_amount = clean_numeric_string(loan_amount)) %>%
    rename(disadvantaged = disadvantaged_y_or_n)
  
  
  # (114, 12)
  ok_combined <- bind_rows(ok_b, ok_e, ok_f)
  
  # -> (114,10)
  ok_clean <- ok_combined %>%
    # drop columns
    select(-base, -cumulative_amount, -anticipated_binding_commitment_date, -anticipated_construction_date) %>%
    # process numeric columns
    mutate(population = clean_numeric_string(population),
           # for all projects on all lists, loan amount is requested amount, even if it is also funding_amount for funded projects
           requested_amount = clean_numeric_string(loan_amount)
           ) %>%
    # process text columns
    mutate(state_score = case_when(
      priority_points == "Being Ranked" ~ "No Information",
      TRUE ~ str_squish(priority_points)),
      borrower = str_squish(system),
      project_description = str_squish(project_description),
      project_id = str_squish(project_number),
      # disadvantaged can either be S/D/Y for some degree of disadvantaged or N for No
      disadvantaged = case_when(
        disadvantaged == "N" ~ "No",
        TRUE ~ "Yes"),
      state = "Oklahoma",
      state_fiscal_year = "2023",
      community_served = as.character(NA),
      pwsid = as.character(NA),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      principal_forgiveness = as.character(NA),
      project_rank = as.character(NA),
      project_score = as.character(NA),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(ok_clean)
  rm(list=setdiff(ls(), "ok_clean"))
  
  return(ok_clean)
}