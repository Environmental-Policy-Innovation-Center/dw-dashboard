source("resources.R")

clean_il_y2 <- function() {
  base_path <- file.path("year2", "IL", "data")

  # Read PPL Fundable data
  il_ppl_f <- fread(file.path(base_path, "y2-Illinois_PPL_Fundable.csv"),
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "General",
           requested_amount = clean_numeric_string(requested_loan_amount))
  
  # Read PPL Applicant data
  il_ppl_a <- fread(file.path(base_path, "y2-Illinois_PPL_Applicant.csv"),
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "No",
           project_type="General",
           requested_amount = clean_numeric_string(requested_loan_amount))
  
  # Combine PPL data
  il_ppl <- bind_rows(il_ppl_a, il_ppl_f) %>%
    mutate(disadvantaged = case_when(
      is.na(disadvantaged_community_principal_forgiveness) ~ "No Information",
      disadvantaged_community_principal_forgiveness == "0" ~ "No",
      TRUE ~ "Yes"
    ))
  
  # Create list of communities and DAC status
  dacs <- il_ppl %>%
    select(loan_applicant, disadvantaged) %>%
    distinct()
  
  # Process Lead Projects
  il_lead <- fread(file.path(base_path, "y2-Illinois_Lead_Fundable.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "Lead")
  
  il_lead <- merge(il_lead, dacs, by = "loan_applicant", all.x = TRUE) %>%
    mutate(
      disadvantaged = coalesce(disadvantaged, "No Information"),
      principal_forgiveness = clean_numeric_string(reserved_principal_forgiveness_amount),
      requested_amount = clean_numeric_string(reserved_loan_amount),
      funding_amount = as.character(as.numeric(principal_forgiveness) + as.numeric(requested_amount)),
    )
  
  # Process Emerging Contaminant data
  il_ec_f <- fread(file.path(base_path, "y2-Illinois_EC_Fundable.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "Emerging Contaminant")
  
  il_ec <- merge(il_ec_f, dacs, by = "loan_applicant", all.x = TRUE) %>%
    mutate(
      disadvantaged = coalesce(disadvantaged, "No Information"),
      requested_amount = clean_numeric_string(requested_loan_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness_reserved),
      funding_amount = as.character(as.numeric(requested_amount) + as.numeric(principal_forgiveness)),
    )
  
  # Combine all data
  il_merge <- bind_rows(il_ppl, il_lead, il_ec) %>%
    mutate(
      funding_amount = if_else(project_type == "General", requested_amount, funding_amount),
      population = clean_numeric_string(service_population),
      principal_forgiveness = coalesce(clean_numeric_string(principal_forgiveness), "No Information")
    )
  
  # Clean and process data
  il_clean <- il_merge %>%
    mutate(
      borrower = str_squish(loan_applicant),
      borrower = str_to_title(borrower, locale="en"),
      project_score = str_replace_all(loan_priority_score, "[^0-9.]", ""),
      project_description = str_squish(project_description),
      project_description = str_to_sentence(project_description),
      state = "Illinois",
      state_fiscal_year = "2024",
      community_served = as.character(NA),
      project_id = coalesce(l17_number, "No Information"),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      project_rank = as.character(NA),
      pwsid = coalesce(facility_no, "No Information"),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  # Verify final shape of il_clean
  # cat("Final shape of il_clean:", dim(il_clean), "\n")
  
  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)
}








