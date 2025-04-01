clean_ky_y1 <- function() {
  
  # (139,13)
  ky_iup <- fread("year1/KY/data/KY-iup-appendix2.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (139,14)
  ky_clean <- ky_iup %>%
    mutate(population = clean_numeric_string(system_population),
           project_cost = clean_numeric_string(total_project_cost),
           requested_amount = clean_numeric_string(requested_loan_amount),
           # convert funding amount columns to numeric and replace 0s for summing up
           loan_base = convert_to_numeric(invited_loan_amount_base, TRUE),
           loan_supplemental = convert_to_numeric(invited_loan_amount_supplemental, TRUE),
           loan_lead = convert_to_numeric(invited_loan_amount_lead_service_line, TRUE),
           funding_amount = loan_base + loan_supplemental + loan_lead,
           # because there are "true 0s" in the dataset separate from NAs, restore these to 0, fill all that would otherwise be NA as No Info
           funding_amount = case_when(
             invite_round_no_bypassed == "Bypassed" ~ "0",
             funding_amount == 0 & invite_round_no_bypassed != "Bypassed" ~ "No Information",
             TRUE ~ as.character(funding_amount)
           ),
           principal_forgiveness = clean_numeric_string(principal_forgiveness_amount),
           ) %>%
    mutate(borrower = str_squish(applicant),
           project_id = str_squish(wris_number),
           project_rank = str_squish(rank),
           project_score = str_squish(score),
           project_description = str_squish(project_title),
           project_type = case_when(
             grepl("lead", project_title, ignore.case=TRUE) ~ "Lead",
             grepl("lsl", project_title, ignore.case=TRUE) ~ "Lead",
             TRUE ~ "General"),
           expecting_funding = case_when(
             funding_amount > 0 ~ "Yes",
             TRUE ~ "No"
           ),
           state = "Kentucky",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           pwsid = as.character(NA),
           project_name = as.character(NA),
           disadvantaged = as.character(NA),
           project_score = replace_na(project_score, "No Information")
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  run_tests(ky_clean)
  rm(list=setdiff(ls(), "ky_clean"))
  
  return(ky_clean)
}