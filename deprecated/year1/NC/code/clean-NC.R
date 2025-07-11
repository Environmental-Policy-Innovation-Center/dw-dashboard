clean_nc_y1 <- function() {
  
  # (222, 15)
  nc_raw <- fread("year1/NC/data/nc-comprehensive-ppl.csv",
                  colClass="character", na.strings="") %>%
    clean_names()
  
  # -> (222,13)
  nc_clean <- nc_raw %>%
    # format numeric columns
    mutate(population = clean_numeric_string(service_populati_on),
           requested_amount = clean_numeric_string(total_funding_request),
           # pre-format columns that will add up to funding amount and PF
           base_dwsrf_loans = convert_to_numeric(base_dwsrf_loans, TRUE),
           bil_suppl_dwsrf_loans = convert_to_numeric(bil_suppl_dwsrf_loans,TRUE),
           bil_suppl_dwsrf_principal_forgivene_ss = convert_to_numeric(bil_suppl_dwsrf_principal_forgivene_ss, TRUE),
           base_dwsrf_principal_forgivene_ss = convert_to_numeric(base_dwsrf_principal_forgivene_ss, TRUE),
           # combine for funding amount and PF
           # does dwsrf loans already include PF? - does not appear so, as there are rows with only PF and DWSRF loans are 0, 
           # so they will need to be added together as well
           principal_forgiveness = bil_suppl_dwsrf_principal_forgivene_ss + base_dwsrf_principal_forgivene_ss,
           funding_amount = base_dwsrf_loans + bil_suppl_dwsrf_loans + principal_forgiveness,
           funding_amount = clean_numeric_string(funding_amount),
           funding_amount = case_when(
             funding_amount == "0" ~ "No Information",
             TRUE ~ funding_amount),
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           principal_forgiveness = case_when(
             principal_forgiveness == "0" ~ "No Information", 
             TRUE ~ principal_forgiveness)
           
    ) %>%
    # format text columns
    mutate(borrower = str_squish(applicant_name),
           project_name = str_squish(project_name),
           project_description = str_squish(project_name),
           project_score = str_replace_all(priorit_y_points, "[^0-9.]", ""),
           pwsid = case_when(
             pwsid == 'x' ~ "No Information",
             TRUE ~ pwsid),
           community_served = str_squish(county),
           project_type = "General",
           expecting_funding = case_when(
             funding_amount != "No Information" ~ "Yes",
             TRUE ~ "No"),
           state = "North Carolina",
           state_fiscal_year = "2023",
           project_id = as.character(NA),
           project_cost = as.character(NA),
           disadvantaged = as.character(NA),
           project_rank = as.character(NA),
           pwsid = replace_na(pwsid, "No Information")
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  run_tests(nc_clean)
  rm(list=setdiff(ls(), "nc_clean"))
  
  return(nc_clean)
}