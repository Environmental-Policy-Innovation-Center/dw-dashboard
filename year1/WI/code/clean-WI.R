source("resources.R")

clean_wi_y1 <- function() {
  
  # (54,17) -> (46,19)
  wi_fund_raw <- fread("year1/WI/data/49-Wisconsin_PPL.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    ## remove non project rows
    filter(!is.na(project_points)) %>%
    # define disadvantaged for the non-EC projects as total points > 59 per data dictionary
    mutate(disadvantaged = case_when(
      as.numeric(str_replace_all(total_pf_points,"[^0-9.]", "")) > 59 ~ "Yes",
      TRUE ~ "No"),
      # determine project type by keyword search for non-EC projects
      project_type = case_when(grepl("lsl", project_description, ignore.case = T) ~ "Lead",
                               grepl("pfas", project_description, ignore.case=T) ~ "Emerging Contaminants",
                               TRUE ~ "General"),)
  
  # (4,17) -> (4,19)
  wi_ec_fund <- fread("year1/WI/data/49-Wisconsin_EC_Funding_List.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # define disadvantaged as financial need points > 29 for only EC projects
    mutate(disadvantaged = case_when(as.numeric(str_replace_all(financial_need_points,"[^0-9.]", "")) > 29 ~ "Yes",
                                     TRUE ~ "No"),
           project_type = "Emerging Contaminants")
  
  # (50,19)
  wi_fund_raw <- bind_rows(wi_fund_raw, wi_ec_fund)
  
  # -> (50,11)
  wi_clean <- wi_fund_raw %>%
    mutate(
      funding_amount = clean_numeric_string(requested_project_costs),
      principal_forgiveness = clean_numeric_string(pf_estimate),
      population = clean_numeric_string(population),
      requested_amount = clean_numeric_string(requested_project_costs),
    ) %>%
    ## non-numeric columns
    mutate(expecting_funding = case_when(funding_amount != "No Information" ~ "Yes",
                                      TRUE ~ "No"),
           ## get rid of numbers from borrower name
           borrower = str_to_title(str_replace_all(municipality, "[0-9.\\#]", "")),
           project_score = str_replace_all(priority_score,"[^0-9.]",""),
           project_id = str_squish(project_number),
           community_served = borrower,
           state = "Wisconsin",
           state_fiscal_year = "2023",
           pwsid = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           project_rank = as.character(NA)
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(wi_clean)
  rm(list=setdiff(ls(), "wi_clean"))
  
  return(wi_clean)
}