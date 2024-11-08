source("resources.R")

clean_ma <- function() {
  
  ## ALL PROJECTS
  # (120,7) -> (113,8)
  ma_comp_raw <- fread("year1/MA/data/21-Massachusetts_PPL.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(!is.na(project))
  
  # process data for all non-funding columns first
  ma_comp <- ma_comp_raw %>%
    # process numeric columns
    mutate(project_cost = clean_numeric_string(project_cost),
           population = clean_numeric_string(pop),
    ) %>%
    # process text columns
    mutate(project_id = str_squish(srf_id),
           # get rid of * in some ranks
           project_score = str_squish(str_replace_all(rating,"[^0-9.]", "")),
           pwsid = str_squish(pwsid),
           # remove all caps and drop special characters
           borrower = str_to_title(str_replace_all(applicant, "\\s*\\([^\\)]+\\)", "")),
           borrower = str_squish(borrower),
           project_type = case_when(grepl("(lr)", applicant, ignore.case = T) ~ "Lead",
                                    grepl("(ec)", applicant, ignore.case = T) ~ "Emerging Contaminants",
                                    TRUE ~ "General"),
           project_description = str_squish(project),
           # DAC defined by receiving PF as denoted in the applicant column
           disadvantaged = case_when(grepl("(pf)", applicant, ignore.case = T) ~ "Yes",
                                     TRUE ~ "No"),
    ) %>%
    # select columns
    select(borrower, pwsid, project_id, project_type, project_cost, project_description,
           population, project_score, disadvantaged, srf_id)
  
  
  ## FUNDED PROJECTS
  # Table 1, listing funded projects in 4 sections, p5-8
  # (74,6) -> (68,6)
  ma_fund_raw <- fread("year1/MA/data/21-Massachusetts_Fundable.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    ## get rid of title rows
    filter(!is.na(project))
  
  
  ## MERGE ALL PROJECTS
  ma_fund <- ma_fund_raw %>%
    # get funding amount, set all projects as funded, keep only new columns and srf_id for matching
    mutate(funding_amount = clean_numeric_string(iup_cost_2022),
           expecting_funding = "Yes") %>%
    select(srf_id, funding_amount, expecting_funding)
  
  # join funded data to all projects
  ma_clean <- left_join(ma_comp, ma_fund) %>%
    # fill created NAs and set funding status
    mutate(funding_amount = replace_na(funding_amount, "No Information"),
           expecting_funding = case_when(
             expecting_funding == "Yes" ~ "Yes",
             TRUE ~ "No"
           ),
           # "For projects listed under “Planning Projects” section of Funding List that include “(LR)” in Applicant, the amount under 2022IUP Cost is considered Principal Forgiveness." -> 
           # Manually identify projects under the Planning Projects subheading and set their funding_amount as principal_forgiveness_amount or set to 0
           principal_forgiveness_amount = case_when(
             srf_id %in% c("7187", "7082", "7181", "7159", "7174") ~ funding_amount,
             TRUE ~ "No Information"
           ),
           state = "Massachusetts",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_rank = as.character(NA),
           pwsid = replace_na(pwsid, "No Information"),
           project_score = replace_na(project_score, "No Information")
           ) %>%
    # drop column used for matching
    select(-srf_id) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(ma_clean)
  rm(list=setdiff(ls(), "ma_clean"))
  
  return(ma_clean)
}