clean_vt_y1 <- function() {
  
  
  # PPL - Fundable and Applicants
  # Document: DWSRF FFY2022 Priority Funding List - General, IUP for FFY22, PPLs are on pp. 45-48. 
  # (99,12)
  vt_ppl <- fread("year1/VT/data/45-Vermont_PPL.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # drop the last three items which are ineligible projects
  vt_ppl <- vt_ppl[1:96,]
  
  # set fundable variable based on where the line is in the document
  vt_ppl$expecting_funding <- ""
  vt_ppl$expecting_funding[1:46] <- "Yes"
  vt_ppl$expecting_funding[47:96] <- "No"
  
  # (96,13) -> (93,10)
  vt_ppl <- vt_ppl %>%
    # drop rows if WSID is empty (sub-heading columns)
    filter(!is.na(wsid)) %>%
    select(-years, -admin_percent) %>%
    # process numeric columns
    mutate(population = clean_numeric_string(user_popln),
           funding_amount = clean_numeric_string(x2022_loan_amount),
           gb_disadv_subsidy = convert_to_numeric(gb_disadv_subsidy,TRUE),
           gs_disadv_subsidy = convert_to_numeric(gs_disadv_subsidy,TRUE),
           principal_forgiveness = gb_disadv_subsidy + gs_disadv_subsidy,
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           principal_forgiveness = case_when(principal_forgiveness == "0" ~ "No Information", TRUE ~ principal_forgiveness)

    ) %>%
    # process text columns
    mutate(project_description = str_squish(project),
           borrower = str_squish(water_system_borrower),
           project_score = str_replace_all(score,"[^0-9.]",""),
           # fill in leading zeros to standardize length
           pwsid = case_when(
             nchar(wsid) == 4 ~ paste0("VT000", wsid),
             nchar(wsid) == 5 ~ paste0("VT00", wsid)),
           # all ppl projects are non ec and non lead
           project_type = "General",
           # for fundable projects, if PF > 0, Disadvantaged 
           # because it is just the sum of GB_disadv_subsidy and gs_disadv_subsidy
           # Applicants assume unknown since PF will be 0 by default of not receiving subsidy
    ) %>%
    # subset columns
    select(project_score, borrower, pwsid, project_description, population,
           funding_amount, principal_forgiveness, project_type, expecting_funding, project_type)
  
  
  ## Import Lead tables independently to reconcile column head differences
  ## Document: DWSRF FFY22 Priority List - Lead service line removal and service line inventories, IUP for FFY22, pp. 49-50. 
  # (3,7)
  vt_lead_t1 <- fread("year1/VT/data/45-Vermont_Lead_Table1.csv",
                      colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # (70,7)
  vt_lead_t2 <- fread("year1/VT/data/45-Vermont_Lead_Table2.csv",
                      colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    rename(line_loan_amount = loan4,
           loan_forgiveness = loan_forgiveness2,
           water_system_borrower = water_system)
  
  # merge together lead tables to process columns
  # (73, 7)
  vt_lead <- bind_rows(vt_lead_t1, vt_lead_t2) 
  
  # -> (73,9)
  vt_lead <- vt_lead %>%
    select(-lsi_score3, -loan_portion_to_be_repaid) %>%
    # process numeric columns
    mutate(population = clean_numeric_string(user_popln),
           funding_amount = clean_numeric_string(line_loan_amount),
           principal_forgiveness = clean_numeric_string(loan_forgiveness),
    ) %>%
    # process text columns
    mutate(borrower = str_squish(water_system_borrower),
           project_score = str_replace_all(application_score,"[^0-9.]",""),
           pwsid = case_when(
             nchar(wsid) == 4 ~ paste0("VT000", wsid),
             nchar(wsid) == 5 ~ paste0("VT00", wsid)),
           project_type = "Lead",
           expecting_funding = "Yes",
           disadvantaged = case_when(
             principal_forgiveness != "No Information" ~ "Yes",
             TRUE ~ "No"
           )
    ) %>%
    select(project_score, borrower, pwsid, funding_amount, principal_forgiveness, population,
           disadvantaged, project_type, expecting_funding)
  
  
  # Emerging Contaminants
  # (17,7) -> (8,7)
  vt_ec <- fread("year1/VT/data/45-Vermont_EC.csv",
                 colClasses = "character", na.strings = "") %>% 
    clean_names()
  # keep only the top section of the table that are not internal account transfers
  vt_ec <- vt_ec[2:9,]
  
  # -> (8,9)
  vt_ec <- vt_ec %>%
    select(-subtotals) %>%
    mutate(population = clean_numeric_string(user_popln),
           funding_amount = clean_numeric_string(line_items),
           principal_forgiveness = clean_numeric_string(line_items),
    ) %>%
    mutate(borrower = str_squish(water_system),
           project_description = str_squish(project),
           pwsid = case_when(
             nchar(wsid) == 4 ~ paste0("VT000", wsid),
             nchar(wsid) == 5 ~ paste0("VT00", wsid)),
           project_score = str_replace_all(plist_app_score,"[^0-9.]",""),
           project_type = "Emerging Contaminants",
           expecting_funding = "Yes",
           disadvantaged = case_when(
             principal_forgiveness != "No Information" ~ "Yes",
             TRUE ~ "No"
           )) %>%
    select(project_score, borrower, pwsid, funding_amount, principal_forgiveness,
           project_description, disadvantaged, population, project_type, expecting_funding)
  
  
  # combine (174,13)
  vt_clean <- bind_rows(vt_ppl, vt_lead, vt_ec) %>%
    mutate(state="Vermont",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA),
           project_rank = as.character(NA),
           project_score = replace_na(project_score, "No Information"),
           project_description = replace_na(project_description, "No Information"),
           disadvantaged = replace_na(disadvantaged, "No Information")
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(vt_clean)
  rm(list=setdiff(ls(), "vt_clean"))
  
  return(vt_clean)
}