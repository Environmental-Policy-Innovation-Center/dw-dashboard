library(tidyverse)
library(data.table)
library(janitor)

clean_vt <- function() {
  
  
  # PPL - Fundable and Applicants
  # Document: DWSRF FFY2022 Priority Funding List - General, IUP for FFY22, PPLs are on pp. 45-48. 
  # (99,12)
  vt_ppl <- fread("year1/VT/data/45-Vermont_PPL.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # drop the last three items which are ineligible projects
  vt_ppl <- vt_ppl[1:96,]
  
  # set fundable variable based on where the line is in the document
  vt_ppl$funding_status <- ""
  vt_ppl$funding_status[1:46] <- "Funded"
  vt_ppl$funding_status[47:96] <- "Not Funded"
  
  # (96,13) -> (93,10)
  vt_ppl <- vt_ppl %>%
    # drop rows if WSID is empty (sub-heading columns)
    filter(!is.na(wsid)) %>%
    select(-years, -admin_percent) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(user_popln,"[^0-9.]","")),
           loan_acct_gb = as.numeric(str_replace_all(loan_acct_gb,"[^0-9.]","")),
           gb_disadv_subsidy = as.numeric(str_replace_all(gb_disadv_subsidy,"[^0-9.]","")),
           # replace nas for adding columns
           gb_disadv_subsidy = replace_na(gb_disadv_subsidy, 0),
           loan_acct_gs = as.numeric(str_replace_all(loan_acct_gs,"[^0-9.]","")),
           gs_disadv_subsidy = as.numeric(str_replace_all(gs_disadv_subsidy,"[^0-9.]","")),
           gs_disadv_subsidy = replace_na(gs_disadv_subsidy, 0),
           funding_amount = as.numeric(str_replace_all(x2022_loan_amount,"[^0-9.]","")),
           principal_forgiveness_amount = gb_disadv_subsidy + gs_disadv_subsidy
    ) %>%
    # process text columns
    mutate(project_description = str_squish(project),
           borrower = str_squish(water_system_borrower),
           state_score = str_replace_all(score,"[^0-9.]",""),
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
    select(state_score, borrower, pwsid, project_description, population,
           funding_amount, principal_forgiveness_amount, project_type, funding_status, project_type)
  
  
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
    mutate(population = as.numeric(str_replace_all(user_popln,"[^0-9.]","")),
           funding_amount = as.numeric(str_replace_all(line_loan_amount,"[^0-9.]","")),
           principal_forgiveness_amount = as.numeric(str_replace_all(loan_forgiveness,"[^0-9.]","")),
    ) %>%
    # process text columns
    mutate(borrower = str_squish(water_system_borrower),
           state_score = str_replace_all(application_score,"[^0-9.]",""),
           pwsid = case_when(
             nchar(wsid) == 4 ~ paste0("VT000", wsid),
             nchar(wsid) == 5 ~ paste0("VT00", wsid)),
           project_type = "Lead",
           funding_status = "Funded",
           disadvantaged = case_when(
             principal_forgiveness_amount > 0 ~ "Yes",
             TRUE ~ "No"
           )
    ) %>%
    select(state_score, borrower, pwsid, funding_amount, principal_forgiveness_amount, population,
           disadvantaged, project_type, funding_status)
  
  
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
    mutate(population = as.numeric(str_replace_all(user_popln,"[^0-9.]","")),
           funding_amount = as.numeric(str_replace_all(line_items,"[^0-9.]","")),
           principal_forgiveness_amount = as.numeric(str_replace_all(line_items,"[^0-9.]","")),
    ) %>%
    mutate(borrower = str_squish(water_system),
           project_description = str_squish(project),
           pwsid = case_when(
             nchar(wsid) == 4 ~ paste0("VT000", wsid),
             nchar(wsid) == 5 ~ paste0("VT00", wsid)),
           state_score = str_replace_all(plist_app_score,"[^0-9.]",""),
           project_type = "Emerging Contaminants",
           funding_status = "Funded",
           disadvantaged = case_when(
             principal_forgiveness_amount > 0 ~ "Yes",
             TRUE ~ "No"
           )) %>%
    select(state_score, borrower, pwsid, funding_amount, principal_forgiveness_amount,
           project_description, disadvantaged, population, project_type, funding_status)
  
  
  # combine (174,13)
  vt_clean <- bind_rows(vt_ppl, vt_lead, vt_ec) %>%
    mutate(state="Vermont",
           category = "1")
  
  rm(list=setdiff(ls(), "vt_clean"))
  
  return(vt_clean)
}