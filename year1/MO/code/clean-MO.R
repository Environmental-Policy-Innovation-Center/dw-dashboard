source("resources.R")

clean_mo <- function() {

  # this is the initially scraped IUP from 10/11/22,
  # however, it is structurally the same as the list in the amended IUP dated 7/11/23, so we keep it and amend individual values below
  # (43,12) -> (43,13)
  mo_ppl <- fread("year1/MO/data/25-Missouri_PPL_Cleaned.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(
      # Projects included on the General Funding List are considered Funded. Projects included on the General Contingency List, General Planning List are considered Not Funded.
      expecting_funding = case_when(
        # list projects in planning list
        project_number %in% c("DW291386-01", "DW291170-02", "DW291387-01", "DW291393-01", "DW291388-01", 
                              "DW291389-01", "DW291390-01", "DW291391-01", "DW291392-01", "DW291314-04") ~ "No",
        TRUE ~ "Yes")
    )
  
  # read in lead tables from the amended IUP dated 7/11/23
  mo_lead_fundable <- fread("year1/MO/data/25-Missouri_LSLFundable.csv",
                            colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(
      expecting_funding = "Yes",
    )
  
  mo_lead_contingency <- fread("year1/MO/data/25-Missouri_LSLContingency.csv",
                               colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(
      expecting_funding = "No",
    )
  
  # (272,14)
  mo_combined <- bind_rows(mo_ppl, mo_lead_fundable, mo_lead_contingency)
  
  
  ### Amendments
  # The following changes bring the document in line with the most recent figures in the 7/11/23 IUPP
  # We also need to decide what to do with projects that are not on this amended list that were previously included.
  # mo_amended <- mo_ppl %>%
  #   mutate(
  #     iup_amount_requested = case_when(
  #       project_number == "DW291142-03" ~ "$951,500",
  #       project_number == "DW291384-01" ~ "$628,400",
  #       TRUE ~ iup_amount_requested),
  #     loan_amount = case_when(
  #       project_number == "DW291370-01" ~ "$1,640,505",
  #       project_number == "DW291142-02" ~ "$593,700",
  #       project_number == "DW291363-02" ~ "$892,238",
  #       project_number == "DW291142-03" ~ "$951,500",
  #       project_number == "DW291384-01" ~ "$628,400",
  #       project_number == "DW291301-02" ~ "$1,164,940",
  #       TRUE ~ loan_amount),
  #     additional_subsidization_amount = case_when(
  #       project_number == "DW291370-01" ~ "$3,000,000",
  #       project_number == "DW291142-02" ~ "$1,781,100",
  #       project_number == "DW291363-02" ~ "$2,676,712",
  #       project_number == "DW291378-01" ~ "$1,954,995",
  #       project_number == "DW291301-02" ~ "$5,361,000",
  #       TRUE ~ additional_subsidization_amount)
  #   )
  
  # -> (272,10)
  mo_clean <- mo_combined %>%
    # drop columns
    select(-carryover) %>%
    # process numeric columns
    mutate(
      population = clean_numeric_string(service_area_population),
      requested_amount = clean_numeric_string(iup_amount_requested),
      loan_amount = convert_to_numeric(loan_amount, TRUE),
      additional_subsidization_amount = convert_to_numeric(additional_subsidization_amount, TRUE),
      funding_amount = loan_amount + additional_subsidization_amount,
      funding_amount = clean_numeric_string(funding_amount),
      funding_amount = case_when(funding_amount=="0" ~ "No Information",
                                 TRUE ~ funding_amount),
      principal_forgiveness = clean_numeric_string(additional_subsidization_amount)
    ) %>%
    # process text columns
    mutate(borrower = str_squish(applicant),
           project_name = str_squish(project_number),
           project_score = str_replace_all(priority_points,"[^0-9.]",""),
           project_description = str_squish(description_needs_category),
           disadvantaged = case_when(
             disadvantaged == "D" ~ "Yes",
             TRUE ~ "No"),
           project_type = case_when(
             grepl("EC",additional_subsidization_funding_source) ~ "Emerging Contaminants",
             grepl("LSLI", description_needs_category) ~ "Lead",
             TRUE ~ "General"
           ),
           state = "Missouri",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           pwsid = as.character(NA),
           project_id = as.character(NA),
           project_cost = as.character(NA),
           project_rank = as.character(NA),
           project_score = replace_na(project_score, "No Information")
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  run_tests(mo_clean)
  rm(list=setdiff(ls(), "mo_clean"))
  
  return(mo_clean)
}