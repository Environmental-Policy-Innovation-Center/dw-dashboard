library(tidyverse)
library(data.table)
library(janitor)

clean_mo <- function() {

  # this is the initially scraped IUP from 10/11/22,
  # however, it is structurally the same as the list in the amended IUP dated 7/11/23, so we keep it and amend individual values below
  # (43,12) -> (43,13)
  mo_ppl <- fread("year1/MO/data/25-Missouri_PPL_Cleaned.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(
      # Projects included on the General Funding List are considered Funded. Projects included on the General Contingency List, General Planning List are considered Not Funded.
      funding_status = case_when(
        # list projects in planning list
        project_number %in% c("DW291386-01", "DW291170-02", "DW291387-01", "DW291393-01", "DW291388-01", 
                              "DW291389-01", "DW291390-01", "DW291391-01", "DW291392-01", "DW291314-04") ~ "Not Funded",
        TRUE ~ "Funded")
    )
  
  # read in lead tables from the amended IUP dated 7/11/23
  mo_lead_fundable <- fread("year1/MO/data/25-Missouri_LSLFundable.csv",
                            colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(
      funding_status = "Funded",
    )
  
  mo_lead_contingency <- fread("year1/MO/data/25-Missouri_LSLContingency.csv",
                               colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(
      funding_status = "Not Funded",
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
      population = as.numeric(str_replace_all(service_area_population,"[^0-9.]","")),
      requested_amount = as.numeric(str_replace_all(iup_amount_requested,"[^0-9.]","")),
      loan_amount = as.numeric(str_replace_all(loan_amount,"[^0-9.]","")),
      additional_subsidization_amount = as.numeric(
        str_replace_all(additional_subsidization_amount,"[^0-9.]","")),
      # replace NA with 0 for column adding
      additional_subsidization_amount = replace_na(additional_subsidization_amount, 0),
      loan_amount = replace_na(loan_amount, 0),
      funding_amount = loan_amount + additional_subsidization_amount
    ) %>%
    # process text columns
    mutate(borrower = str_squish(applicant),
           project_name = str_squish(project_number),
           state_score = str_replace_all(priority_points,"[^0-9.]",""),
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
           category = "1"
    ) %>%
    rename(principal_forgiveness_amount = additional_subsidization_amount) %>%
    select(state_score, borrower, project_description, population, funding_amount,
           principal_forgiveness_amount, disadvantaged, funding_status, state, category, project_type)  

  rm(list=setdiff(ls(), "mo_clean"))
  
  return(mo_clean)
}