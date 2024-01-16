library(tidyverse)
library(data.table)
library(janitor)

clean_fl <- function() {

  fl_fund <- fread("year1/FL/data/9-Florida_ppl_fundable.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # format numeric columns
    mutate(funding_amount = as.numeric(str_replace_all(authorized_loan_amount,"[^0-9.]","")),
           principal_forgiveness_amount = as.numeric(str_replace_all(principal_forgiveness_amt,"[^0-9.]","")),
    ) %>%
    # formant text columns
    mutate(borrower = str_squish(applicant),
           project_name = str_squish(project_number),
           project_description = str_squish(project_description),
           state_score = str_replace_all(priority_score,"[^0-9.]",""),
           funding_status = "Funded") %>%
    select(borrower, project_name, project_description, state_score, funding_amount, 
           principal_forgiveness_amount, funding_status)
  
  #NOTE: This file was created manually as it only includes two rows
  fl_waiting <- fread("year1/FL/data/9-Florida_ppl_WaitingList.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # format number columns
    mutate(project_cost = as.numeric(str_replace_all(estimated_unfunded_cost,"[^0-9.]",""))) %>%
    # format text columns 
    mutate(borrower = str_squish(applicant),
           project_name = str_squish(project_nbr),
           project_description = str_squish(project_description),
           state_score = str_squish(priority_score),
           funding_status = "Not Funded") %>%
    select(borrower, project_name, project_description, state_score, project_cost, funding_status)
    
    
  fl_clean <- bind_rows(fl_fund, fl_waiting) %>%
    mutate(state = "Flordia",
           project_type = "General",
           category = "1")
  
  # rm(list=setdiff(ls(), "fl_clean"))

  return(NULL)
}