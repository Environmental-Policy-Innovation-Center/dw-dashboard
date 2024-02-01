library(tidyverse)
library(data.table)
library(janitor)

clean_fl <- function() {

  fl_clean <- fread("year1/FL/data/9-Florida_ppl_fundable.csv",
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
           funding_status = case_when(
             project_name == "0604D" ~ "Not Funded",
             project_name == "53200" & project_description == "Treatment (DIW Pump Station & RO Effluent Removal) Distribution" ~ "Not Funded",
             TRUE ~ "Funded"),
           project_type = "General",
           category = "1",
           state = "Florida"
           ) %>%
    select(borrower, project_name, project_description, state_score, funding_amount, 
           principal_forgiveness_amount, funding_status, project_type, category, state)

  
  rm(list=setdiff(ls(), "fl_clean"))

  return(fl_clean)
}