
library(tidyverse)
library(data.table)
library(janitor)

clean_de <- function() {
  
  ### APPLICANT
  # only three projects on this list aren't already on the fundable list
  de_ppl <- fread("year1/DE/data/8-Delaware_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(
      (rank == "43" & water_system_borrower == "Artesian") |
        (rank == "44" & water_system_borrower == "Artesian") | 
        (rank == "39" & water_system_borrower == "Artesian")  
    ) %>%
    mutate(funding_status = "Not Funded") %>%
    rename(project_name = comprehensive_project_name)
  
  ### FUNDED
  de_fund <- fread("year1/DE/data/8-Delaware_PPL_fund.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(funding_status = "Funded") %>%
    rename(project_name = fundable_project_name)
  
  
  de_all <- bind_rows(de_fund, de_ppl)
  
  
  de_clean <- de_all %>%
    # rowwise operations for sums
    rowwise() %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population_served,"[^0-9.]","")),
           # sum of columns, getting rid of everything besides numbers and periods
           funding_amount = sum(as.numeric(str_replace_all(amount,"[^0-9.]", "")),
                                na.rm = T),
           principal_forgiveness_amount = sum(as.numeric(str_replace_all(anticipated_subsidy_amount,"[^0-9.]","")),
                                              na.rm = T),
    ) %>%
    ungroup() %>%
    # process text columns
    mutate(project_type = case_when(grepl("lead|lsl", project_name, ignore.case = T) ~
                                      "Lead",
                                    grepl("pfas", project_name, ignore.case = T) ~
                                      "Emerging Contaminants",
                                    TRUE ~ "General"),
           state_score = str_replace_all(total_points,"[^0-9.]",""),
           state_rank = str_replace_all(rank,"[^0-9.]",""),
           funding_status = "Funded",
           disadvantaged = case_when(!is.na(dac_a_e_w_u) ~ "Yes",
                                     TRUE ~ "No"),
           pwsid_number = str_replace(pwsid_number, "Delmar DE0000567 Holy Oak               DE0000568", "DE0000567"),
           borrower = str_squish(water_system_borrower),
           pwsid = str_squish(pwsid_number),
           project_name = str_squish(project_name),
           project_description = str_squish(project_description),
           state = "Delaware",
           category = "1"
    ) %>%
    select(borrower, pwsid, project_name, project_description, project_type,
           state_rank, state_score, funding_amount, principal_forgiveness_amount,
           disadvantaged, population, funding_status, state, category)
  
  
  rm(list=setdiff(ls(), "de_clean"))

return(de_clean)

}