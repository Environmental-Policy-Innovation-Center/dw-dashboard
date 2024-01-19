library(tidyverse)
library(data.table)
library(janitor)

clean_md <- function() {
  
  # Due to the format of the Maryland PPL table format, the PPL and Project Funding List are manually combined
  # before further cleaning and standardization
  # (24,19)
  md_raw <- fread("year1/MD/data/20-Maryland_PPL_manual.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (24,14)
  md_clean <- md_raw %>%
    #drop columns
    select(-comments, -v16, -system_size_ownership_small_large_public_private, -const_start) %>%
    # process numeric columns
    mutate(funding_amount = as.numeric(str_replace_all(dwsrf_total_funding,"[^0-9.]","")),
           dwsrf_base_loan_forgive = as.numeric(str_replace_all(dwsrf_base_loan_forgive,"[^0-9.]","")),
           #replace NA for adding columns
           dwsrf_base_loan_forgive = replace_na(dwsrf_base_loan_forgive, 0),
           bil_loan = as.numeric(str_replace_all(bil_loan,"[^0-9.]","")),
           bil_loan_forgive = as.numeric(str_replace_all(bil_loan_forgive,"[^0-9.]","")),
           bil_loan_forgive = replace_na(bil_loan_forgive, 0),
           principal_forgiveness_amount = bil_loan_forgive + dwsrf_base_loan_forgive,
           population = as.numeric(str_replace_all(population,"[^0-9.]",""))
    ) %>%
    # process text columns
    mutate(project_name = as.character(map(strsplit(project_name_mde_project_number, split = "\\(DW"), 1)),
           project_name = str_to_sentence(project_name),
           project_name = str_squish(project_name),
           project_description = str_squish(project_description),
           project_description = str_to_sentence(project_description),
           cities_served = str_to_title(county),
           state_rank = str_replace_all(priority_rank,"[^0-9.]",""),
           state_score = str_replace_all(scoring_points,"[^0-9.]",""),
           borrower = str_to_title(applicant),
           disadvantaged = str_to_sentence(dac_community),
           pwsid = str_squish(pwsid),
           # manually fix pwsid typos
           pwsid = str_replace(pwsid, "MD006002", "MD0060002"),
           pwsid = str_replace(pwsid, "MD023006", "MD0230006"),
           pwsid = str_replace(pwsid, "MD210010", "MD0210010"),
           funding_status = "Funded"
    ) %>%
    select(state_rank, state_score, borrower, pwsid, project_name, project_description, funding_amount,
           principal_forgiveness_amount, cities_served, population, disadvantaged, funding_status)
  
  ### APPLICANT ###
  # this dataset is 42 projects, the first 24 of which appear to be on the fundable list above. Others will be listed as Not Funded.
  
  
  
  # (43,13) -> ()
  md_comp <- fread("year1/MD/data/20-Maryland_ComprehensivePPL.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]","")),
           project_cost = as.numeric(str_replace_all(total_cost,"[^0-9.]","")),
           requested_amount = as.numeric(str_replace_all(requested_funding,"[^0-9.]",""))) %>%
    # process text columns
    mutate(state_rank = str_squish(rank),
           state_score = str_squish(points),
           project_name = str_squish(project_title),
           project_description = str_to_sentence(project_description),
           borrower = str_to_title(borrower),
           cities_served = str_squish(county),
           disadvantaged = str_squish(disadvantaged),
           pwsid = str_squish(pwsid),
           funding_status = case_when(
             as.numeric(rank) <= 24 ~ "Funded",
             TRUE ~ "Not Funded")
    ) %>%
    select(state_rank, state_score, borrower, pwsid, project_name, project_description, requested_amount, project_cost,
           cities_served, population, disadvantaged, funding_status)
  
  
  # Combine Fundable and Applicant
  
  # for projects on the fundable list, take the requested amount and project cost from the applicant list and merge on only those additional features
  md_comp_funded <- md_comp %>%
    filter(funding_status == "Funded") %>%
    select(state_rank, requested_amount, project_cost)
  
  md_comp_not_funded <- md_comp %>%
    filter(funding_status == "Not Funded")
  
  md_clean <- md_clean %>%
    left_join(md_comp_funded, by="state_rank")
  
  
  md_clean <- bind_rows(md_clean, md_comp_not_funded) %>%
    mutate(project_type = "General",
           state = "Maryland",
           category = "3")
  
  rm(list=setdiff(ls(), "md_clean"))
  
  return(md_clean)
}