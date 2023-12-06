library(tidyverse)
library(data.table)
library(janitor)


clean_ar <- function() {

  # the manually merged data from MDI includes fundable projects with columns from the comprehensive list
  # this is a combination of charts 3 and 4 from the IUP
  # (15,21)
  ar_merge <- fread("year1/AR/data/4-Arkansas_Merged.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names()
  
  
  ### APPLICANT
  
  # the comprehensive PPL includes applicant projects, (747,13)
  ar_comp <- fread("year1/AR/data/4-Arkansas.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names()
  
  ar_app <- ar_comp %>%
    ## get rid of rows that the rank is not a number (not a project, total columns, etc)
    mutate(state_rank = as.numeric(no)) %>%
    filter(!is.na(state_rank)) %>%
    ## process numeric columns
    mutate(
      ## getting rid of $ and comma and replacing blanks with zero         
      population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
      funding_amount = as.numeric(str_replace_all(project_cost,"[^0-9.]", "")),
    ) %>%
    mutate(
      # fix one-off NAs for pwsid
      pws_id = case_when(
        pws == "FORT SMITH WATER  UTILITIES" ~ "507",
        TRUE ~ pws_id
      ),
      pwsid = case_when(
        # pws_id is missing leading zeros, so standardize length to 9 characters based on length
        nchar(pws_id) == 1 ~ paste0("AR000000", pws_id), 
        nchar(pws_id) == 2 ~ paste0("AR00000", pws_id),
        nchar(pws_id) == 3 ~ paste0("AR0000", pws_id),
        nchar(pws_id) == 4 ~ paste0("AR000", pws_id)),
      # project types are denoted by X's in their respective column
      project_type = case_when(
        lsl == "X" ~ "Lead",
        ec == "X" ~ "Emerging Contaminants",
        TRUE ~ "General"),
      # disadv column is either YES, NO, or NA by default
      disadvantaged = case_when(
        disadv_antaged_y_n == "YES" ~ "Yes",
        disadv_antaged_y_n == "NO" ~ "No",
        TRUE ~ as.character(NA)),
      borrower = str_squish(pws),
      state_score = gsub(",", "", total_points),
      state_rank = as.character(state_rank),
      # all projects in ppl are applicants, see Merged for fundable projects
      funding_status = "Not Funded",
    ) %>%
    ## keep relevant columns
    select(borrower, pwsid, project_description, project_type,
           state_rank, state_score, funding_amount, 
           disadvantaged, population, funding_status)
  
  
  ### FUNDABLE
  
  ar_fund <- ar_merge %>% 
    clean_names() %>%
    select(-b_c_date_actual_estimated, -term_in_years, -interest_rate,
           -green_project_reserve_amt_estimate, -gpr_category_estimate,
           -mhi, -small_system_y_n) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
           funding_amount = as.numeric(str_replace_all(project_cost,"[^0-9.]", "")),
           principal_forgiveness_amount = as.numeric(str_replace_all(additional_subsidy,"[^0-9.]", ""))
    ) %>%
    # process text columns
    mutate(borrower = str_squish(pws),
           pwsid = case_when(
             nchar(pws_id) == 2 ~ paste0("AR00000", pws_id),
             nchar(pws_id) == 3 ~ paste0("AR0000", pws_id),
             nchar(pws_id) == 4 ~ paste0("AR000", pws_id)),
           project_description = str_squish(project_description),
           state_score = str_replace_all(total_points,"[^0-9.]", ""),
           state_rank = str_replace_all(no,"[^0-9.]", ""),
           project_type = case_when(lsl == "X" ~ "Lead",
                                    ec == "X" ~ "Emerging Contaminants",
                                    TRUE ~ "General"),
           disadvantaged = str_to_sentence(disadvantaged_community),
           funding_status = "Funded",
    ) %>%
    select(state_rank, state_score, borrower, pwsid, project_description, 
           funding_amount, principal_forgiveness_amount, population, disadvantaged, project_type, funding_status)
  
  # expect (760,11) -> (760,13)
  ar_clean <- bind_rows(ar_app, ar_fund) %>%
    mutate(state = "Arkansas",
           category = "1")
  
  rm(list=setdiff(ls(), "ar_clean"))

  return(ar_clean)
}