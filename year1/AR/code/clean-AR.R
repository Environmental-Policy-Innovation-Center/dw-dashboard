source("resources.R")

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
    mutate(project_rank = as.numeric(no)) %>%
    filter(!is.na(project_rank)) %>%
    ## process numeric columns
    mutate(
      ## getting rid of $ and comma and replacing blanks with zero         
      population = clean_numeric_string(population),
      funding_amount = clean_numeric_string(project_cost),
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
      project_score = gsub(",", "", total_points),
      project_rank = as.character(project_rank),
      # all projects in ppl are applicants, see Merged for fundable projects
      expecting_funding = "No",
    ) %>%
    ## keep relevant columns
    select(borrower, pwsid, project_description, project_type,
           project_rank, project_score, funding_amount, 
           disadvantaged, population, expecting_funding)
  
  
  ### FUNDABLE
  
  ar_fund <- ar_merge %>% 
    clean_names() %>%
    select(-b_c_date_actual_estimated, -term_in_years, -interest_rate,
           -green_project_reserve_amt_estimate, -gpr_category_estimate,
           -mhi, -small_system_y_n) %>%
    # process numeric columns
    mutate(population = clean_numeric_string(population),
           funding_amount = clean_numeric_string(project_cost),
           principal_forgiveness = clean_numeric_string(additional_subsidy)
    ) %>%
    # process text columns
    mutate(borrower = str_squish(pws),
           pwsid = case_when(
             nchar(pws_id) == 2 ~ paste0("AR00000", pws_id),
             nchar(pws_id) == 3 ~ paste0("AR0000", pws_id),
             nchar(pws_id) == 4 ~ paste0("AR000", pws_id)),
           project_description = str_squish(project_description),
           project_score = str_replace_all(total_points,"[^0-9.]", ""),
           project_rank = str_replace_all(no,"[^0-9.]", ""),
           project_type = case_when(lsl == "X" ~ "Lead",
                                    ec == "X" ~ "Emerging Contaminants",
                                    TRUE ~ "General"),
           disadvantaged = str_to_sentence(disadvantaged_community),
           expecting_funding = "Yes",
    ) %>%
    select(project_rank, project_score, borrower, pwsid, project_description, 
           funding_amount, principal_forgiveness, population, disadvantaged, project_type, expecting_funding)
  
  # expect (760,11) -> (760,13)
  ar_clean <- bind_rows(ar_app, ar_fund) %>%
    mutate(pwsid = replace_na(pwsid, "No Information"),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
           disadvantaged = replace_na(disadvantaged, "No Information"),
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA),
           state = "Arkansas",
           state_fiscal_year = "2023") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(ar_clean)
  rm(list=setdiff(ls(), "ar_clean"))

  return(ar_clean)
}