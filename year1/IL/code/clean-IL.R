clean_il_y1 <- function() {
  
  ## Read in the manually separated versions of the PPL and lead data 
  ## from the excel spreadsheets sent by the state
  
  # (70,11)
  il_ppl_f <- fread("year1/IL/data/13-Illinois_PPL_Fundable.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "General",
           # replace "N/E" with 0 for principal forgiveness since N/E is different from the NAs in other docs
           disadvantaged_community_principal_forgiveness = str_replace(disadvantaged_community_principal_forgiveness, "N/E", "0")
    )
  
  # (19,11)
  il_ppl_a <- fread("year1/IL/data/13-Illinois_PPL_Applicant.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "No",
           project_type="General")
  
  # concat files of the same structure
  # determine disadvantaged based on defined PF column, to use in merging with lead below
  # (89,11)
  il_ppl <- bind_rows(il_ppl_a, il_ppl_f) %>%
    mutate(disadvantaged = case_when(
      clean_numeric_string(disadvantaged_community_principal_forgiveness) > 0 ~ "Yes",
      TRUE ~ "No"))
  
  # create (85,2) list of communities and DAC status
  dacs <- il_ppl %>%
    select(loan_applicant, disadvantaged) %>%
    distinct()
  
  # Preprocess Lead Projects
  # (18,10)
  il_lead_f <- fread("year1/IL/data/13-Illinois_Lead_Fundable.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "Lead")
  
  # (1,10)
  il_lead_a <- fread("year1/IL/data/13-Illinois_Lead_Applicant.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "No",
           project_type = "Lead")
  
  # merge lead projects together
  # (19,10)
  il_lead <- bind_rows(il_lead_a, il_lead_f)
  
  # merge lead projects with dac list
  il_lead <- merge(il_lead, dacs, all.x=TRUE, by="loan_applicant") %>%
    mutate(disadvantaged = replace_na(disadvantaged, "No Information") )
  
  
  # (108, 11)
  il_merge <- bind_rows(il_ppl, il_lead)
  
  # -> (108,11)
  il_clean <- il_merge %>%
    # drop columns
    select(-l17_number, -estimated_construction_start) %>%
    # process numeric columns
    mutate(funding_amount = clean_numeric_string(estimated_loan_amount),
           principal_forgiveness = clean_numeric_string(disadvantaged_community_principal_forgiveness),
           population = clean_numeric_string(service_population),
    ) %>%
    # process text columns
    mutate(borrower = str_squish(loan_applicant),
           borrower = str_to_title(borrower, locale="en"),
           project_score = str_replace_all(loan_priority_score, "[^0-9.]", ""),
           project_description = str_squish(project_description),
           project_description = str_to_sentence(project_description),
           state = "Illinois",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA),
           project_rank = as.character(NA),
    ) %>%
    # rename columns
    rename(pwsid = facility_no) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)
}