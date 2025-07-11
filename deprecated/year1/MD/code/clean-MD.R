clean_md_y1 <- function() {
  
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
    mutate(funding_amount = clean_numeric_string(dwsrf_total_funding),
           dwsrf_base_loan_forgive = convert_to_numeric(dwsrf_base_loan_forgive, TRUE),
           bil_loan_forgive = convert_to_numeric(bil_loan_forgive, TRUE),
           principal_forgiveness = bil_loan_forgive + dwsrf_base_loan_forgive,
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           principal_forgiveness = case_when(
             principal_forgiveness == "0" ~ "No Information",
             TRUE ~ principal_forgiveness),
           population = clean_numeric_string(population)
    ) %>%
    # process text columns
    mutate(project_name = as.character(map(strsplit(project_name_mde_project_number, split = "\\(DW"), 1)),
           project_name = str_to_sentence(project_name),
           project_name = str_squish(project_name),
           project_description = str_squish(project_description),
           project_description = str_to_sentence(project_description),
           community_served = str_to_title(county),
           project_rank = str_replace_all(priority_rank,"[^0-9.]",""),
           project_score = str_replace_all(scoring_points,"[^0-9.]",""),
           borrower = str_to_title(applicant),
           disadvantaged = str_to_sentence(dac_community),
           pwsid = str_squish(pwsid),
           # manually fix pwsid typos
           pwsid = str_replace(pwsid, "MD006002", "MD0060002"),
           pwsid = str_replace(pwsid, "MD023006", "MD0230006"),
           pwsid = str_replace(pwsid, "MD210010", "MD0210010"),
           expecting_funding = "Yes"
    ) %>%
    select(project_rank, project_score, borrower, pwsid, project_name, project_description, funding_amount,
           principal_forgiveness, community_served, population, disadvantaged, expecting_funding)
  
  ### APPLICANT ###
  # this dataset is 42 projects, the first 24 of which appear to be on the fundable list above. Others will be listed as Not Funded.
  
  
  
  # (43,13) -> ()
  md_comp <- fread("year1/MD/data/20-Maryland_ComprehensivePPL.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # process numeric columns
    mutate(population = clean_numeric_string(population),
           project_cost = clean_numeric_string(total_cost),
           requested_amount = clean_numeric_string(requested_funding)
           ) %>%
    # process text columns
    mutate(project_rank = str_squish(rank),
           project_score = str_squish(points),
           project_name = str_squish(project_title),
           project_description = str_to_sentence(project_description),
           borrower = str_to_title(borrower),
           community_served = str_squish(county),
           disadvantaged = str_squish(disadvantaged),
           pwsid = str_squish(pwsid),
           expecting_funding = case_when(
             as.numeric(rank) <= 24 ~ "Yes",
             TRUE ~ "No")
    ) %>%
    select(project_rank, project_score, borrower, pwsid, project_name, project_description, requested_amount, project_cost,
           community_served, population, disadvantaged, expecting_funding)
  
  
  # Combine Fundable and Applicant
  
  # for projects on the fundable list, take the requested amount and project cost from the applicant list and merge on only those additional features
  md_comp_funded <- md_comp %>%
    filter(expecting_funding == "Yes") %>%
    select(project_rank, requested_amount, project_cost)
  
  md_comp_not_funded <- md_comp %>%
    filter(expecting_funding == "No")
  
  md_clean <- md_clean %>%
    left_join(md_comp_funded, by="project_rank")
  
  
  md_clean <- bind_rows(md_clean, md_comp_not_funded) %>%
    mutate(project_type = "General",
           state = "Maryland",
           state_fiscal_year = "2023",
           project_id = as.character(NA),
           pwsid = replace_na(pwsid, "No Information"),
           funding_amount = replace_na(funding_amount, "No Information"),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information")) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(md_clean)
  rm(list=setdiff(ls(), "md_clean"))
  
  return(md_clean)
}