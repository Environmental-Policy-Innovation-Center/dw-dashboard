clean_nv_y1 <- function() {
  
  # (172, 16)
  # Nevada Drinking Water State Revolving Fund Priority List Effective June 2022
  # https://ndep.nv.gov/uploads/water-financing-srf-drinkingwater-docs/DW_modified_FINAL_PL-Effective-June-2022.pdf
  nv_ppl <- fread("year1/NV/data/28-Nevada_PPL.csv",
                  colClass="character", na.strings="") %>%
    clean_names()
  
  # -> (169, 11) - drop is due to the removal of "Total" rows and non fundable projects
  nv_ppl <- nv_ppl %>%
    # remove non project rows
    filter(!grepl("Total", priority_number)) %>%
    # process numeric columns
    mutate(population = clean_numeric_string(project_population),
           requested_amount = clean_numeric_string(estimated_loan_amount)
           ) %>%
    # process text columns
    mutate(disadvantaged = case_when(
      is.na(d2) ~ "No",
      TRUE ~ "Yes"
    ),
    borrower = str_squish(entity),
    pwsid = str_squish(state_id),
    project_score = str_replace_all(revised_points, "[^0-9.]", ""),
    project_rank = str_replace_all(priority_number, "[^0-9.]", ""),
    project_description = str_squish(project_description)
    ) %>%
    select(borrower, pwsid, project_description, project_rank, project_score, requested_amount, 
           population, disadvantaged)
  
  
  # (28,15)
  ### Nevada Attachment C: DWSRF 2022 Fundable List DWSRF 2nd Amended Intended Use Plan SFY 2022
  nv_fund <- fread("year1/NV/data/28-Nevada_Fundinglist.csv",
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    # process numeric columns
    mutate(
      funding_amount = clean_numeric_string(total_assistance),
      # transform the components of PF into numeric and replace NAs for summing
      # TODO: Confirm whether disadvantaged_subsidy should also be included here
      base_disadvantaged_subsidy = convert_to_numeric(base_disadvantaged_subsidy, TRUE),
      base_additional_subsidy = convert_to_numeric(base_additional_subsidy, TRUE),
      disadvantaged_subsidy = convert_to_numeric(disadvantaged_subsidy, TRUE),
      principal_forgiveness = base_disadvantaged_subsidy + base_additional_subsidy + disadvantaged_subsidy,
      principal_forgiveness = case_when(principal_forgiveness == 0 ~ "No Information", TRUE ~ clean_numeric_string(principal_forgiveness))
    ) %>%
    # process text columns
    mutate(
      project_rank = str_replace_all(pl_rank, "[^0-9.]", ""),
      # replace the listing of 93,94, and 95 with only 95 to attach funding_amount to a single project
      # then manually set all state ranks to the PPL list to force correct matches
      project_rank = case_when(
        project_rank == "939495" ~ "95",
        project_rank == "57" ~ "60",
        project_rank == "61" ~ "62",
        project_rank == "97" ~ "100",
        project_rank == "122" ~ "126",
        project_rank == "129" & pws_id == "NV0000226" ~ "132",
        project_rank == "163" ~ "165",
        project_rank == "169" ~ "52",
        TRUE ~ project_rank),
      expecting_funding = "Yes",
    ) %>%
    # keep standardized columns to match onto the PPL List
    select(project_rank, funding_amount, principal_forgiveness, expecting_funding)
  
  
  ### Join PPL and funding list
  nv_clean <- merge(nv_ppl, nv_fund, all=TRUE, by="project_rank") %>%
    # assign variables consistent across both
    mutate(state = "Nevada",
           state_fiscal_year = "2023",
           project_type = "General",
           # manually update the two projects that are funded in project ranked 95, fill rest in as applicant
           expecting_funding = case_when(
             project_rank == "93" ~ "Yes",
             project_rank == "94" ~ "Yes",
             is.na(expecting_funding) ~ "No",
             TRUE ~ expecting_funding
           ),
           #manually fix one missing project description
           project_description = case_when(
             borrower == "Beatty Water & Sanitation District - DW" ~ "Water System Rehab",
             TRUE ~ project_description
           ),
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
           funding_amount = replace_na(funding_amount, "No Information")
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(nv_clean)
  rm(list=setdiff(ls(), "nv_clean"))
  
  return(nv_clean)
}