clean_or_y1 <- function() {

  # read in larger tables and clean up their column names for easier parsing
  or_hcc <- fread("year1/OR/data/or-hcc-ppl.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(applicant = applicant_loi_sd_number_1_county_rdo_rpm_2_population,
           primary_focus = primary_project_focus_e_g_treat_dist_storage_4,
           disadvantaged = disadvantaged_community_7,
           project_rating = project_rating_120_8
           )
  
  or_gen <- fread("year1/OR/data/or-gen-infra-res-ppl.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(applicant = applicant_loi_sd_number_1_county_rdo_rpm_2_population,
           primary_focus = primary_project_focus_e_g_treat_dist_storage_4,
           disadvantaged = disadvantaged_community_7,
           project_rating = project_rating_30_8)

  # combine similar tables, then extract multi-column information then cleaning as normal
  or_comb <- bind_rows(or_hcc, or_gen) 
  
  or_comb <- separate(or_comb, applicant, into = paste0("applicant_", 1:5), sep = "\n")
  
  or_comb <- or_comb %>%
    rename(borrower = applicant_1,
           project_id = applicant_2,
           community_served = applicant_3,
           population = applicant_5) %>%
    mutate(population = clean_numeric_string(population),
           requested_amount = clean_numeric_string(amount_req),
           project_description = str_squish(str_replace_all(primary_focus, "\n", " ")),
           project_rank = str_squish(rank),
           project_score = str_squish(project_rating),
           ) %>%
    select(community_served, borrower, project_id, requested_amount, project_description,
           population, disadvantaged, project_rank, project_score)
  
  # only one entry, but different enough columns to clean and then merge together
  or_em <-  fread("year1/OR/data/or-emergency-ej-ppl.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(disadvantaged = disadvantaged_community,
           borrower = applicant,
           project_id = applicant_number,
           community_served = county) %>%
    mutate(population = clean_numeric_string(population),
           requested_amount = clean_numeric_string(amount_req)) %>%
    select(community_served, borrower, project_id, requested_amount, population)
    
  # combine final tables and add common features
  or_clean <- bind_rows(or_comb, or_em) %>%
    mutate(state = "Oregon",
           expecting_funding = "No",
           state_fiscal_year = "2023",
           pwsid = as.character(NA),
           project_name = as.character(NA),
           project_type = as.character(NA),
           project_cost = as.character(NA),
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_description = replace_na(project_description, "No Information"),
           disadvantaged = replace_na(disadvantaged, "No Information"),
           project_rank = replace_na(project_rank, "No Information"),
           project_score = replace_na(project_score, "No Information")
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(or_clean)
  rm(list=setdiff(ls(), "or_clean"))
  
  return(or_clean)
}