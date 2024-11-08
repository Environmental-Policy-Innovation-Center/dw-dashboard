source("resources.R")

clean_mi <- function() {
  
  # (112,24)
  mi_raw <- fread("year1/MI/data/22-Michigan_PPL.csv",
                  colClasses = "character", na.strings = "")
  # -> (110,12)
  mi_clean <- mi_raw %>%
    ## clean names
    clean_names() %>%
    ## get rid of rows without projects
    filter(!is.na(project_number)) %>%
    ## rowwise operations for sums
    rowwise() %>%
    ## make relevant columns numbers
    mutate(project_rank = as.numeric(gsub(",", "", rank)),
           population = clean_numeric_string(population),
           ## sum of columns, getting rid of $ and comma and replacing blanks with zero
           funding_amount = sum(as.numeric(gsub("\\$|,", "", dwsrf_loan)),
                                as.numeric(gsub("\\$|,", "", dwsrf_pf)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_supplemental_loan)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_supplemental_pf)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_emerging_contaminants_pf)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_loan)),
                                as.numeric(gsub("\\$|,", "", bil_dwsrf_lslr_pf)),
                                na.rm = T),
           principal_forgiveness = sum(as.numeric(str_replace_all(total_pf_grant,"[^0-9.]", "")),
                                              -as.numeric(str_replace_all(arp_grant, "[^0-9.]", "")),
                                              na.rm = T)) %>%
    ungroup() %>%
    ## standardize names
    rename(borrower = applicant_name,
           cities_served = project_location) %>%
    ## create project type column
    mutate(funding_amount = clean_numeric_string(funding_amount),
           funding_amount = case_when(
             funding_amount == 0 ~ "No Information",
             TRUE ~ funding_amount),
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           principal_forgiveness = case_when(
             principal_forgiveness == 0 ~ "No Information",
             TRUE ~ principal_forgiveness),
           project_type = case_when(lslr_costs != "" ~ "Lead",
                                    pfas_costs != "" ~ "Emerging Contaminants"),
           project_type = replace_na(project_type, "General"),
           expecting_funding = case_when(project_rank <= 70 ~ "Yes",
                                      project_rank > 70 ~ "No"),
           # with project_rank used for specifying funding, return to string
           project_rank = as.character(project_rank),
           project_score = str_squish(total_points),
           project_id = str_squish(project_number),
           project_cost = clean_numeric_string(estimated_project_cost),
           disadvantaged = replace_na(disadvantaged, "No"),
           state = "Michigan",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           pwsid = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(mi_clean)
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}