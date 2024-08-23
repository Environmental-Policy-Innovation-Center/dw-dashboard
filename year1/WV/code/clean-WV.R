source("resources.R")

clean_wv <- function() {
  
  # (163,36)
  # this manually merged updated file includes 
  wv_raw <- fread("year1/WV/data/48-West_Virginia_Merged_Updated.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (157,15)
  wv_clean <- wv_raw %>%
    ## get rid of rows that the rank is not a number (not a project)
    filter(!is.na(ranking) & county != "NA" & ranking !="Total") %>% 
    ## rowwise operations for sums
    rowwise() %>%
    ## make relevant columns numbers 
    mutate(
           principal_forgiveness =
             # Sum of DWTRF Principal Forgiveness from Base Grant, 
             # DWTRF Principal Forgiveness from Supplemental Grant, 
             # DWTRF Principal Forgiveness from Emerging Cont. Grant, and 
             # DWTRF Principal Forgiveness from LSLR Grant in Funding List
             sum(convert_to_numeric(dwtrf_principal_forgiveness_from_base_grant),
                 convert_to_numeric(dwtrf_principal_forgiveness_from_supplemental_grant),
                 convert_to_numeric(dwtrf_principal_forgiveness_from_emerging_cont_grant),
                 convert_to_numeric(dwtrf_principal_forgiveness_from_lslr_grant),
                 na.rm = T),
           funding_amount = 
             # Sum of all PF sources and
             # DWTRF Assistance from Base Grant, 
             # DWTRF Assistance from Supplemental Grant, 
             # DWTRF Assistance from LSLR Grant
             sum(principal_forgiveness,
                convert_to_numeric(dwtrf_assisitance_from_base_grant),
                convert_to_numeric(dwtrf_assisitance_from_supplemental_grant),
                convert_to_numeric(dwtrf_assisitance_from_lslr_grant),
                 na.rm=T)
    ) %>%
    ungroup() %>%
    # process numeric columns that don't require aggregating
    mutate(
      funding_amount = clean_numeric_string(funding_amount),
      funding_amount = case_when(funding_amount == "0" ~ "No Information", TRUE ~ funding_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      principal_forgiveness = case_when(principal_forgiveness == "0" ~ "No Information", TRUE ~ principal_forgiveness),
      population = clean_numeric_string(new_popu_lation),
      project_cost = clean_numeric_string(total_cost),
      requested_amount = clean_numeric_string(dwtrf_funding_requested)
    ) %>%
    ## split applicant name to borrower and project name
    ## get rid of wifta- then split at - if there is one
    mutate(project_type = case_when(grepl("lead", dwtrf_eligible_funding_type, ignore.case = T) ~
                                      "Lead",
                                    grepl("pfas", dwtrf_eligible_funding_type, ignore.case = T) ~
                                      "Emerging Contaminants",
                                    TRUE ~ "General"),
           expecting_funding = case_when(
             funding_amount != "No Information" | principal_forgiveness != "No Information" ~ "Yes",
             TRUE ~ "No"),
           project_score = str_replace_all(points,"[^0-9.]", ""),
           project_rank = str_replace_all(ranking,"[^0-9.]", ""),
           project_description = str_squish(project_description_x),
           project_name = str_squish(project_name),
           borrower = str_squish(system),
           disadvantaged = str_squish(disadvan_taged),
           community_served = str_squish(county),
           state = "West Virginia",
           state_fiscal_year = "2023",
           pwsid = as.character(NA),
           project_id = as.character(NA),
           project_description = replace_na(project_description, "No Information")
    ) %>%
    ## keep relevant columns
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(wv_clean)
  rm(list=setdiff(ls(), "wv_clean"))
  
  return(wv_clean)
}