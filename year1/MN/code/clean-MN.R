source("resources.R")

clean_mn_y1 <- function() {
  
  # (142, 16)
  mn_raw <- fread("year1/MN/data/23-Minnesota_IUP_manual.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # -> (142, 18)
  mn_clean <- mn_raw %>%
    # remove extra columns
    select(-est_const_start, v16) %>%
    # process numeric columns
    mutate(
      population = clean_numeric_string(population),
      # transform string to numeric and replace NAs specifically for adding columns
      est_lslr_pf = as.numeric(str_replace_all(estimated_dwrf_lead_service_line_replacement_principal_forgiveness_not_final_1,"[^0-9.]","")),
      est_lslr_pf = replace_na(est_lslr_pf, 0),
      est_ec_pf = as.numeric(str_replace_all(estimated_dwrf_emerging_contaminant_principal_forgiveness_not_final_2,"[^0-9.]","")),
      est_ec_pf = replace_na(est_ec_pf, 0),
      est_dac_pf = as.numeric(str_replace_all(estimated_dwrf_disadvantgd_community_principal_forgiveness_not_final_3,"[^0-9.]","")),
      est_dac_pf = replace_na(est_dac_pf, 0),
      # Sum of Estimated DWRF Lead, Estimated DWRF Emerging Contaminant, 
      # Estimated DWRF Disadvantaged Community
      principal_forgiveness = est_lslr_pf + est_ec_pf + est_dac_pf,
      # transform to numeric to save rows where numbers are present,
      # but fill in 0s from adding above with "No Information" for the final database
      # per State Sources Data Dictionary, funding amount = estimated loan + PFA
      funding_amount = as.numeric(str_replace_all(estimated_dwrf_loan,"[^0-9.]","")) + principal_forgiveness,
    ) %>%
    # process text columns
    mutate(
      borrower = str_squish(system_name),
      project_description = str_squish(project_description),
      project_rank = str_replace_all(mdh_2023_ppl_rank,"[^0-9.]",""),
      project_score = str_replace_all(mdh_2023_ppl_points,"[^0-9.]",""),
      project_id = str_squish(project_number),
      project_type = case_when(
        status == "Part A5,LSL" ~ "Lead",
        status == "Carryover,LSL" ~ "Lead",
        status == "Part A6,EC" ~ "Emerging Contaminants",
        TRUE ~ "General"),
      # Only projects with a dollar amount indicated for “Estimated DWRF Loan” and/or “Estimated Principal Forgiveness” are expected to receive SRF awards at this time and only these projects are included in the dashboard analysis.
      expecting_funding = case_when(
        !is.na(funding_amount) | principal_forgiveness > 0 ~ "Yes",
        TRUE ~ "No"),
      # Projects with a dollar amount or “eligible” in any of the “estimated principal forgiveness columns” or in the “estimated WIF grant” column are considered DACs.
      disadvantaged = case_when(
        # if not NA or two characters (the space and dash in otherwise empty cells in the raw data), mark as DAC for each column
        !is.na(estimated_dwrf_lead_service_line_replacement_principal_forgiveness_not_final_1) & nchar(  estimated_dwrf_lead_service_line_replacement_principal_forgiveness_not_final_1) > 2 ~ "Yes",
        !is.na(estimated_dwrf_emerging_contaminant_principal_forgiveness_not_final_2) & nchar(estimated_dwrf_emerging_contaminant_principal_forgiveness_not_final_2) > 2 ~ "Yes",
        !is.na(estimated_dwrf_disadvantgd_community_principal_forgiveness_not_final_3) & nchar(estimated_dwrf_disadvantgd_community_principal_forgiveness_not_final_3) > 2 ~ "Yes",
        !is.na(estimated_wif_grant_not_final_4) & nchar(estimated_wif_grant_not_final_4) > 2 ~ "Yes",
        TRUE ~ "No"
      ),
      funding_amount = clean_numeric_string(funding_amount),
      funding_amount = case_when(funding_amount == "0" ~ "No Information",
                                 TRUE ~ funding_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      principal_forgiveness = case_when(principal_forgiveness == "0" ~ "No Information",
                                        TRUE ~ principal_forgiveness),
      state = "Minnesota",
      state_fiscal_year = "2023",
      # add in NA columns for state
      pwsid = as.character(NA),
      community_served = as.character(NA),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = as.character(NA),
      
    ) %>%
    # subset columns
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(mn_clean)
  rm(list=setdiff(ls(), "mn_clean"))
  
  return(mn_clean)
}