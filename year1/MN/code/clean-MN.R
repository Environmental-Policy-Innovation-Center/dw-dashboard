library(tidyverse)
library(data.table)
library(janitor)

clean_mn <- function() {
  
  # (142, 16)
  mn_raw <- fread("year1/MN/data/23-Minnesota_IUP_manual.csv",
                  colClasses = "character", na.strings = "") %>% 
    clean_names()
  
  # -> (142, 11)
  mn_clean <- mn_raw %>%
    # remove extra columns
    select(-est_const_start, v16) %>%
    # process numeric columns
    mutate(
      population = as.numeric(str_replace_all(population,"[^0-9.]","")),
      # transform string to numeric and replace NAs specifically for adding columns
      est_lslr_pf = as.numeric(str_replace_all(estimated_dwrf_lead_service_line_replacement_principal_forgiveness_not_final_1,"[^0-9.]","")),
      est_lslr_pf = replace_na(est_lslr_pf, 0),
      est_ec_pf = as.numeric(str_replace_all(estimated_dwrf_emerging_contaminant_principal_forgiveness_not_final_2,"[^0-9.]","")),
      est_ec_pf = replace_na(est_ec_pf, 0),
      est_dac_pf = as.numeric(str_replace_all(estimated_dwrf_disadvantgd_community_principal_forgiveness_not_final_3,"[^0-9.]","")),
      est_dac_pf = replace_na(est_dac_pf, 0),
      # Sum of Estimated DWRF Lead, Estimated DWRF Emerging Contaminant, 
      # Estimated DWRF Disadvantaged Community
      principal_forgiveness_amount = est_lslr_pf + est_ec_pf + est_dac_pf,
      # transform to numeric to save rows where numbers are present,
      # but fill in 0s from adding above with "No Information" for the final database
      # per State Sources Data Dictionary, funding amount = estimated loan + PFA
      funding_amount = as.numeric(str_replace_all(estimated_dwrf_loan,"[^0-9.]","")) + principal_forgiveness_amount,
    ) %>%
    # process text columns
    mutate(
      borrower = str_squish(system_name),
      project_description = str_squish(project_description),
      state_rank = str_replace_all(mdh_2023_ppl_rank,"[^0-9.]",""),
      state_score = str_replace_all(mdh_2023_ppl_points,"[^0-9.]",""),
      pwsid = paste0("MN", as.character(map(strsplit(project_number, split = "-"), 1))),
      project_type = case_when(
        status == "Part A5,LSL" ~ "Lead",
        status == "Carryover,LSL" ~ "Lead",
        status == "Part A6,EC" ~ "Emerging Contaminants",
        TRUE ~ "General"),
      # Only projects with a dollar amount indicated for “Estimated DWRF Loan” and/or “Estimated Principal Forgiveness” are expected to receive SRF awards at this time and only these projects are included in the dashboard analysis.
      funding_status = case_when(
        !is.na(funding_amount) | principal_forgiveness_amount > 0 ~ "Funded",
        TRUE ~ "Not Funded"),
      # Projects with a dollar amount or “eligible” in any of the “estimated principal forgiveness columns” or in the “estimated WIF grant” column are considered DACs.
      disadvantaged = case_when(
        # if not NA or two characters (the space and dash in otherwise empty cells in the raw data), mark as DAC for each column
        !is.na(estimated_dwrf_lead_service_line_replacement_principal_forgiveness_not_final_1) & nchar(  estimated_dwrf_lead_service_line_replacement_principal_forgiveness_not_final_1) > 2 ~ "Yes",
        !is.na(estimated_dwrf_emerging_contaminant_principal_forgiveness_not_final_2) & nchar(estimated_dwrf_emerging_contaminant_principal_forgiveness_not_final_2) > 2 ~ "Yes",
        !is.na(estimated_dwrf_disadvantgd_community_principal_forgiveness_not_final_3) & nchar(estimated_dwrf_disadvantgd_community_principal_forgiveness_not_final_3) > 2 ~ "Yes",
        !is.na(estimated_wif_grant_not_final_4) & nchar(estimated_wif_grant_not_final_4) > 2 ~ "Yes",
        TRUE ~ "No"
      ),
      state = "Minnesota",
      category = "3",
    ) %>%
    # subset columns
    select(state_rank, state_score, borrower, pwsid, project_description, population, disadvantaged,
           funding_amount, principal_forgiveness_amount, funding_status, project_type, state, category)
  
  rm(list=setdiff(ls(), "mn_clean"))
  
  return(mn_clean)
}