source("resources.R")

clean_nh <- function() {
  
  # (190,32)
  nh_raw <- fread("year1/NH/data/29-NewHampshire_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (181,13)
  nh_clean <- nh_raw %>%
    # drop columns
    select(-a_water_quality_score, -b_quantity_deficiencies_score, 
           -c_treatment_score, -d_storage_score, -e_distribution_score, 
           -f_affordability_score, -g_cap_dev_score, -h_green_score, 
           -i_resiliency_score,
           -j_consolidation_interconnecti_on_score, -k_critical_infrastructur_e_score, 
           -l_project_readiness_score,
           -water_rate, -mhi, -affordability_index,
           -arpa_disadvantaged_co_op_or_epa_wiin_grant,
           -x2022_arpa_grant) %>%
    
    # drop empty rows
    filter(!is.na(rank)) %>%
    # drop "n/a" rows which are only ARPA Funding
    filter(rank != "n/a") %>%
    
    # process numeric columns
    mutate(population = clean_numeric_string(population),
           # these projects are true NA for all funding columns and should be set to No Information after math is done
           # all other projects have true 0s listed and should be kept that way
           bypass_projects = case_when(rank %in% c("1","5","13","14","18") ~ "Yes",
                                       TRUE ~ "No"),
           # transform all funding columns for summing
           # only NAs are the "Bypass..." applicant projects,
           # these will be filled with 0s, but are technically NA, 
           # but this will only matter when we expand the project to work with all Applicant projects 
           # and can be replaced with NA downstream for easier processing
           emerging_contaminants = convert_to_numeric(emerging_contaminants, TRUE),
           lead_service_line_lsl_loan_amount = convert_to_numeric(lead_service_line_lsl_loan_amount,TRUE),
           base_supplemental_loan_amount = convert_to_numeric(base_supplemental_loan_amount,TRUE),
           lsl_forgiveness = convert_to_numeric(lsl_forgiveness, TRUE),
           base_supplemental_forgiveness = convert_to_numeric(base_supplemental_forgiveness,TRUE),
           ## define funding amount as sum of loan, assumes it includes forgiveness_amount
           funding_amount = emerging_contaminants + lead_service_line_lsl_loan_amount + lsl_forgiveness
           + base_supplemental_loan_amount + base_supplemental_forgiveness,
           ## define PF as sum of forgiveness columns
           principal_forgiveness = lsl_forgiveness + base_supplemental_forgiveness + emerging_contaminants,
           requested_amount = clean_numeric_string(requested_loan_amount),
           funding_amount = case_when(
             bypass_projects == "Yes" ~ "No Information",
             TRUE ~ clean_numeric_string(funding_amount)),
           principal_forgiveness = case_when(
             bypass_projects == "Yes" ~ "No Information",
             TRUE ~ clean_numeric_string(principal_forgiveness)),
    ) %>%
    
    # process text columns
    mutate(pwsid = case_when(
      nchar(pws_number) == 7 ~paste0("NH", pws_number),
      nchar(pws_number) == 6 ~ paste0("NH0", pws_number),
      nchar(pws_number) == 5 ~ paste0("NH00", pws_number),
      pws_number == "1392200/139222 0/1392230" ~ "NH1392200/NH1392220/NH1392230"),
      # deal with multiple pwsid entries
      # pwsid = case_when(
      # grepl("/", pwsid) ~ as.character(map(strsplit(pwsid, split = "/"), 1)),
      # TRUE ~ pwsid),
      project_score = str_replace_all(total_score, "[^0-9.]", ""),
      project_rank = str_replace_all(rank, "[^0-9.]", ""),
      borrower = str_to_title(str_squish(applicant)),
      community_served = str_to_title(str_squish(system_town)),
      project_name = str_to_title(str_squish(project_name)),
      disadvantaged = case_when(
        financially_disadvantaged == "YES" | environmentall_y_disadvantaged == "YES" ~ "Yes",
        TRUE ~ "No"),
      # fundable indicated by highlighting in document, manually add projects by rank
      expecting_funding = case_when(
        project_rank %in% c("1","2","3","4","6","7","8","9","10","11","12","15","16","17",
                          "19","20","21","22","23","24","25","26","27","28","29","30") ~ "Yes",
        TRUE ~ "No"),
      project_type = case_when(
        emerging_contaminants > 0 ~ "Emerging Contaminants",
        lead_service_line_lsl_loan_amount > 0 | lsl_forgiveness > 0 ~ "Lead",
        funding_amount > 0 ~ "General",
        TRUE ~ "No Information"),
      state = "New Hampshire",
      state_fiscal_year = "2023",
      project_id = as.character(NA),
      project_cost = as.character(NA),
      project_description = as.character(NA),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(nh_clean)
  rm(list=setdiff(ls(), "nh_clean"))
  
  return(nh_clean)
}