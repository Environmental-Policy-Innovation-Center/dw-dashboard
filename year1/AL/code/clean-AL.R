source("resources.R")

clean_al <- function() {

  ### APPLICANT - Not included in dashboard, kept in case it becomes relevant
  
  # (364, 11)
#  al_applicant <- fread("year1/AL/data/1-Alabama_ARPA_SRF_Combined_PPL.csv",
#                        colClasses = "character", na.strings = "") %>%
#    clean_names()
#  
#  al_app_clean <- al_applicant %>%
#    select(-date_pre_app_or_supp_received, -project_award_date_when_grant_is_signed,
#           -unfunded_mat_ch_portion) %>%
#    # process numeric columns
#    mutate(
#      requested_amount = convert_to_numeric(applied_for_project_amount),
#     funding_amount = convert_to_numeric(funded_portion),
#      principal_forgiveness = convert_to_numeric(principal_forgiveness_and_or_grant),
#    ) %>%
#    # process text columns
#   mutate(
#      borrower = str_squish(applicant_name),
#      project_description = str_squish(project_description),
#      community_served = str_squish(county),
#      expecting_funding = case_when(
#        project_approved_yes_or_no == "Yes" ~ "Yes",
#       TRUE ~ "No"),
#    ) %>%
#    select(borrower, community_served, project_description, requested_amount, funding_amount,
#           principal_forgiveness, expecting_funding)
  
  
  ### FUNDABLE
  
  # (7,14)
  al_base <- fread("year1/AL/data/1-Alabama_Base-PPL.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # (19,14)
  al_bil <- fread("year1/AL/data/1-Alabama_BIL-PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # (3,14)
  al_lead <- fread("year1/AL/data/1-Alabama_Lead-PPL.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (29,14)
  al_combined <- bind_rows(al_base, al_bil, al_lead) 
  
  al_clean <- al_combined %>%
    select(-gpr_component_cost, -gpr_type, -gpr_project, -estimated_construction_start_date) %>%
    # process numeric columns
    mutate(funding_amount = clean_numeric_string(assistance_amount),
           principal_forgiveness = clean_numeric_string(additional_subsidization_principal_forgiveness),
           project_cost = as.character(NA),
           requested_amount = as.character(NA)
    ) %>%
    # process text columns
    mutate(community_served = str_squish(county_served),
           borrower = str_squish(str_replace_all(applicant_name, "\\*", "")),
           project_score = str_squish(priority_point_rank),
           population = clean_numeric_string(population_served),
           disadvantaged = case_when(
             disadvantaged_criteria == "Y" ~ "Yes",
             TRUE ~ "No"),
           pwsid = str_squish(pwsid_number),
           pwsid = str_replace(pwsid, "AL000341", "AL0000341"),
           project_name = str_squish(project_name),
           project_description = str_squish(project_description),
           project_description = str_replace(project_description, "Project Description: ", ""),
           project_type = case_when(
             grepl("LEAD", appropriation_fund_being_used, ignore.case=TRUE) ~ "Lead",
             TRUE ~ "General"),
           # ALL IUP projects are expected to be funded
           expecting_funding = "Yes",
           project_id = as.character(NA),
           project_rank = as.character(NA),
           state = "Alabama",
           state_fiscal_year = "2023"
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year)

  
  # # Merge back together if applicant data ever used
  # al_clean <- bind_rows(al_app_clean, al_combined_clean) %>%
  #  mutate(state = "Alabama",
  #         category = "1")
  
  run_tests(al_clean)
  rm(list=setdiff(ls(), "al_clean"))

  return(al_clean)
}
