library(tidyverse)
library(data.table)
library(janitor)

clean_al <- function() {

### APPLICANT 

# (364, 11)
al_applicant <- fread("year1/AL/data/1-Alabama_ARPA_SRF_Combined_PPL.csv",
                      colClasses = "character", na.strings = "") %>%
  clean_names()

al_app_clean <- al_applicant %>%
  select(-date_pre_app_or_supp_received, -project_award_date_when_grant_is_signed,
         -unfunded_mat_ch_portion) %>%
  # process numeric columns
  mutate(
    requested_amount = as.numeric(str_replace_all(applied_for_project_amount,"[^0-9.]", "")),
    funding_amount = as.numeric(str_replace_all(funded_portion,"[^0-9.]", "")),
    principal_forgiveness_amount = as.numeric(str_replace_all(principal_forgiveness_and_or_grant,"[^0-9.]", "")),
  ) %>%
  # process text columns
  mutate(
    borrower = str_squish(applicant_name),
    project_description = str_squish(project_description),
    cities_served = str_squish(county),
    funding_status = case_when(
      project_approved_yes_or_no == "Yes" ~ "Funded",
      TRUE ~ "Not Funded"),
  ) %>%
  select(borrower, cities_served, project_description, requested_amount, funding_amount,
         principal_forgiveness_amount, funding_status)


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

al_combined_clean <- al_combined %>%
  select(-gpr_component_cost, -gpr_type, -gpr_project, -estimated_construction_start_date) %>%
  # process numeric columns
  mutate(population = as.numeric(str_replace_all(population_served,"[^0-9.]", "")),
         funding_amount = as.numeric(str_replace_all(assistance_amount,"[^0-9.]", "")),
         principal_forgiveness_amount = as.numeric(str_replace_all(additional_subsidization_principal_forgiveness,
                                                                   "[^0-9.]", "")),
  ) %>%
  # process text columns
  mutate(cities_served = str_squish(county_served),
         borrower = str_squish(str_replace_all(applicant_name, "\\*", "")),
         state_score = str_replace_all(priority_point_rank,"[^0-9.]", ""),
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
         funding_status = "Funded"
  ) %>%
  select(state_score, borrower, pwsid, project_name, project_description, funding_amount, principal_forgiveness_amount, 
         # add requested amount in once the applicant data is added to al_combined
         #requested_amount, 
         population, cities_served, disadvantaged, project_type, funding_status)

al_clean <- al_combined_clean %>%
  mutate(state = "Alabama",
         category = "1")

## Temporary: Recombine when ARPA question answered
# al_clean <- bind_rows(al_app_clean, al_combined_clean) %>%
#  mutate(state = "Alabama",
#         category = "1")

rm(list=setdiff(ls(), "al_clean"))

return(al_clean)
}
