library(tidyverse)
library(data.table)
library(janitor)

clean_ga <- function() {
  

## BASE IUP
# (60,14)
ga_raw <- fread("year1/GA/data/10-Georgia_Comp_clean.csv",
                colClasses = "character", na.strings = "") %>%
  clean_names()

# (60,14) -> (60,8)
ga_base <- ga_raw %>%
  # remove columns
  select(-est_notice_to_proceed, -est_construction_start, -est_construction_completion, -
           est_interst_rate, -est_terms) %>%
  # process numeric columns
  mutate(
    population = as.numeric(str_replace_all(population,"[^0-9.]","")),
    funding_amount = as.numeric(str_replace_all(total_disburs, "[^0-9.]","")),
    project_cost = as.numeric(str_replace_all(total_project_cost, "[^0-9.]","")),
  ) %>%
  # process text columns
  mutate(borrower = str_squish(community),
         project_description = str_squish(project_description),
         state_score = str_replace_all(project_score,"[^0-9.]",""),
         project_type = case_when(
           grepl("Lead", project_description, ignore.case=TRUE) ~ "Lead",
           grepl("PFAS", project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
           TRUE ~ "General"),
         # Affordability Score in Attachment 1: 2022 Comprehensive List in PPL is 29 or above.
         disadvantaged = case_when(
           affordability_score >= 29 ~ "Yes",
           TRUE ~ "No"
         ),
         funding_status = case_when(
           funding_amount > 0 ~ "Funded",
           TRUE ~ "Not Funded"
         ),
  ) %>% 
  select(state_score, borrower, project_description, funding_amount, project_cost,
         population, disadvantaged, funding_status, project_type)



## SUPPLEMENTAL IUP

ga_supp_1 <- fread("year1/GA/data/10-Georgia_Supplemental_IUP_A1.csv",
                   colClasses = "character", na.strings = "") %>%
  clean_names()

ga_supp_2 <- fread("year1/GA/data/10-Georgia_Supplemental_IUP_A2.csv",
                   colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  select(project, total_disburs) %>%
  # manually standardize names for matching
  mutate(project = str_replace(project, "Clayton County Water Authority", "Clayton County Water Authority (CCWA)"),
         total_disburs = str_squish(total_disburs)) %>%
  rename(community = project)

ga_supp <- ga_supp_1 %>%
  left_join(ga_supp_2) %>%
  # remove columns
  select(-est_notice_to_proceed, -est_construction_start, -est_construction_completion, -
           est_interst_rate, -est_terms) %>%
  # process numeric columns
  mutate(
    population = as.numeric(str_replace_all(population,"[^0-9.]","")),
    funding_amount = as.numeric(str_replace_all(total_disburs, "[^0-9.]","")),
    project_cost = as.numeric(str_replace_all(total_project_cost, "[^0-9.]","")),
  ) %>%
  # process text columns
  mutate(borrower = str_squish(community),
         project_description = str_squish(project_description),
         state_score = str_replace_all(project_score,"[^0-9.]",""),
         project_type = case_when(
           grepl("Lead", project_description, ignore.case=TRUE) ~ "Lead",
           grepl("PFAS", project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
           TRUE ~ "General"),
         # Affordability Score in Attachment 1: 2022 Comprehensive List in PPL is 29 or above.
         disadvantaged = case_when(
           affordability_score >= 29 ~ "Yes",
           TRUE ~ "No"
         ),
         funding_status = case_when(
           funding_amount > 0 ~ "Funded",
           TRUE ~ "Not Funded"
         )
  ) %>% 
  select(state_score, borrower, project_description, funding_amount, project_cost,
         population, disadvantaged, funding_status, project_type)



## LEAD IUP
ga_lead_1 <- fread("year1/GA/data/10-Georgia_Lead_IUP_A1.csv",
                   colClasses = "character", na.strings = "") %>%
  clean_names()

ga_lead_2 <- fread("year1/GA/data/10-Georgia_Lead_IUP_A2.csv",
                   colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  select(project, total_disburs) %>%
  # manually standardize names for matching
  mutate(
    project = str_squish(str_replace(project, "Bartow County Water", "Bartow County Water Department")),
    project = str_squish(str_replace(project, "Lee County Water Authority", "Lee County Utility Authority")),
    project = str_squish(str_replace(project, "City of Monroe", "Monroe County")),
    project = str_squish(str_replace(project, "Cobb County Water Authority", "Cobb County Water System")),
    project = str_squish(str_replace(project, "Jackson County Water Authority", "Jackson County Water and Sewerage Authority")),
    total_disburs = str_squish(total_disburs)) %>%
  rename(community = project)

ga_lead <- ga_lead_1 %>%
  # remove asterisks for joining second table by community name
  mutate(community = str_squish(str_remove_all(community, "\\*"))) %>%
  full_join(ga_lead_2) %>%
  # process numeric columns
  mutate(
    population = as.numeric(str_replace_all(x2020_pop,"[^0-9.]","")),
    funding_amount = as.numeric(str_replace_all(total_disburs, "[^0-9.]","")),
    project_cost = as.numeric(str_replace_all(total_project_cost, "[^0-9.]","")),
  ) %>%
  # process text columns
  mutate(borrower = str_squish(community),
         project_description = str_squish(project_description),
         state_score = str_replace_all(affordability_score,"[^0-9.]",""),
         project_type = "Lead",
         # Affordability Score in Attachment 1: 2022 Comprehensive List in PPL is 29 or above.
         disadvantaged = case_when(
           as.numeric(state_score) >= 29 ~ "Yes",
           TRUE ~ "No"
         ),
         funding_status = case_when(
           funding_amount > 0 ~ "Funded",
           TRUE ~ "Not Funded"
         ),
  ) %>% 
  select(state_score, borrower, project_description, funding_amount, project_cost,
         population, disadvantaged, funding_status, project_type)


## EC IUP
# (2,10)
ga_ec_1 <- fread("year1/GA/data/10-Georgia_EC_IUP_A1.csv",
                 colClasses = "character", na.strings = "") %>%
  clean_names()

# (2,10) -> (2,2)
ga_ec_2 <- fread("year1/GA/data/10-Georgia_EC_IUP_A2.csv",
                 colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  select(project, total_disburs) %>%
  # prep for merging with appendix 1 table
  rename(community = project) %>%
  mutate(community = str_squish(str_remove_all(community, "\\*")))

# process and merge GA EC tables
# -> (2,10)
ga_ec <- ga_ec_1 %>%
  left_join(ga_ec_2) %>%
  # process numeric columns
  mutate(population = as.numeric(str_replace_all(x2019_pop,"[^0-9.]","")),
         project_cost = as.numeric(str_replace_all(total_project_cost,"[^0-9.]","")),
         funding_amount = as.numeric(str_replace_all(total_disburs,"[^0-9.]","")),
  ) %>%
  # process text columns
  mutate(borrower = str_squish(community),
         project_description = str_squish(project_description),
         project_type = "Emerging Contaminants",
         state_score = str_replace_all(affordability_score,"[^0-9.]",""),
         # Affordability Score in Attachment 1: 2022 Comprehensive List in PPL is 29 or above.
         disadvantaged = case_when(
           as.numeric(state_score) >= 29 ~ "Yes",
           TRUE ~ "No"
         ),
         funding_status = case_when(
           funding_amount > 0 ~ "Funded",
           TRUE ~ "Not Funded"
         )
  ) %>%
  select(state_score, borrower, project_description, funding_amount, project_cost,
         population, disadvantaged, funding_status, project_type)


# join four tables together
# NOTE: for funding_amount we would sum together values from base and supplemental, but there are no projects in common on both lists, so summing is not necessary, we can just bind the rows together.
# -> (323,10)
ga_clean <- bind_rows(ga_base, ga_supp, ga_lead, ga_ec) %>%
  mutate(state = "Georgia",
         category = "3")

rm(list=setdiff(ls(), "ga_clean"))

return(ga_clean)
}