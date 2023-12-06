library(tidyverse)
library(data.table)
library(janitor)

clean_co <- function() {

## Appendix B
  
  # (165,11)
  co_b <- fread("year1/CO/data/7-Colorado_AppendixB.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names()
  
  
  # currently assumes that all funding should be added into a single funding column because, "Specific PF amounts are not available in this IUP, probably because p36 states, "Eligibility for BIL PF will be determined when a pre-qualification application is submitted." Also, on p6 they mention that "Amounts available will vary, and at times, may not be available."
  co_aggregate <- co_b %>%
    # process numeric columns for aggregating
    mutate(funding_amount = as.numeric(str_replace_all(approved_loan_amount,"[^0-9.]", "")),
    ) %>%
    # total different sources of funding by project ID
    group_by(project_number) %>%
    summarize(total_funding = sum(funding_amount, na.rm=TRUE))
  
  co_b_clean <- co_b %>%
    left_join(co_aggregate) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(pop,"[^0-9.]", "")),
           funding_amount = as.numeric(str_replace_all(total_funding,"[^0-9.]", "")),
           project_cost = as.numeric(str_replace_all(estimated_project_cost,"[^0-9.]", "")),
    ) %>%
    # process text columns
    mutate(pwsid= str_squish(pws_id_number),
           project_name = str_squish(project_number),
           borrower = str_squish(facility),
           cities_served = str_squish(county),
           state_score = str_replace_all(pts,"[^0-9.]", ""),
           disadvantaged = case_when(
             dac == "Y" ~ "Yes",
             TRUE ~ "No"
           ),
           project_type = "General",
           project_description = str_squish(project_description),
           funding_status = case_when(
             funding_amount > 0 ~ "Funded",
             TRUE ~ "Not Funded")) %>%
    select(borrower, pwsid, project_name, project_description, state_score, project_cost,
           funding_amount, population, cities_served, disadvantaged, project_type, funding_status) %>%
    distinct()
  
  
  # There are currently some rows with NA project_descriptions that need to be removed where they are duplicates, but not all NA project descriptions should be dropped as some are simply missing from the data.
  co_antijoin <- co_b_clean %>%
    filter(is.na(project_description) & project_name %in% c("132321D-Q", "140401D-M", "140421D-I", "140521D-Q", 
                                                            "140771D-I", "140951D-I", "142361D-Q", "230340D")) 
  
  # (-> 134,13) # should be same number as when aggregated by project number and is
  co_b_clean <- co_b_clean %>%
    anti_join(co_antijoin) %>%
    # then, get rid of one project that is duplicated because of cut/cropped project descriptions from scraping
    mutate(
      project_description = case_when(
        project_name == "132321D-Q" ~ "New Water Treatment Facilities; Improvement/Expansion of Water Treatment Facilities; Connection to a New or Existing Water Treatment Facility; Construction or Rehabilitation of Distribution and/or Transmission Lines; Water Storage Facilities; Water Supply Facilities; Water Meters; Source Water Protection Plan; Green Infrastructure",
        project_name == "140831D-Q" ~ "Improvement/Expansion of Water Treatment Facilities; Construction or Rehabilitation of Distribution and/or Transmission Lines; Water Storage Facilities; Water Supply Facilities; Water Rights",
        project_name == "141951D-I" ~ "Construction or Rehabilitation of Distribution and/or Transmission Lines; Water Meters; Green Infrastructure",
        project_name == "141970D" ~ "Improvement/Expansion of Water Treatment Facilities; Construction or Rehabilitation of Distribution and/or Transmission Lines; Water Storage Facilities; Water Supply Facilities; Water Meters; Source Water Protection Plan",
        project_name == "142130D" ~ "Improvement/Expansion of Water Treatment Facilities; Construction or Rehabilitation of Distribution and/or Transmission Lines; Water Storage Facilities; Water Supply Facilities; Water Meters",
        project_name == "170361D-Q" ~ "New Water Treatment Facilities; Improvement/Expansion of Water Treatment Facilities; Connection to a New or Existing Water Treatment Facility; Construction or Rehabilitation of Distribution and/or Transmission Lines; Water Storage Facilities; Water Supply Facilities; Water Meters; Source Water Protection Plan",
        project_name == "230450D" ~ "Connection to a New or Existing Water Treatment Facility; Construction or Rehabilitation of Distribution and/or Transmission Lines; Water Meters",
        project_name == "142220D" ~ "Improvement/Expansion of Water Treatment Facilities; Construction or Rehabilitation of Distribution and/or Transmission Lines; Water Storage Facilities; Water Supply Facilities; Water Meters; Source Water Protection Plan; Green Infrastructure",
        project_name == "142341D-Q" ~ "New Water Treatment Facilities; Improvement/Expansion of Water Treatment Facilities; Consolidation of Water Treatment Facilities; Connection to a New or Existing Water Treatment Facility; Construction or Rehabilitation of Distribution and/or Transmission Lines; Water Storage Facilities; Water Supply Facilities; Water Meters; Water Rights; Source Water Protection Plan; Green Infrastructure",
        TRUE ~ project_description)
    ) %>%
    distinct()
  
  
  
  ## Appendix B.1
  
  # (104,13)
  co_b1 <- fread("year1/CO/data/7-Colorado_AppendixB1.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (104,13)
  co_b1_clean <- co_b1 %>%
    # drop unused columns
    select(-term_yrs, -loan_type, -interest_rate) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(pop,"[^0-9.]","")),
           project_cost = as.numeric(str_replace_all(estimated_project_cost,"[^0-9.]","")),
           # Assumes estimated_project_cost should be project_cost, not funding_amount,
           # because while the projects are fundable, the IUP does not make it clear which ones will be funded,
           # only that these projects expressed that they would be replacing lead service lines with their project funding
           funding_amount = as.numeric(NA)
    ) %>%
    # process text columns
    mutate(project_name = str_squish(project_number),
           state_score = str_squish(pts),
           borrower = str_squish(facility),
           cities_served = str_squish(county),
           pwsid = str_squish(pws_id_number),
           project_description = str_squish(project_description),
           # Assumes 'BIL' in 'dac' means they are disadvantaged, see p36 of the IUP
           disadvantaged = "Yes",
           funding_status = "Funded",
           project_type = "Lead") %>%
    # select relevant states
    select(state_score, borrower, pwsid, project_name, project_description, project_cost,
           funding_amount, project_type, cities_served, population, disadvantaged, funding_status)
  
  ## Appendix B.2
  
  # (26,13)
  co_b2 <- fread("year1/CO/data/7-Colorado_AppendixB2.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (26,13)
  co_b2_clean <- co_b2 %>%
    # drop unused columns
    select(-term_yrs, -loan_type, -interest_rate) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(pop,"[^0-9.]","")),
           project_cost = as.numeric(str_replace_all(estimated_project_cost,"[^0-9.]","")),
           # Assumes estimated_project_cost should be project_cost, not funding_amount,
           # because while the projects are fundable, the IUP does not make it clear which ones will be funded,
           # only that these projects expressed that they would be replacing lead service lines with their project funding
           funding_amount = as.numeric(NA),
    ) %>%
    # process text columns
    mutate(project_name = str_squish(project_number),
           state_score = str_squish(pts),
           borrower = str_squish(facility),
           cities_served = str_squish(county),
           pwsid = str_squish(pws_id_number),
           project_description = str_squish(project_description),
           # Assumes 'BIL' in 'dac' means they are disadvantaged, see p36 of the IUP
           disadvantaged = "Yes",
           funding_status = "Funded",
           project_type = "Emerging Contaminants") %>%
    # select relevant states
    select(state_score, borrower, pwsid, project_name, project_description, project_cost,
           funding_amount, project_type, cities_served, population, disadvantaged, funding_status)
  
  
  # -> (264,15)
  co_clean <- bind_rows(co_b_clean, co_b1_clean, co_b2_clean) %>%
    mutate(state = "Colorado",
           category = "1")
  
  
  rm(list=setdiff(ls(), "co_clean"))
  
  return(co_clean)

}