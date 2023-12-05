library(tidyverse)
library(data.table)
library(janitor)

clean_ca <- function() {

# - TODO: Possible we might extract some Disadvantaged info from COMP that isn't currently in FUND


### FUNDABLE

# (16,12) -> (15,8)
# intended to be funded projects that are not EC
ca_fund <- fread("year1/CA/data/5-California_Fundable_PPL.csv",
                      colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  # get rid of non-project columns
  filter(!grepl("Total", project_number)) %>%
  # process numeric columns
  mutate(population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
         funding_amount = as.numeric(str_replace_all(estimated_dwsrf_loan,"[^0-9.]", "")),
         principal_forgiveness_amount = as.numeric(str_replace_all(estimated_pf_grant_amount,"[^0-9.]", "")),
         project_cost = as.numeric(str_replace_all(estimated_project_costs, "[^0-9.]", ""))
  ) %>%
  # process text columns
  # pwsid is CA + first seven characters of project number
  mutate(pwsid = paste0("CA", as.character(map(strsplit(project_number, split = "-"), 1))),
         borrower = str_squish(applicant),
         project_name = str_squish(project_number),
         project_description = str_squish(project_title_description),
         project_type = "General",
         funding_status = "Funded",
         ) %>%
  select(borrower, pwsid, project_name, project_description, funding_amount, principal_forgiveness_amount, 
         project_type, project_cost, population, funding_status)


# (11,9) -> (11,19)
# EC projects that have funding data
ca_ec <- fread("year1/CA/data/5-California_EC.csv",
                      colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  # process numeric columns
  mutate(population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
         funding_amount = as.numeric(str_replace_all(requested_funding,"[^0-9.]", "")),
         principal_forgiveness_amount = as.numeric(str_replace_all(estimated_maximum_pf_grant_amount,"[^0-9.]", "")),
         ) %>%
  # process text columns
  mutate(pwsid = paste0("CA", as.character(map(strsplit(project_number, split = "-"), 1))),
         borrower = str_squish(applicant),
         project_name = str_squish(project_number),
         project_description = str_squish(project_title_description),
         project_type = "Emerging Contaminants",
         disadvantaged = case_when(
           grepl("Not", degree_of_disadvantaged) ~ "No",
           TRUE ~ "Yes"
         ),
         funding_status = "Funded") %>%
  select(borrower, pwsid, project_name, project_description, funding_amount, principal_forgiveness_amount, 
         project_type, population, disadvantaged, funding_status)


# -> (26,10)
ca_merge <- bind_rows(ca_fund, ca_ec)


### APPLICANT

# (345, 11) -> (313, 7) when dropping non-project rows and likely to be funded projects and consolidating columns
ca_app <- fread("year1/CA/data/5-California_Comprehensive_PPL.csv",
                      colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  # drop non-project rows
  filter(!is.na(applicant) & applicant != "Not Funded" & !is.na(project_number2)) %>%
  # drop fundable projects already processed
  filter(!project_number2 %in% ca_merge$project_name) %>%
  # process numeric columns
  mutate(
    project_cost = as.numeric(str_replace_all(estimated_requested_project_costs,"[^0-9.]", "")),
    population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
  ) %>%
  # process text columns
  mutate(
    borrower = str_squish(applicant),
    project_name = str_squish(project_number2),
    project_description = str_squish(project_title_description),
    degree_of_disadvantaged = str_squish(degree_of_disadvantaged),
    disadvantaged = case_when(
      degree_of_disadvantaged == "DAC" | degree_of_disadvantaged == "DAC" | degree_of_disadvantaged == "SDAC" | degree_of_disadvantaged == "Large Disadvantaged" | degree_of_disadvantaged == "Large Severely Disadvantaged" ~ "Yes",
      degree_of_disadvantaged == "non-DAC" | degree_of_disadvantaged == "Not Disadvantaged" ~ "No",
      degree_of_disadvantaged == "Pending" ~ "No Information",
      TRUE ~ "No Information"
    ),
    project_type = case_when(
      grepl("Lead", project_title_description, ignore.case=TRUE) ~ "Lead",
      TRUE ~ "General"
    ),
    funding_status = "Not Funded",
  ) %>%
  select(borrower, project_name, project_description, project_type, project_cost, population, 
         disadvantaged, funding_status)

# (339, 11) -> # (339, 13)
ca_clean <- bind_rows(ca_merge, ca_app) %>%
  mutate(state = "California",
         category = "1")

rm(list=setdiff(ls(), "ca_clean"))


return(ca_clean)
}