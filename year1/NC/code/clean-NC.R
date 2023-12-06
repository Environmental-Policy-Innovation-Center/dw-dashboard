library(tidyverse)
library(data.table)
library(janitor)

clean_nc <- function() {
  
  
  # (221, 15)
  nc_raw <- fread("year1/NC/data/33-NorthCarolina_PPL_Comprehensive.csv",
                  colClass="character", na.strings="") %>%
    clean_names()
  
  # -> (221,11)
  nc_clean <- nc_raw %>%
    # format numeric columns
    mutate(population = as.numeric(str_replace_all(service_populati_on, "[^0-9.]", "")),
           # pre-format columns that will add up to funding amount and PF
           base_dwsrf_loans = as.numeric(str_replace_all(base_dwsrf_loans, "[^0-9.]", "")),
           bil_suppl_dwsrf_loans = as.numeric(str_replace_all(bil_suppl_dwsrf_loans, "[^0-9.]", "")),
           bil_suppl_dwsrf_principal_forgivene_ss = as.numeric(str_replace_all(bil_suppl_dwsrf_principal_forgivene_ss, "[^0-9.]", "")),
           base_dwsrf_principal_forgivene_ss = as.numeric(str_replace_all(base_dwsrf_principal_forgivene_ss, "[^0-9.]", "")),
           # combine for funding amount and PF
           # does dwsrf loans already include PF? - does not appear so, as there are rows with only PF and DWSRF loans are 0, 
           # so they will need to be added together as well
           principal_forgiveness_amount = bil_suppl_dwsrf_principal_forgivene_ss + base_dwsrf_principal_forgivene_ss,
           funding_amount = base_dwsrf_loans + bil_suppl_dwsrf_loans + principal_forgiveness_amount
    ) %>%
    # format text columns
    mutate(borrower = str_squish(applicant_name),
           project_name = str_squish(project_name),
           state_score = str_replace_all(priorit_y_points, "[^0-9.]", ""),
           pwsid = case_when(
             pwsid == 'x' ~ as.character(NA),
             TRUE ~ pwsid),
           cities_served = str_squish(county),
           project_type = "General",
           state = "North Carolina",
           category = "",
    ) %>%
    mutate(funding_status = case_when(
      !is.na(funding_amount) ~ "Funded",
      TRUE ~ "Not Funded")) %>%
    select(borrower, pwsid, project_name, state_score, funding_amount, principal_forgiveness_amount,
           cities_served, project_type, population, state, category, funding_status)

  
  rm(list=setdiff(ls(), "nc_clean"))
  
  return(nc_clean)
}