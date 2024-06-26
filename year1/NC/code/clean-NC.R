library(tidyverse)
library(data.table)
library(janitor)

clean_nc <- function() {
  
  
  # (222, 15)
  nc_raw <- fread("year1/NC/data/nc-comprehensive-ppl.csv",
                  colClass="character", na.strings="") %>%
    clean_names()
  
  # -> (222,13)
  nc_clean <- nc_raw %>%
    # format numeric columns
    mutate(population = as.numeric(str_replace_all(service_populati_on, "[^0-9.]", "")),
           requested_amount = as.numeric(str_replace_all(total_funding_request, "[^0-9.]", "")),
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
           project_description = str_squish(project_name),
           state_score = str_replace_all(priorit_y_points, "[^0-9.]", ""),
           pwsid = case_when(
             pwsid == 'x' ~ as.character(NA),
             TRUE ~ pwsid),
           cities_served = str_squish(county),
           project_type = "General",
           funding_status = case_when(
             !is.na(funding_amount) ~ "Funded",
             TRUE ~ "Not Funded"),
           state = "North Carolina",
           category = "3",
    ) %>%
    select(cities_served, borrower, pwsid, project_name, project_type, requested_amount, funding_amount,
           principal_forgiveness_amount, population, state_score, funding_status, state, category)

  
  rm(list=setdiff(ls(), "nc_clean"))
  
  return(nc_clean)
}