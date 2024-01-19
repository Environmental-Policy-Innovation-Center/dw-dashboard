library(tidyverse)
library(data.table)
library(janitor)

clean_nv <- function() {
  
  # (172, 16)
  # Nevada Drinking Water State Revolving Fund Priority List Effective June 2022
  # https://ndep.nv.gov/uploads/water-financing-srf-drinkingwater-docs/DW_modified_FINAL_PL-Effective-June-2022.pdf
  nv_ppl <- fread("year1/NV/data/28-Nevada_PPL.csv",
                  colClass="character", na.strings="") %>%
    clean_names()
  
  # -> (169, 11) - drop is due to the removal of "Total" rows and non fundable projects
  nv_ppl <- nv_ppl %>%
    # remove non project rows
    filter(!grepl("Total", priority_number)) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(project_population, "[^0-9.]", "")),
           requested_amount = as.numeric(str_replace_all(estimated_loan_amount, "[^0-9.]", ""))) %>%
    # process text columns
    mutate(disadvantaged = case_when(
      is.na(d2) ~ "No",
      TRUE ~ "Yes"
    ),
    borrower = str_squish(entity),
    pwsid = str_squish(state_id),
    state_score = str_replace_all(revised_points, "[^0-9.]", ""),
    state_rank = str_replace_all(priority_number, "[^0-9.]", ""),
    project_description = str_squish(project_description)
    ) %>%
    select(borrower, pwsid, project_description, state_rank, state_score, requested_amount, 
           population, disadvantaged)
  
  
  # (28,15)
  ### Nevada Attachment C: DWSRF 2022 Fundable List DWSRF 2nd Amended Intended Use Plan SFY 2022
  nv_fund <- fread("year1/NV/data/28-Nevada_Fundinglist.csv",
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    # process numeric columns
    mutate(
      funding_amount = as.numeric(str_replace_all(total_assistance, "[^0-9.]", "")),
      # transform the components of PF into numeric and replace NAs for summing
      # TODO: Confirm whether disadvantaged_subsidy should also be included here
      base_disadvantaged_subsidy = as.numeric(str_replace_all(base_disadvantaged_subsidy, "[^0-9.]", "")),
      base_disadvantaged_subsidy = replace_na(base_disadvantaged_subsidy, 0),
      base_additional_subsidy = as.numeric(str_replace_all(base_additional_subsidy, "[^0-9.]", "")),
      base_additional_subsidy = replace_na(base_additional_subsidy, 0),
      disadvantaged_subsidy = as.numeric(str_replace_all(disadvantaged_subsidy, "[^0-9.]", "")),
      disadvantaged_subsidy = replace_na(disadvantaged_subsidy, 0),
      principal_forgiveness_amount = base_disadvantaged_subsidy + base_additional_subsidy + disadvantaged_subsidy
    ) %>%
    # process text columns
    mutate(
      state_rank = str_replace_all(pl_rank, "[^0-9.]", ""),
      # replace the listing of 93,94, and 95 with only 95 to attach funding_amount to a single project
      # then manually set all state ranks to the PPL list to force correct matches
      state_rank = case_when(
        state_rank == "939495" ~ "95",
        state_rank == "57" ~ "60",
        state_rank == "61" ~ "62",
        state_rank == "97" ~ "100",
        state_rank == "122" ~ "126",
        state_rank == "129" & pws_id == "NV0000226" ~ "132",
        state_rank == "163" ~ "165",
        state_rank == "169" ~ "52",
        TRUE ~ state_rank),
      funding_status = "Funded",
    ) %>%
    # keep standardized columns to match onto the PPL List
    select(state_rank, funding_amount, principal_forgiveness_amount, funding_status)
  
  
  ### Join PPL and funding list
  nv_clean <- merge(nv_ppl, nv_fund, all=TRUE, by="state_rank") %>%
    # assign variables consistent across both
    mutate(state = "Nevada",
           category = "1",
           project_type = "General",
           # manually update the two projects that are funded in project ranked 95, fill rest in as applicant
           funding_status = case_when(
             state_rank == "93" ~ "Funded",
             state_rank == "94" ~ "Funded",
             is.na(funding_status) ~ "Not Funded",
             TRUE ~ funding_status
           ),
           #manually fix one missing project description
           project_description = case_when(
             borrower == "Beatty Water & Sanitation District - DW" ~ "Water System Rehab",
             TRUE ~ project_description
           ))
  
  
  rm(list=setdiff(ls(), "nv_clean"))
  
  return(nv_clean)
}