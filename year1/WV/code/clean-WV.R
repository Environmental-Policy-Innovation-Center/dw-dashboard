library(tidyverse)
library(data.table)
library(janitor)

clean_wv <- function() {
  
  # (163,36)
  # this manually merged updated file includes 
  wv_raw <- fread("year1/WV/data/48-West_Virginia_Merged_Updated.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (157,15)
  wv_clean <- wv_raw %>%
    ## get rid of rows that the rank is not a number (not a project)
    filter(!is.na(ranking) & county != "NA" & ranking !="Total") %>% 
    ## rowwise operations for sums
    rowwise() %>%
    ## make relevant columns numbers 
    mutate(population = as.numeric(str_replace_all(new_popu_lation,"[^0-9.]", "")),
           principal_forgiveness_amount =
             # Sum of DWTRF Principal Forgiveness from Base Grant, 
             # DWTRF Principal Forgiveness from Supplemental Grant, 
             # DWTRF Principal Forgiveness from Emerging Cont. Grant, and 
             # DWTRF Principal Forgiveness from LSLR Grant in Funding List
             sum(as.numeric(str_replace_all(dwtrf_principal_forgiveness_from_base_grant,"[^0-9.]","")),
                 as.numeric(str_replace_all(dwtrf_principal_forgiveness_from_supplemental_grant,"[^0-9.]","")),
                 as.numeric(str_replace_all(dwtrf_principal_forgiveness_from_emerging_cont_grant,"[^0-9.]","")),
                 as.numeric(str_replace_all(dwtrf_principal_forgiveness_from_lslr_grant,"[^0-9.]","")),
                 na.rm = T),
           funding_amount = 
             # Sum of all PF sources and
             # DWTRF Assistance from Base Grant, 
             # DWTRF Assistance from Supplemental Grant, 
             # DWTRF Assistance from LSLR Grant
             sum(principal_forgiveness_amount,
                 as.numeric((str_replace_all(dwtrf_assisitance_from_base_grant,"[^0-9.]",""))),
                 as.numeric((str_replace_all(dwtrf_assisitance_from_supplemental_grant,"[^0-9.]",""))),
                 as.numeric((str_replace_all(dwtrf_assisitance_from_lslr_grant,"[^0-9.]",""))),
                 na.rm=T)
    ) %>%
    ungroup() %>%
    # process numeric columns that don't require aggregating
    mutate(
      project_cost = as.numeric(str_replace_all(total_cost,"[^0-9.]","")),
      requested_amount = as.numeric(str_replace_all(dwtrf_funding_requested,"[^0-9.]",""))
    ) %>%
    ## split applicant name to borrower and project name
    ## get rid of wifta- then split at - if there is one
    mutate(project_type = case_when(grepl("lead", dwtrf_eligible_funding_type, ignore.case = T) ~
                                      "Lead",
                                    grepl("pfas", dwtrf_eligible_funding_type, ignore.case = T) ~
                                      "Emerging Contaminants",
                                    TRUE ~ "General"),
           funding_status = case_when(
             funding_amount > 0 | principal_forgiveness_amount > 0 ~ "Funded",
             TRUE ~ "Not Funded"),
           state_score = str_replace_all(points,"[^0-9.]", ""),
           state_rank = str_replace_all(ranking,"[^0-9.]", ""),
           project_description = str_squish(project_description_x),
           project_name = str_squish(project_name),
           borrower = str_squish(system),
           disadvantaged = str_squish(disadvan_taged),
           cities_served = str_squish(county),
           state = "West Virginia",
           category = "1"
    ) %>%
    ## keep relevant columns
    select(borrower, project_name, project_description, project_type,
           state_rank, state_score, funding_amount, principal_forgiveness_amount,
           project_cost, requested_amount,
           disadvantaged, population, cities_served, funding_status, state, category)
  
  rm(list=setdiff(ls(), "wv_clean"))
  
  return(wv_clean)
}