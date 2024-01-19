library(tidyverse)
library(data.table)
library(janitor)

clean_nd <- function() {
  
  
  ## Read in tables for PF funding from the tables in page 10-13 of PPL
  
  # (4,6) -> (4,7)
  nd_gen <- fread("year1/ND/data/34-NorthDakota_GeneralSupplementalFunding.csv",
                  colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "General")
  
  # (2,6) -> (2,7)
  nd_ec <- fread("year1/ND/data/34-NorthDakota_EmergingContaminantsFunding.csv",
                 colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants")
  
  # (45,6) -> (45,7)
  nd_lead <- fread("year1/ND/data/34-NorthDakota_LeadFunding.csv",
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "Lead")
  
  
  ## COMBINE GEN, EC, AND LEAD FOR PF INFO
  # -> (51, 8)
  nd_funding <- bind_rows(nd_gen, nd_ec, nd_lead) %>%
    mutate(principal_forgiveness = as.numeric(str_replace_all(additional_subsidy,"[^0-9.]","")),
           project_name = str_squish(tracking_no)) %>%
    # combine PF funding across the tables on p 10-13 where projects appear twice
    group_by(project_name) %>%
    summarize(principal_forgiveness_amount = sum(principal_forgiveness))
  
  
  
  ## APPENDIX B 
  # (262,14) -> (262,11)
  nd_ppl <- fread("year1/ND/data/34-NorthDakota_PPL.csv",
                  colClass="character", na.strings="") %>%
    clean_names() %>%
    # drop columns not needed for analysis
    select(-construction_start_date, -est_loan_term, -v14)
  
  
  #funded projects by rank determined by highlighting in table
  funded <- c(1,4,5,6,8,9,10,12,13,22,25,35,38,39,40,50,60,75,76,78,88,93,108,109,110,111,122,124,125,126,127,
              148,151,152,156,166,169,178,199,200,201,202,215,216,217,218,219,220,221,245,250)
  
  # (262,12)
  nd_clean <- nd_ppl %>%
    # transform numeric columns
    # keep state rank numeric for filtering funded / not funded temporarily
    mutate(state_rank = as.numeric(priority_ranking_supplemental),
           population = as.numeric(str_replace_all(present_population,"[^0-9.]","")),
           # transform to numeric and multiply by 1000
           funding_amount_gen_1000 =  as.numeric(str_replace_all(project_cost_1_000,"[^0-9.]","")) * 1000,
           funding_amount_ec_1000 =  as.numeric(str_replace_all(project_cost_emerging_contaminants_1_000,"[^0-9.]","")) * 1000,
           funding_amount_lead_1000 =  as.numeric(str_replace_all(project_cost_lead_1_000,"[^0-9.]","")) * 1000,
           # replace NAs with 0 for adding together
           funding_amount_gen_1000 =   replace_na(funding_amount_gen_1000, 0),
           funding_amount_ec_1000 =   replace_na(funding_amount_ec_1000, 0),
           funding_amount_lead_1000 = replace_na(funding_amount_lead_1000, 0),
           # add together to make total funding amount
           funding_amount = funding_amount_gen_1000 + funding_amount_ec_1000 + funding_amount_lead_1000
    ) %>%
    # process text columns
    mutate(project_description = str_squish(project_description),
           project_name = str_squish(tracking_no),
           funding_status = case_when(
             state_rank %in% funded ~ "Funded",
             TRUE ~ "Not Funded"),
           # reset state rank to character after used for funding_status
           state_rank = as.character(state_rank),
           # extract numeric portion of pwsid and append state abbreviation 
           pwsid = paste0("ND", as.character(map(strsplit(tracking_no, split = "-"), 1))),
           project_type = case_when(
             project_cost_emerging_contaminants_1_000 != "-" ~ "Emerging Contaminants",
             project_cost_lead_1_000 != "-" ~ "Lead",
             TRUE ~ "General"),
           state = "North Dakota",
           category = "1"
    ) %>%
    # rename columns
    rename(disadvantaged = disadvantaged_community,
           borrower = system_name) %>%
    left_join(nd_funding) %>%
    # replace NAs introduced by adding principal_forgiveness_amount
    mutate(principal_forgiveness_amount = replace_na(principal_forgiveness_amount, 0)) %>%
    # keep columns for analysis
    select(borrower, project_name, pwsid, project_type, project_description,
           state_rank, funding_amount, principal_forgiveness_amount, disadvantaged, population, funding_status, state, category)
  
  
  rm(list=setdiff(ls(), "nd_clean"))
  
  return(nd_clean)
}