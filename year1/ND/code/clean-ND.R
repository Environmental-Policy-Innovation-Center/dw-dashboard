source("resources.R")

clean_nd_y1 <- function() {
  
  
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
           project_id = str_squish(tracking_no)) %>%
    # combine PF funding across the tables on p 10-13 where projects appear twice
    group_by(project_id) %>%
    summarize(principal_forgiveness = sum(principal_forgiveness))
  
  
  
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
    mutate(project_rank = str_squish(priority_ranking_supplemental),
           population = clean_numeric_string(present_population),
           # transform to numeric and multiply by 1000
           funding_amount_gen_1000 =  convert_to_numeric(project_cost_1_000,TRUE) * 1000,
           funding_amount_ec_1000 =  convert_to_numeric(project_cost_emerging_contaminants_1_000,TRUE) * 1000,
           funding_amount_lead_1000 =  convert_to_numeric(project_cost_lead_1_000,TRUE) * 1000,
           # add together to make total funding amount
           funding_amount = funding_amount_gen_1000 + funding_amount_ec_1000 + funding_amount_lead_1000,
           funding_amount = clean_numeric_string(funding_amount),
           funding_amount = case_when(
             funding_amount == "0" ~ "No Information",
             TRUE ~ funding_amount)
    ) %>%
    # process text columns
    mutate(project_description = str_squish(project_description),
           project_id = str_squish(tracking_no),
           expecting_funding = case_when(
             project_rank %in% funded ~ "Yes",
             TRUE ~ "No"),
           # reset state rank to character after used for funding_status
           project_rank = as.character(project_rank),
           # extract numeric portion of pwsid and append state abbreviation 
           pwsid = paste0("ND", as.character(map(strsplit(tracking_no, split = "-"), 1))),
           project_type = case_when(
             project_cost_emerging_contaminants_1_000 != "-" ~ "Emerging Contaminants",
             project_cost_lead_1_000 != "-" ~ "Lead",
             TRUE ~ "General"),
           state = "North Dakota",
           state_fiscal_year = "2023"
    ) %>%
    # rename columns
    rename(disadvantaged = disadvantaged_community,
           borrower = system_name) %>%
    left_join(nd_funding) %>%
    # replace NAs introduced by adding principal_forgiveness_amount
    mutate(principal_forgiveness = clean_numeric_string(principal_forgiveness),
           community_served = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA),
           project_score = as.character(NA)) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(nd_clean)
  rm(list=setdiff(ls(), "nd_clean"))
  
  return(nd_clean)
}