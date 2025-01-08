source("resources.R")

clean_sc_y1 <- function() {
  
  
  # import EC projects, (3,12) -> (3,5)
  sc_ec <- fread("year1/SC/data/sc-fy22-iup-ec.csv",
                 colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           expecting_funding = "Yes",
           principal_forgiveness = convert_to_numeric(estimated_principal_forgiveness_assistance3, TRUE),
           funding_amount = convert_to_numeric(estimated_srf_loan_amount,TRUE) + principal_forgiveness) %>%
    select(srf_project_number, project_type, expecting_funding, principal_forgiveness, funding_amount)
  
  
  # import lead (3,12)
  sc_lead <- fread("year1/SC/data/sc-fy22-iup-lslr.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(project_type = "Lead",
           expecting_funding = "Yes",
           principal_forgiveness = convert_to_numeric(estimated_princpal_forgiveness_3_assistance, TRUE),
           estimated_srf_loan_amount = convert_to_numeric(estimated_srf_loan_amount, TRUE),
           funding_amount = estimated_srf_loan_amount + principal_forgiveness) %>%
    select(srf_project_number, project_type, expecting_funding, principal_forgiveness, funding_amount)
  
  
  # import base projects (9,11)
  sc_base <- fread("year1/SC/data/sc-fy22-iup-base.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "General",
           principal_forgiveness = convert_to_numeric(estimated_principal_forgiveness_assistance2, TRUE),
           funding_amount = convert_to_numeric(estimated_srf_loan_amount4, TRUE) + principal_forgiveness) %>%
    select(srf_project_number, project_type, expecting_funding, principal_forgiveness, funding_amount)
  
  
  # import supplemental projects (17,11)
  sc_supp <- fread("year1/SC/data/sc-fy22-iup-gen-supp.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(expecting_funding = "Yes",
           project_type = "General",
           principal_forgiveness = convert_to_numeric(estimated_principal_forgiveness_assistance2, TRUE),
           funding_amount = convert_to_numeric(estimated_srf_loan_amount, TRUE) + principal_forgiveness) %>%
    select(srf_project_number, project_type, expecting_funding, principal_forgiveness, funding_amount)
  
  
  # merge base, supp, and ec together for uniform processing other features
  sc_combined <- bind_rows(sc_base, sc_supp, sc_ec, sc_lead)
  
  
  # import comprehensive project list of funded and applicants
  sc_comp <- fread("year1/SC/data/sc-fy22-iup-comprehensive-project-list.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    left_join(sc_combined, by="srf_project_number") 
  
  
  
  sc_clean <- sc_comp %>%
    # process numeric columns
    mutate(
           population = clean_numeric_string(sponsors_service_population),
           project_cost = clean_numeric_string(estimated_total_project_cost),
           requested_amount = clean_numeric_string(requested_srf_assistance1),
           funding_amount = clean_numeric_string(funding_amount),
           funding_amount = case_when(funding_amount == "0" ~ "No Information", TRUE ~ funding_amount),
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           principal_forgiveness = case_when(principal_forgiveness == "0" ~ "No Information", TRUE ~ principal_forgiveness)
    ) %>%
    # process text columns
    mutate(project_description = str_squish(project_description),
           # take the first portion of the water system id number and attach SC
           pwsid = paste0("SC", as.character(map(strsplit(srf_project_number, split = "-"), 1))),
           borrower = str_extract(sponsor_project_name, "^[^-]+"),
           project_id = str_squish(srf_project_number),
           # manually fix borrower / project_names that didn't have dashes or multiple dashses
           borrower = case_when(
            project_id == "2620004-30" ~ "Grand Strand Water and Sewer Authority Bull Creek",
            project_id == "2620004-28" ~ "Grand Strand Water and Sewer Authority Conway",
            project_id == "3220001-05" ~ "Gilbert-Summit Rural Water District",
            project_id == "3210002-04" ~ "Batesburg-Leesville, Town of",
            project_id == "0410011-04" ~ "Belton-Honea Path Water Authority",
            project_id == "0420005-03" ~ "Starr-Iva Water & Sewer District",
            project_id == "0420005-02" ~ "Starr-Iva Water & Sewer District",
            project_id == "0420005-01" ~ "Starr-Iva Water & Sewer District",
            project_id == "0120001-03" ~ "Donalds-Due West Water & Sewer Authority",
            project_id == "0820002-12" ~ "Berkeley County Water & Sewer",
             TRUE ~ borrower),
           project_score = str_replace_all(total_points,"[^0-9.]",""),
           project_rank = str_squish(v1),
           state = "South Carolina",
           state_fiscal_year = "2023",
           expecting_funding = case_when(
             is.na(expecting_funding) ~ "No",
             TRUE ~ expecting_funding
           ),
           project_type = case_when(
             is.na(project_type) & grepl("lead", project_description, ignore.case=TRUE) ~ "Lead",
             is.na(project_type) & grepl("lsl", project_description, ignore.case=TRUE) ~ "Lead",
             is.na(project_type) & grepl("pfas", project_description, ignore.case=TRUE) ~ "Emerging Contamninants",
             is.na(project_type) & grepl("pfoa", project_description, ignore.case=TRUE) ~ "Emerging Contamninants",
             is.na(project_type) & grepl("pfos", project_description, ignore.case=TRUE) ~ "Emerging Contamninants",
             is.na(project_type) ~ "General",
             TRUE ~ project_type),
           community_served = as.character(NA),
           project_name = as.character(NA),
           disadvantaged = as.character(NA)
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  run_tests(sc_clean)
  rm(list=setdiff(ls(), "sc_clean"))
  
  return(sc_clean)
}