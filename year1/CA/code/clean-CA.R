source("resources.R")

clean_ca <- function() {
  
  ### FUNDED LIST - Updated
  
  # (111,13) -> (111,11)
  # intended to be funded projects that are not EC
  ca_fund <- fread("year1/CA/data/california-updated-fundable-list.csv",
                        colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # get rid of non-project columns
    filter(!grepl("Total", project_number)) %>%
    # process numeric columns
    mutate(requested_amount = clean_numeric_string(estimated_project_costs),
           funding_amount = clean_numeric_string(estimated_project_costs),
           principal_forgiveness = clean_numeric_string(estimated_pf_grant_amount),
           project_cost = as.character(NA)
           ) %>%
    # process text columns
    # pwsid is CA + first seven characters of project number
    mutate(pwsid = paste0("CA", as.character(map(strsplit(project_number, split = "-"), 1))),
           borrower = str_squish(applicant),
           population = clean_numeric_string(population),
           project_id = str_squish(project_number),
           project_name = as.character(NA),
           project_description = str_squish(project_title_description),
           disadvantaged = case_when(
             is.na(degree_of_disadvantaged) ~ "No Information",
             degree_of_disadvantaged == "DAC" | degree_of_disadvantaged == "SDAC" ~ "Yes",
             TRUE ~ "No"),
           project_type = "General",
           expecting_funding = "Yes",
           ) %>%
    select(borrower, pwsid, project_id, project_name, project_description, funding_amount, requested_amount, 
           project_cost, disadvantaged, principal_forgiveness, project_type, population, expecting_funding)
  
  
  # (11,9) -> (11,9)
  # EC projects that have funding data
  ca_ec <- fread("year1/CA/data/california-ec-ppl.csv",
                        colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # process numeric columns
    mutate(
           funding_amount = clean_numeric_string(requested_funding),
           requested_amount = clean_numeric_string(requested_funding),
           principal_forgiveness = clean_numeric_string(estimated_maximum_pf_grant_amount),
           project_cost = as.character(NA)
           ) %>%
    # process text columns
    mutate(pwsid = paste0("CA", as.character(map(strsplit(project_number, split = "-"), 1))),
           borrower = str_squish(applicant),
           population = clean_numeric_string(population),
           project_id = str_squish(project_number),
           project_name = as.character(NA),
           project_description = str_squish(project_title_description),
           project_type = "Emerging Contaminants",
           disadvantaged = case_when(
             grepl("Not", degree_of_disadvantaged) ~ "No",
             TRUE ~ "Yes"
           ),
           expecting_funding = "Yes") %>%
    select(borrower, pwsid, project_name, project_id, project_description, funding_amount, principal_forgiveness, 
           project_type, population, disadvantaged, expecting_funding)
  
  
  # -> (122,11)
  ca_merge <- bind_rows(ca_fund, ca_ec)
  
  
  ### APPLICANT - Comprehensive List, Appendix B of SFY23 IUP
  
  # (345, 11) -> (294, 8) when dropping non-project rows and likely to be funded projects and consolidating columns
  ca_app <- fread("year1/CA/data/california-comprehensive-ppl.csv",
                        colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # drop non-project rows
    filter(!is.na(applicant) & applicant != "Not Funded" & !is.na(project_number2) & applicant != "Applicant") %>%
    # drop fundable projects already processed
    filter(!project_number2 %in% ca_merge$project_name) %>%
    # process numeric columns
    mutate(
      requested_amount = clean_numeric_string(estimated_requested_project_costs),
      population = clean_numeric_string(population),
    ) %>%
    # process text columns
    mutate(
      borrower = str_squish(applicant),
      pwsid = paste0("CA", as.character(map(strsplit(project_number2, split = "-"), 1))),
      # replace pwsid where above extraction is wrong
      pwsid = case_when(
        pwsid == "CA7844" ~ "CA5610040",
        TRUE ~ pwsid),
      project_id = str_squish(project_number2),
      project_name = as.character(NA),
      project_description = str_squish(project_title_description),
      degree_of_disadvantaged = str_squish(degree_of_disadvantaged),
      disadvantaged = case_when(
        degree_of_disadvantaged == "DAC" | degree_of_disadvantaged == "SDAC" | degree_of_disadvantaged == "Large Disadvantaged" | degree_of_disadvantaged == "Large Severely Disadvantaged" ~ "Yes",
        degree_of_disadvantaged == "non-DAC" | degree_of_disadvantaged == "Not Disadvantaged" ~ "No",
        degree_of_disadvantaged == "Pending" ~ "No Information",
        TRUE ~ "No Information"
      ),
      project_type = "No Information",
      expecting_funding = "No",
    ) %>%
    select(borrower, pwsid, project_name, project_id, project_type, project_description, requested_amount, population, 
           disadvantaged, expecting_funding)
  
  ### Lead PPL
  
  # (14,10)
  lead_replacement <- read.csv("year1/CA/data/california-lsl-replacement-inventory-list.csv") %>%
    clean_names() %>%
    mutate(project_cost = clean_numeric_string(estimated_cost_to_replace),
           project_description = "Lead Service Line Replacement")
  
  # (110,10)
  lead_investigation <- read.csv("year1/CA/data/california-lsl-investigation-inventory-list.csv") %>%
    clean_names() %>%
    mutate(project_cost = convert_to_numeric(estimated_costs_to_investigate, TRUE) +
             convert_to_numeric(estimated_costs_to_replace_25, TRUE),
           project_cost = clean_numeric_string(project_cost),
           project_cost = replace_na(project_cost, "No Information"),
           project_description = "Lead Service Line Investigation"
           )
  
  # -> (124, 7)
  ca_lead <- merge(lead_replacement, lead_investigation, all=TRUE) %>%
    mutate(borrower = str_squish(water_system_name),
           pwsid = str_squish(water_system_id),
           project_type = "Lead",
           population = clean_numeric_string(population),
           disadvantaged = case_when(
             degree_of_disadvantaged == "DAC" ~ "Yes",
             TRUE ~ "No"),
           expecting_funding = "No") %>%
    select(borrower, pwsid, project_type, project_description, population, disadvantaged, expecting_funding)
  
  
  ### Merge Datasets
  
  # (539, 11) -> # (539, 18)
  ca_clean <- bind_rows(ca_merge, ca_app, ca_lead) %>%
    mutate(state = "California",
           state_fiscal_year = "2023",
           funding_amount = replace_na(funding_amount, "No Information"),
           requested_amount = replace_na(requested_amount, "No Information"),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
           project_id = replace_na(project_id, "No Information"),
           community_served = as.character(NA),
           project_rank = as.character(NA),
           project_score = as.character(NA)) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(ca_clean)
  rm(list=setdiff(ls(), "ca_clean"))

  return(ca_clean)
}