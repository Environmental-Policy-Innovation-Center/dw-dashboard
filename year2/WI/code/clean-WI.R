source("resources.R")

clean_wi_y2 <- function() {
    
    wi_ppl <- fread("year2/WI/data/wi-y2-ppl.csv",
                    colClasses = "character", na.strings = "") %>%
      clean_names() %>%
      mutate(borrower = str_to_title(str_replace_all(municipality, "[0-9.\\#]", "")),
             borrower = str_squish(borrower),
             project_description = str_squish(project_description),
             project_id = str_squish(project_number),
             project_id = str_replace(project_id, "‐", "-"),
               ) %>%
      select(borrower, project_description, project_id)
    
    # of the three projects, RIB MOUNTAIN is already included on a list, but the other two are not
    # so we drop that one and the bind rows for the other two, rather than merging onto the PPL
    # we can then manually update that project in the PPL as being funded, EC, etc.
    wi_ec_funding <- fread("year2/WI/data/wi-y2-ec-funding-list.csv",
                           colClasses = "character", na.strings = "") %>%
      clean_names() %>%
      filter(municipality != "RIB MOUNTAIN SD") %>%
      mutate(funding_amount = clean_numeric_string(as.numeric(str_replace_all(estimated_loan_amount,"[^0-9.]", "")) + 
                                                     as.numeric(str_replace_all(pf_estimate,"[^0-9.]", ""))),
             principal_forgiveness = clean_numeric_string(pf_estimate),
             population = clean_numeric_string(population),
             requested_amount = clean_numeric_string(requested_project_costs),
             project_type = "Emerging Contaminants",
             # both projects are > 29
             disadvantaged = "Yes",
             expecting_funding = "Yes",
             borrower = str_to_title(str_replace_all(municipality, "[0-9.\\#]", "")),
             borrower = str_squish(borrower),
             project_score = str_replace_all(priority_score,"[^0-9.]",""),
             project_id = str_squish(project_number),
             project_id = str_replace(project_id, "‐", "-"),
             borrower = str_to_title(str_replace_all(municipality, "[0-9.\\#]", "")),
             )
    
    wi_funding <- fread("year2/WI/data/wi-y2-final-funding-list.csv",
                      colClasses = "character", na.strings = "") %>%
      clean_names() %>%
      filter(!is.na(project_points)) %>%
      mutate(
        funding_amount = clean_numeric_string(as.numeric(str_replace_all(estimated_loan_amount,"[^0-9.]", "")) + 
                                                as.numeric(str_replace_all(total_estimated_pf,"[^0-9.]", ""))),
        principal_forgiveness = clean_numeric_string(total_estimated_pf),
        population = clean_numeric_string(population),
        requested_amount = clean_numeric_string(requested_project_costs),
        disadvantaged = case_when(
          program == "EC" & as.numeric(str_replace_all(financial_need_points,"[^0-9.]", "")) > 29 ~ "Yes",
          as.numeric(str_replace_all(total_pf_points,"[^0-9.]", "")) > 59 ~ "Yes",
          TRUE ~ "No"),
        expecting_funding = "Yes", 
        project_score = str_replace_all(priority_score,"[^0-9.]",""),
        project_id = str_squish(project_number),
        project_id = str_replace(project_id, "‐", "-"),
      ) %>%
      select(-project_description)
    
    wi_all <- wi_ppl %>%
      left_join(wi_funding, by="project_id")
    
    wi_all <- bind_rows(wi_all, wi_ec_funding)
             
    wi_clean <- wi_all %>%
      mutate(community_served = str_squish(borrower),
             project_type = case_when(
               program == "LSL" | grepl("LSL", project_description, ignore.case=TRUE) ~ "Lead",
               program == "EC" | grepl("PFAS|Emerging Contaminants", project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
               TRUE ~ "General"),
             state = "Wisconsin",
             state_fiscal_year = "2024",
             pwsid = as.character(NA),
             project_name = as.character(NA),
             project_cost = as.character(NA),
             project_rank = as.character(NA),
             disadvantaged = replace_na(disadvantaged, "No Information"),
             population = replace_na(population, "No Information"),
             requested_amount = replace_na(requested_amount, "No Information"),
             principal_forgiveness = replace_na(principal_forgiveness, "0"),
             funding_amount = replace_na(funding_amount, "0"),
             project_score = replace_na(project_score, "No Information"),
             expecting_funding = replace_na(expecting_funding, "No"),
             )  %>%
      select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
             requested_amount, funding_amount, principal_forgiveness, population, project_description,
             disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(wi_clean)
  return(wi_clean)
}