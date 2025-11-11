clean_pa_y0 <- function() {
  
  base_path <- "year0/PA/data"
  
  # NOTE - because these tables are organized a bit differently, they were 
  # scraped using notebookLM and the output was checked against the PDF.
  
  # fundable list - (4, 11)
  pa_fund <- fread(file.path(base_path, "pa-y0-fund-ppl.csv")) %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes", 
           # prepping to merge to the comp list by project_name: 
           project_name = trimws(str_to_title(project_name)), 
           project_name = case_when(project_name == "Mahoning Township Municipal Authority" ~ "Mahoning Township Municipal Auth.", 
                                    TRUE ~ project_name)) %>%
    rename(funding_amount = total_assistance_amount) %>%
    select(loan_number, project_name, funding_amount, expecting_funding, 
           principal_forgiveness)
  
  # comprehensive list (25, 20)
  pa_comp <- fread(file.path(base_path, "pa-y0-comp-ppl.csv")) %>%
    clean_names() %>%
    mutate(community_served = paste0(str_to_title(city), ", ",
                                     str_to_title(county), " County"), 
           applicant_name = str_to_title(applicant_name), 
           borrower = trimws(str_extract(applicant_name, "[^-]+")), 
           project_name = trimws(str_extract(applicant_name, "(?<=-).*"))) %>%
    # bring this to the front
    relocate(borrower, project_name) %>%
    # spot checking some of the entries that didn't nicely split
    mutate(borrower = case_when(borrower == "Mahoning Township Municipal Auth. Water System Improvement" ~ "Mahoning Township Municipal Auth.", 
                                borrower == "City Of Philadelphia Torresdale Filtered Water Ps Rehab." ~ "City Of Philadelphia", 
                                borrower == "City Of Lancaster Water Infrastructure Improvements" ~ "City Of Lancaster", 
                                borrower == "West View Water Authority Lead Service Line Repl. Wifta" ~ "West View Water Authority", 
                                borrower == "Schuylkill Haven Service Line Replacement Project" ~ "Schuylkill Haven", 
                                borrower == "City Of Bethlehem Lead Line Replacement" ~ "City Of Bethlehem",
                                borrower == "Borough Of Chambersburg Lead Gooseneck Replacement" ~ "Borough Of Chambersburg", 
                                TRUE ~ borrower), 
           project_name = case_when(borrower %in% c("Mahoning Township Municipal Auth.", 
                                                    "City Of Lancaster") ~ "Water System Improvement", 
                                    borrower == "City Of Philadelphia" ~ "Filtered Water Ps Rehab.",
                                    borrower == "West View Water Authority" ~ "Lead Service Line Repl. Wifta", 
                                    borrower == "Schuylkill Haven" ~ "Service Line Replacement Project", 
                                    borrower == "City Of Bethlehem" ~ "Lead Line Replacement", 
                                    borrower == "Borough Of Chambersburg" ~ "Lead Gooseneck Replacement", 
                                    TRUE ~ project_name),
           pwsid = paste0("PA", pwsid), 
           # cleaning numeric string
           project_rank = clean_numeric_string(project_rank), 
           project_score = clean_numeric_string(dep_project_ranking), 
           population = clean_numeric_string(population), 
           project_cost = clean_numeric_string(project_cost)) %>%
    # trimming extra cols before the merge
    select(-c(city, county, street_address, mtgdate, region, green_project, 
              business_case, green_category, green_amount, pv_rating, 
              dep_project_ranking, funding_source))
  
  
  # bringing the final bits together:
  pa_clean <- merge(pa_comp, pa_fund, by.x = "borrower", 
                    by.y = "project_name", all = T) %>%
    mutate(project_id = clean_numeric_string(loan_number), 
           funding_amount = clean_numeric_string(funding_amount), 
           principal_forgiveness = clean_numeric_string(principal_forgiveness), 
           expecting_funding = case_when(is.na(expecting_funding) ~ "No", 
                                         TRUE ~ "Yes"), 
           # only using the project and problem description because all
           # projects on the fundable project list just contain the 
           # borrower name
           project_description = paste0("Project Description: ", project_description, 
                                        "; Problem Description: ", problem_description),
           project_type = case_when(grepl(ec_str, project_description) ~ "Emerging Contaminants", 
                                    grepl(lead_str, project_description) ~ "Lead",
                                    TRUE ~ "General"), 
           disadvantaged = as.character(NA), 
           requested_amount = as.character(NA), 
           state = "Pennsylvania",
           state_fiscal_year = "2022") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  

  run_tests(pa_clean)
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}