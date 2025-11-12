clean_pa_y4 <- function() {
  
  # NOTE: because PA does not provide a project ID and the applicant 
  # name changes across lists in unpredictable ways, an epic_project_id
  # was created to match projects on the fundable list to the comprehensive 
  # list 
  
  pa_fundable <- fread("year4/PA/data/pa-fundable.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # all of these are expecting funding 
    mutate(expecting_funding = "Yes") %>%
    rename(project_id = loan_number, 
           funding_amount = total_assistance_amount) %>%
    select(-c(green:small_system, project_name, loan,
              single_audit_required:categorical_green_or_business_case_green, 
              loan)) 
  
  # comp list: note there was some light tidying of borrower column manually 
  # (i.e., adding "-" between borrower and project)
  pa_comp <- fread("year4/PA/data/pa-base.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(community_served = paste0(str_to_title(city), ", ", 
                                     str_to_title(county), " County"), 
           requested_amount = as.character(NA), 
           population = clean_numeric_string(population), 
           project_rank = clean_numeric_string(projrank), 
           project_cost = clean_numeric_string(project_cost),
           project_score = clean_numeric_string(dep_project_rating),
           pwsid = paste0("PA", trimws(pwsid)), 
           disadvantaged = "No Information", 
           project_description = paste0("Project Description: ", proj_description, 
                                        "; Problem Description: ", prob_description)) %>%
    select(-c(pv_rating:green_amount, region, dep_project_rating, projrank, 
              prob_description, proj_description, project_type, 
              mtgdate, street_address, city, county))
  
  # bring it back
  pa_clean <- merge(pa_comp, pa_fundable, all = T) %>%
    mutate(principal_forgiveness = clean_numeric_string(principal_forgiveness), 
           expecting_funding = replace_na(expecting_funding, "No"),
           project_id = clean_numeric_string(project_id), 
           funding_amount = clean_numeric_string(funding_amount), 
           project_type = case_when(grepl(lead_str, project_description, ignore.case = T) ~ "Lead", 
                                    grepl(ec_str, project_description, ignore.case = T) ~ "Emerging Contaminants", 
                                    # the presence of a project on a list should 
                                    # override the string matches above
                                    list == "lead_fundable" ~ "lead_list", 
                                    list == "ec_fundable" ~ "ec_list", 
                                    TRUE ~ "General"),
           state = "Pennsylvania",
           state_fiscal_year = "2025") %>%
    # handling the applicant --> project name and borrower columns: 
    # extract based on presence of hyphen:
    mutate(applicant = str_to_title(applicant_name),
           ## borrower is text before dash
           borrower = trimws(str_extract(applicant, "[^-]+")), 
           ## project name is text after dash
           project_name = trimws(str_extract(applicant, "(?<=-).*")), 
           # note there is one project with applicant_name == "Benton Municipal 
           # Water And Sewer Authority" and the name was copied over for 
           # project name 
           project_name = case_when(is.na(project_name) ~ borrower, 
                                    TRUE ~ project_name)) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(pa_clean)
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}