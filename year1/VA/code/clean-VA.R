source("resources.R")



clean_va_y1 <- function() {
  
  ### Base, BIL Supplemental, Emerging Contaminants
  
  # (10,15)
  va_base <- fread("year1/VA/data/va-fy22-ppl-base.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "General") %>%
    select(-notes)
  
  # (9,14)
  va_bil <- fread("year1/VA/data/va-fy22-ppl-bil.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "General")
  
  # (3,14)
  va_ec <- fread("year1/VA/data/va-fy22-ppl-ec.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants")
  
  # (22,15) 
  va_base_bil <- bind_rows(va_base, va_bil, va_ec) %>%
    mutate(disadvantaged = "No Information")
  
  
  ### Lead
  
  # (22,14) -> (22,10)
  va_lsl_22 <- fread("year1/VA/data/va-fy22-lsl-may22.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(priority, project_number, owner_information, project_name, project_description,
           total_points, principal_forgiveness, project_cost, srf_amount_for_this_iup)
  
  # (39,16) -> (39, 8)
  va_lsl_23 <- fread("year1/VA/data/va-fy22-lsl-may23.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    select(priority, project_number, owner_information, project_name,
           total_score, principal_forgiveness, project_cost, srf_amount_for_this_iup) %>%
    rename(total_points = total_score)
  
  # (61,10)
  va_lsl <- bind_rows(va_lsl_22, va_lsl_23) %>%
    mutate(project_type = "Lead") %>%
    rename(point_total = total_points)
  
  
  ### Combine & Clean
  
  # (83,13)
  va_clean <- bind_rows(va_base_bil, va_lsl)  %>%
    
    mutate(project_cost = clean_numeric_string(project_cost),
           funding_amount = clean_numeric_string(srf_amount_for_this_iup),
           principal_forgiveness = clean_numeric_string(principal_forgiveness)) %>%
    
    mutate(community_served = str_squish(city_county),
           borrower = str_squish(owner_information),
           project_id = str_squish(project_number),
           project_description = str_squish(project_description),
           project_rank = str_squish(priority),
           project_score = str_squish(point_total),
           expecting_funding = "Yes",
           state = "Virginia",
           state_fiscal_year = "2023",
           pwsid = as.character(NA),
           requested_amount = as.character(NA),
           population = as.character(NA),
           community_served = replace_na(community_served, "No Information"),
           borrower = replace_na(borrower, "No Information"),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
           project_description = replace_na(project_description, "No Information"),
           disadvantaged = replace_na(disadvantaged, "No Information")
           
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  run_tests(va_clean)
  rm(list=setdiff(ls(), "va_clean"))
  
  return(va_clean)
}