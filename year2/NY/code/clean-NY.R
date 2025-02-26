source("resources.R")

clean_ny_y2 <- function() {
  base_path <- file.path("year2", "NY", "data")
  
  ny_annual <- fread(file.path(base_path, "ny-y2-annual-list.csv"),
                     colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(expecting_funding = case_when(
      row_number() <= 163 ~ "Yes",
      TRUE ~ "No"
    ))
  
  ny_multi_year <- fread(file.path(base_path, "ny-y2-multi-year.csv"),
                   colClass="character", na.strings="") %>%
    clean_names()

  
  ny_gs <- fread(file.path(base_path, "ny-y2-bil-gs-ppl.csv"),
                     colClass="character", na.strings="") %>%
    clean_names()
  
  
  ny_lead <- fread(file.path(base_path, "ny-y2-bil-lead.csv"),
                   colClasses="character", na.strings=c("", "NA")) %>%
    clean_names()
  
  # EC PPL: Projects addressing emerging contaminants (e.g., PFAS)
  ny_ec <- fread(file.path(base_path, "ny-y2-bil-ec.csv"),
                 colClass="character", na.strings="") %>%
    clean_names()
 
  
  # Process Lead projects first
  lead_projects <- ny_lead %>%
    mutate(
      project_type = "Lead",
      disadvantaged = case_when(
        dac == "DAC" ~ "Yes",
        TRUE ~ "No Information")
    )

  # Process EC projects second
  ec_projects <- ny_ec %>%
    mutate(
      project_type = "Emerging Contaminants",
      disadvantaged = case_when(
        dac == "DAC" ~ "Yes",
        TRUE ~ "No Information"
      )
    )

  # Process Annual List projects (excluding those already in Lead/EC)
  annual_projects <- ny_annual %>%
    # Exclude projects already in Lead or EC lists
    filter(!project_number %in% c(lead_projects$project_number, 
                                  ec_projects$project_number)) %>%
    mutate(
      project_type = case_when(
        str_detect(tolower(description), "lead") ~ "Lead",
        str_detect(tolower(description), "pfas|dioxane|cyanotoxins|mn") ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      disadvantaged = case_when(
        score == "H" | project_number %in% ny_gs$project_number ~ "Yes",
        TRUE ~ "No Information"
      ),
    )

  # Process remaining Multi-year projects
  processed_numbers <- c(
    lead_projects$project_number,
    ec_projects$project_number,
    annual_projects$project_number
  )
  
  multiyear_projects <- ny_multi_year %>%
    filter(!project_number %in% processed_numbers) %>%
    mutate(
      # For remaining projects, check descriptions
      project_type = case_when(
        str_detect(tolower(description), "lead") ~ "Lead",
        str_detect(tolower(description), "pfas|dioxane|cyanotoxins|mn") ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      disadvantaged = "No Information",  # Per data dictionary
      expecting_funding = "No"  # Not on any funding list
    )
  
  # Step 5: Combine all projects and check distributions
  ny_clean <- bind_rows(
    lead_projects %>% select(-dac),
    ec_projects %>% select(-dac, -pfas),
    annual_projects,
    multiyear_projects
  )
  
  ## Add in funding amount and principal forgiveness columns
  ny_gs_grants <- fread(file.path(base_path, "ffy24-bil-gs-grant-awards.csv"),
                        colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(disadvantaged = "Yes",
           expecting_funding = "Yes",
           project_number = srf_number) %>%
    select(project_number, disadvantaged, expecting_funding)

  ny_lslr_grants <- fread(file.path(base_path, "ffy24-bil-lslr-grant-awards.csv"),
                          colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_number = srf_number,
           funding_amount = clean_numeric_string(total_bil_lslr_funding_award),
           principal_forgiveness = clean_numeric_string(bil_lslr_grant_award),
           disadvantaged = "Yes",
           expecting_funding = "Yes") %>%
    select(project_number, funding_amount, principal_forgiveness, disadvantaged, expecting_funding)
  
  ny_ec_grants <- fread(file.path(base_path, "ffy24-bil-ec-grant-awards.csv"),
                        colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_number = srf_number,
           funding_amount = clean_numeric_string(bil_ec_grant_award_ffy24_iup),
           principal_forgiveness = clean_numeric_string(bil_ec_grant_award_ffy24_iup),
           disadvantaged = "Yes",
           expecting_funding = "Yes") %>%
    select(project_number, funding_amount, principal_forgiveness, disadvantaged, expecting_funding)
    
  
  ny_funding <- bind_rows(ny_ec_grants, ny_lslr_grants, ny_gs_grants)
  
  ny_clean <- ny_clean %>%
    left_join(ny_funding, by="project_number") %>%
    mutate(disadvantaged = case_when(
            !is.na(disadvantaged.y) ~ disadvantaged.y,
            TRUE ~ disadvantaged.x),
           expecting_funding = case_when(
            !is.na(expecting_funding.y) ~ expecting_funding.y,
            TRUE ~ expecting_funding.x)
      ) %>%
    select(-disadvantaged.x, -disadvantaged.y, -expecting_funding.x, -expecting_funding.y)

  
  # Step 6: Clean and standardize columns
  ny_clean <- ny_clean %>%
    mutate(
      population = clean_numeric_string(pop),
      project_cost = clean_numeric_string(project_cost),
      project_id = str_squish(project_number),
      community_served = str_squish(county),
      borrower = paste(str_squish(system_name), "/", str_squish(borrower)),
      project_score = str_squish(score),
      project_description = str_squish(description),
      state = "New York",
      state_fiscal_year = "2024",
      pwsid = as.character(NA),
      project_name = as.character(NA),
      requested_amount = as.character(NA),
      project_rank = as.character(NA),
      funding_amount = replace_na(funding_amount, "0"),
      principal_forgiveness = replace_na(principal_forgiveness, "0"),
      disadvantaged = replace_na(disadvantaged, "No Information"),
      expecting_funding = replace_na(expecting_funding, "No")
    )
  
  # Step 7: Implement Amendment 1
  ny_clean <- ny_clean %>%
    mutate(
      project_cost = ifelse(project_id == "17629", "1800000", project_cost),
      project_score = ifelse(project_id == "18854" & project_score == "40", "80", project_score)
    )
  
  # Step 8: Final column selection and validation
  ny_clean <- ny_clean %>%
    select(community_served, borrower, pwsid, project_id, project_name,
           project_type, project_cost, requested_amount, funding_amount,
           principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding,
           state, state_fiscal_year)
  

  # # Remove zero-cost projects
  ny_clean <- ny_clean %>%
    filter(project_cost != "0" & project_cost != "0.0" &
             project_cost != "" & !is.na(project_cost))

  run_tests(ny_clean)
  rm(list=setdiff(ls(), "ny_clean"))
  
  return(ny_clean)
}
  
  
  