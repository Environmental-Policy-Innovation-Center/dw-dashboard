source("resources.R")

clean_ny_y3 <- function() {
  base_path <- file.path("year3", "NY", "data")
  
  # NOTE - there were changes to these lists, and emmali scraped the latest 
  # list on Jan 24th 2025. All files ending with "-v2" are from the latest 
  # list, and additional guidance/notes on scraping are included in the data 
  # folder 
  
  ny_annual <- fread(file.path(base_path, "ny-y3-annual-list-v2.csv"),
                     colClass="character", na.strings="") %>%
    clean_names()  %>%
    mutate(cumulative_total = convert_to_numeric(cumulative_total),
           # the expected subsidized interest rate funding line is now on 243
           expecting_funding = ifelse(row_number() <= 243, "Yes", "No"),
           cumulative_total = clean_numeric_string(cumulative_total))
  
  # Multi-year list: Contains all eligible projects submitted for SRF assistance
  ny_multi_year <- fread(file.path(base_path, "ny-y3-multi-year-v2.csv"),
                   colClass="character", na.strings="") %>%
    clean_names()
  
  # Annual List/BIL PPL: Projects with reports
  ny_gs <- fread(file.path(base_path, "ny-y3-bil-ppl-v2.csv"),
                     colClass="character", na.strings="") %>%
    clean_names()
  
  # Lead PPL: Lead service line projects
  ny_lead <- fread(file.path(base_path, "ny-y3-bil-lead-v2.csv"),
                   colClasses="character", na.strings=c("", "NA")) %>%
    clean_names()

  
  # EC PPL: Projects addressing emerging contaminants (e.g., PFAS)
  ny_ec <- fread(file.path(base_path, "ny-y3-bil-ec-v2.csv"),
                 colClass="character", na.strings="") %>%
    clean_names()
  
  
  # Process Lead projects first
  lead_projects <- ny_lead %>%
    mutate(
      project_type = "Lead",
      disadvantaged = case_when(
        dac == "DAC" ~ "Yes",
        TRUE ~ "No Information"
      ),
      expecting_funding = "Yes"  
    )
  
  # Process EC projects second
  ec_projects <- ny_ec %>%
    mutate(
      project_type = "Emerging Contaminants",
      disadvantaged = case_when(
        dac == "dac" ~ "Yes",
        TRUE ~ "No Information"
      ),
      expecting_funding =  "No" 
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
    ec_projects %>% select(-dac),
    annual_projects,
    multiyear_projects
  )
  
  
  # Step 6: Clean and standardize columns
  ny_clean <- ny_clean %>%
    mutate(
      population = clean_numeric_string(pop),
      project_cost = clean_numeric_string(project_cost),
      project_id = str_squish(project_number),
      community_served = str_squish(county),
      borrower = str_squish(system_name_borrower),
      project_score = str_squish(score),
      project_description = str_squish(description),
      state = "New York",
      state_fiscal_year = "2025",
      pwsid = as.character(NA),
      project_name = as.character(NA),
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      project_rank = as.character(NA)
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
  
  # Remove zero-cost projects
  ny_clean <- ny_clean %>%
    filter(project_cost != "0" & project_cost != "0.0" &
             project_cost != "" & !is.na(project_cost))
  
  
  run_tests(ny_clean)
  rm(list=setdiff(ls(), "ny_clean"))
  
  return(ny_clean)
}