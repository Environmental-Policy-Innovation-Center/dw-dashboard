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
           expecting_funding = case_when(
             row_number() <= 243 ~ "Yes",
             # if one of the BEC/BLSLR/BGS codes are present, expecting_funding
             !is.na(codes) ~ "Yes",
             TRUE ~ "No"),
           cumulative_total = clean_numeric_string(cumulative_total))

  
  # Lead PPL: Lead service line projects
  ny_lead <- fread(file.path(base_path, "ny-y3-bil-lead-v2.csv"),
                   colClasses="character", na.strings=c("", "NA")) %>%
    clean_names() %>%
    mutate(project_type = "Lead",
           list = "lead") %>%
    rename(project_number = project) %>%
    select(project_number, project_type, list, dac)

  
  # EC PPL: Projects addressing emerging contaminants (e.g., PFAS)
  ny_ec <- fread(file.path(base_path, "ny-y3-bil-ec-v2.csv"),
                 colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           list = "ec"
    ) %>%
    rename(project_number = project) %>%
    select(project_number, project_type, list, dac)
  
  ny_ec_lead <- bind_rows(ny_ec, ny_lead)
  
  ny_annual <- ny_annual %>%
    left_join(ny_ec_lead, by="project_number")
  
  
  # Process Annual List
  ny_annual <- ny_annual %>%
    mutate(
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        str_detect(tolower(description), "lead") ~ "Lead",
        str_detect(tolower(description), "pfas|dioxane|cyanotoxins|mn") ~ "Emerging Contaminants",
        codes == "BEC" ~ "Emerging Contaminants",
        codes == "BLSLR" ~ "Lead",
        TRUE ~ "General"),
      list = ifelse(is.na(list), "annual", list)
    )
  
  
  # Multi-year list: Contains all eligible projects submitted for SRF assistance
  ny_multi_year <- fread(file.path(base_path, "ny-y3-multi-year-v2.csv"),
                         colClass="character", na.strings="") %>%
    clean_names()
  
  # Only used for checking for General DAC status
  ny_gs <- fread(file.path(base_path, "ny-y3-bil-ppl-v2.csv"),
                 colClass="character", na.strings="") %>%
    clean_names()

  
  ny_multi_year <- ny_multi_year %>%
    # all projects not on annual, ec, or lead by project_number
    filter(!project_number %in% ny_annual$project_number) %>%
    mutate(
      # For remaining projects, check descriptions
      project_type = case_when(
        str_detect(tolower(description), "lead") ~ "Lead",
        str_detect(tolower(description), "pfas|dioxane|cyanotoxins|mn") ~ "Emerging Contaminants",
        TRUE ~ "General"),
      list = "multi",
      # disadvantaged = "No Information",  # Per data dictionary
      expecting_funding = "No"  # Not on any funding list
    )

  # Step 5: Combine all projects and check distributions
  ny_all <- bind_rows(
    ny_annual,
    ny_multi_year,
  )
  
  
  
  # Step 6: Clean and standardize columns
  ny_clean <- ny_all %>%
    mutate(
      disadvantaged = case_when(
        score == "H" ~ "Yes",
        list == "annual" & expecting_funding == "Yes" & score != "H" ~ "No",
        list == "lead" & dac=="Yes" ~ "Yes",
        list == "lead" & dac=="No" ~ "No",
        list == "ec" & dac=="dac" ~ "Yes",
        list == "ec" & is.na(dac) ~ "No",
        list == "annual" & expecting_funding == "No" ~ "No Information",
        list == "multi" & expecting_funding == "No" ~ "No Information",
        TRUE ~ "!missing"
      ),
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
      project_rank = as.character(NA),
    )
  
  # Amendment 1 update
  ny_clean <- ny_clean %>%
    mutate(project_cost = case_when(
      project_id == "18596" ~ "45537145",
      project_id == "17787" ~ "30000",
      project_id == "18507" ~ "75000",
      project_id == "18757" ~ "300000",
      project_id == "18584" ~ "100000",
      project_id == "18651" ~ "75000",
      project_id == "18216" ~ "175000",
      project_id == "18787" ~ "22000",
      project_id == "19215" ~ "8300000",
      project_id == "18854" ~ "2659853",
      project_id == "18548" ~ "1500000",
      TRUE ~ project_cost
    ))
  
  # Step 7: Final column selection and validation
  ny_clean <- ny_clean %>%
    select(community_served, borrower, pwsid, project_id, project_name,
           project_type, project_cost, requested_amount, funding_amount,
           principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding,
           state, state_fiscal_year)
  
  # Remove zero-cost projects per Janet's instructions in data dictionary
  ny_clean <- ny_clean %>%
    filter(project_cost != "0")
  
  
  run_tests(ny_clean)
  rm(list=setdiff(ls(), "ny_clean"))
  
  return(ny_clean)
}