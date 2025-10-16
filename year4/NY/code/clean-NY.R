clean_ny_y4 <- function() {
  base_path <- file.path("year4", "NY", "data")
  
  # NOTE - maria scraped the latest list on Oct 25th 2025. 
  
  ny_annual <- data.table::fread(file.path(base_path, "Annual.csv"),
                     colClass="character", na.strings="") |>
    janitor::clean_names()  |>
    dplyr::mutate(
      cumulative_total = as.numeric(clean_numeric_string(cumulative_total)),
      hardship = dplyr::case_when(
        cumulative_total <= 718076036 ~ "Yes", #cummulative total amount in annual list for which hardship eligibility was evaluated
        .default = "No"
      ),
      expecting_funding = dplyr::case_when(
        cumulative_total <= 1885161283 ~ "Yes", #expanded subsidized interest rate funding line
        .default = "No"
      ),
      description = stringr::str_squish(description)
    )

  # Lead PPL: Lead service line projects
  ny_lead <- data.table::fread(file.path(base_path, "LSLR.csv"),
                   colClasses="character", na.strings=c("", "NA")) |>
    janitor::clean_names() |>
    dplyr::mutate(project_type = "Lead") |>
    dplyr::select(project_number, project_type, list, codes)
  
  # EC PPL: Projects addressing emerging contaminants (e.g., PFAS)
  ny_ec <- data.table::fread(file.path(base_path, "EC.csv"),
                 colClass="character", na.strings="") %>%
    janitor::clean_names() %>%
    dplyr::mutate(project_type = "Emerging Contaminants") |>
    dplyr::select(project_number, project_type, list, codes)
  
  ny_ec_lead <- dplyr::bind_rows(ny_ec, ny_lead)
  
  ny_annual <- ny_annual |>
    dplyr::left_join(ny_ec_lead, by="project_number") |>
    dplyr::mutate(dplyr::across(dplyr::ends_with(".x"), 
                ~dplyr::case_when(
                  is.na(.) & is.na(get(sub("\\.x$", ".y", dplyr::cur_column()))) ~ NA_character_,
                  is.na(.) ~ get(sub("\\.x$", ".y", dplyr::cur_column())),
                  is.na(get(sub("\\.x$", ".y", dplyr::cur_column()))) ~ .,
                  TRUE ~ paste(., get(sub("\\.x$", ".y", dplyr::cur_column())), sep = "; ")
                ),
                .names = "{sub('\\\\.x$', '', .col)}")) %>%
  dplyr::select(-dplyr::ends_with(".x"), -dplyr::ends_with(".y"))
  
  # Process Annual List
  ny_annual <- ny_annual |>
    dplyr::mutate(
      project_type = dplyr::case_when(
        !is.na(project_type) & project_type !="General" ~ project_type,
        grepl(lead_str, description, ignore.case=TRUE)  ~ "Lead",
        grepl(ec_str, description, ignore.case=TRUE)  ~ "Emerging Contaminants",
        TRUE ~ "General")
    )
  
  # Multi-year list: Contains all eligible projects submitted for SRF assistance
  ny_multi_year <- data.table::fread(file.path(base_path, "MY.csv"),
                         colClass="character", na.strings="") |>
    janitor::clean_names() |>
    dplyr::mutate(
      description = stringr::str_squish(description)
    )
  
  # IIJA Gen Supp PPL
  ny_gs <- data.table::fread(file.path(base_path, "gs.csv"),
                 colClass="character", na.strings="") |>
    janitor::clean_names() |>
    dplyr::mutate(
      disadvantaged = "Yes"
    )
  
  ny_multi_year <- ny_multi_year |>
    # all projects not on annual, ec, or lead by project_number
    dplyr::filter(!project_number %in% ny_annual$project_number) |>
    mutate(
      # For remaining projects, check descriptions
      project_type = dplyr::case_when(
        grepl(lead_str, description, ignore.case=TRUE)  ~ "Lead",
        grepl(ec_str, description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      expecting_funding = "No",  # Not on any funding list
      cumulative_total = as.numeric(clean_numeric_string(cumulative_total))
    )

  # Step 5: Combine all projects and check distributions
  ny_all <- bind_rows(
    ny_annual,
    ny_multi_year,
  )
  
  # Step 6: Clean and standardize columns
  ny_clean <- ny_all |>
    dplyr::mutate(
      community_served = str_squish(county),
      borrower = str_squish(system_name_borrower),
      pwsid = as.character(NA),
      project_id = str_squish(project_number),
      project_name = as.character(NA),
      project_type = project_type, #already processed
      project_cost = project_cost, #drop project cost =0 downstream
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA), #SFY26 additional subsidies procided as grants, not PF
      project_description = str_squish(description),
      population = clean_numeric_string(pop),
      disadvantaged = dplyr::case_when(
        score == "H" ~ "Yes",
        project_number %in% ny_gs$project_number ~ "Yes",
        grepl("Annual", list) & hardship == "Yes" & score != "H" ~ "No", 
        grepl("LSLR", list) & grepl("DAC", codes) ~ "Yes",
        grepl("LSLR", list) & !grepl("DAC", codes) ~ "No",
        grepl("EC", list) & grepl("DAC", codes) ~ "Yes",
        grepl("EC", list) & !grepl("DAC", codes) ~ "No",
        TRUE ~ "No Information"
      ), 
      project_rank = as.character(NA),
      project_score = str_squish(score),
      project_score = dplyr::case_when(
        project_score == "H" ~ "No Information",
        .default = project_score
      ),
      expecting_funding = dplyr::case_when(
        !is.na(expecting_funding) ~ expecting_funding,
        .default = "No"
      ),
      state_fiscal_year = "2026",
      state = "New York"
    )

  # Step 7: Final column selection and validation
  ny_clean <- ny_clean |>
    select(community_served, borrower, pwsid, project_id, project_name,
           project_type, project_cost, requested_amount, funding_amount,
           principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding,
           state, state_fiscal_year)
  
  # Remove zero-cost projects per Janet's instructions in data dictionary
  ny_clean <- ny_clean |>
    filter(project_cost != "0")
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  
  ny_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: No duplicates
  
  # Check for disinfection byproduct in description
  ny_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: No change, classified as expected
    
  ####### SANITY CHECKS END #######

  run_tests(ny_clean)
  rm(list=setdiff(ls(), "ny_clean"))
  
  return(ny_clean)
}