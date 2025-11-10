clean_ny_y3 <- function() {
  base_path <- file.path("year3", "NY", "data")
  
  # NOTE - there were changes to these lists, and emmali scraped the latest 
  # list on Jan 24th 2025. All files ending with "-v2" are from the latest 
  # list, and additional guidance/notes on scraping are included in the data 
  # folder 
  
  #NY_annual --> 754 projects
  ny_annual <- fread(file.path(base_path, "ny-y3-annual-list-v2.csv"),
                     colClass="character", na.strings="") %>%
    clean_names()  %>%
    mutate(cumulative_total = convert_to_numeric(cumulative_total),
           # the expected subsidized interest rate funding line is now on 243
           expecting_funding = case_when(
             row_number() <= 243 ~ "Yes",
             # if one of the BEC/BLSLR/BGS codes are present, expecting_funding
             !is.na(codes) ~ "Yes",
             TRUE ~ NA_character_),
           cumulative_total = clean_numeric_string(cumulative_total),
           # hardship evaluation eligibility line
          hardship = dplyr::case_when(
             row_number() <= 143 ~ "Yes",
             .default = "No"),
          list = "annual"   
          ) 

  # Lead PPL: Lead service line projects
  #NY_lead --> 127 projects
  ny_lead <- fread(file.path(base_path, "ny-y3-bil-lead-v2.csv"),
                   colClasses="character", na.strings=c("", "NA")) %>%
    clean_names() %>%
    mutate(project_type = "Lead",
           list = "lead") %>%
    rename(project_number = project) %>%
    select(project_number, project_type, list, dac)

  # Lead Awards
  #NY_lead_awards --> 15 projects; all in lead ppl
  ny_lead_awards <- data.table::fread(file.path(base_path, "ffy25-bil-lslr-grant-awards.tsv"),
                   colClasses="character", na.strings=c("", "NA")) |>
    janitor::clean_names() |>
    # projects on lead awards are expecting funding, but dac is determined by PPL 'dac' column
    dplyr::mutate(
      expecting_funding = "Yes",
      funding_amount = clean_numeric_string(total_bil_lslr_funding_award),
      principal_forgiveness = clean_numeric_string(bil_lslr_grant_award_ffy24)
           ) |>
    dplyr::rename(project_number = srf_number) %>%
    dplyr::select(project_number, expecting_funding, funding_amount, principal_forgiveness)


  ny_lead_combined <- ny_lead |>
    dplyr::left_join(ny_lead_awards, by="project_number")

  # EC PPL: Projects addressing emerging contaminants (e.g., PFAS)
  #NY_ec --> 22 projects
  ny_ec <- fread(file.path(base_path, "ny-y3-bil-ec-v2.csv"),
                 colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           list = "ec"
    ) %>%
    rename(project_number = project) %>%
    select(project_number, project_type, list, dac)

  #EC Awards
  #NY_ec_awards --> 12 projects, all in ec ppl
  ny_ec_awards <- data.table::fread(file.path(base_path, "ffy25-bil-ec-grant-awards.tsv"),
                        colClass="character", na.strings="") |>
    janitor::clean_names() |>
    # projects on ec awards list are expecting funding, but dac is determined on ec ppl
    dplyr::mutate(expecting_funding = "Yes",
           principal_forgiveness = clean_numeric_string(bil_ec_grant_award_ffy25_iup),
           funding_amount = clean_numeric_string(bil_ec_grant_award_ffy25_iup)) |>
    dplyr::rename(project_number = srf_number) |>
    dplyr::select(project_number, expecting_funding, funding_amount, principal_forgiveness)
  
  ny_ec_combined <- ny_ec |>
    left_join(ny_ec_awards, by="project_number")

  
  # IIJA Gen Supp PPL
  #NY_gs --> 138 projects
  ny_gs <- fread(file.path(base_path, "ny-y3-bil-ppl-v2.csv"),
                 colClass="character", na.strings="") %>%
    clean_names() |>
    rename(project_number = project) |>
    dplyr::mutate(
      disadvantaged = "Yes",
      list = "gs"
    )
  
  #GS Awards
  #NY_gs_awards --> 19 projects, all in gs ppl
   ny_gs_awards <- data.table::fread(file.path(base_path, "ffy25-bil-gs-grant-awards.tsv"),
                          colClass="character", na.strings="") |>
    janitor::clean_names() |>
    # projects on GS award list are expecting funding
    dplyr::mutate(
           expecting_funding = "Yes",
           funding_amount = convert_to_numeric(x2025_iup_bgs_grant_award, TRUE) + convert_to_numeric(x2025_iup_bgs_0_percent_award, TRUE),
           funding_amount = clean_numeric_string(funding_amount),
           principal_forgiveness = clean_numeric_string(x2025_iup_bgs_grant_award)
    ) |>
    dplyr::rename(project_number = dwsrf_number) |>
    dplyr::select(project_number, expecting_funding, funding_amount, principal_forgiveness)
  
  
  ny_gs_combined <- ny_gs |>
    dplyr::left_join(ny_gs_awards, by="project_number") |>
    dplyr::select(project_number, disadvantaged, expecting_funding, funding_amount, principal_forgiveness, list) |>
    # drop projects that also appear on the ec list where it has more details than this list
    filter(!project_number %in% ny_ec_combined$project_number)

  #NY_combined --> 282 projects
  ny_combined <- bind_rows(ny_ec_combined, ny_lead_combined, ny_gs_combined)


  #NY_annual --> 754 projects 
  #NY_cobined --> 282 projects
  #all projects found in gs, lead, and ec ppl and award list are in annual
  ny_annual_combined <- ny_annual |>
     dplyr::left_join(ny_combined, by="project_number")
  
  
  # Process Annual List
  ny_annual <- ny_annual_combined |>
    dplyr::mutate(
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        grepl(lead_str, description, ignore.case=TRUE)  ~ "Lead",
        grepl(ec_str, description, ignore.case=TRUE)  ~ "Emerging Contaminants",
        TRUE ~ "General"),
      list = ifelse(is.na(list.y), list.x, list.y),
      expecting_funding = ifelse(is.na(expecting_funding.y), expecting_funding.x, expecting_funding.y) 
    ) |>
    dplyr::select(-list.x, -list.y, -expecting_funding.x, -expecting_funding.y)
  
  
  # Multi-year list: Contains all eligible projects submitted for SRF assistance
  #NY_multi_year --> 1075 projects
  ny_multi_year <- data.table::fread(file.path(base_path, "ny-y3-multi-year-v2.csv"),
                         colClass="character", na.strings="") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_number = clean_numeric_string(project_number) # at least 2 project numbers had "," which hamper matching
    )
  
  #NY_multi_year not in annual list --> 321
  ny_multi_year <- ny_multi_year |>
    # all projects not on annual, ec, or lead by project_number
    filter(!project_number %in% ny_annual$project_number) %>%
    mutate(
      # For remaining projects, check descriptions
      project_type = case_when(
        grepl(lead_str, description, ignore.case=TRUE)  ~ "Lead",
        grepl(ec_str, description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      list = "multi",
      expecting_funding = "No"  # Not on any funding list
    )

  # Step 5: Combine all projects and check distributions
  ny_all <- bind_rows(
    ny_annual,
    ny_multi_year,
  )
  
  # Step 6: Clean and standardize columns
  ny_clean <- ny_all |>
    dplyr::mutate(
      disadvantaged = case_when(
        score == "H" ~ "Yes",
        project_number %in% ny_gs$project ~ "Yes",
        list == "annual" & hardship == "Yes" & score != "H" ~ "No", 
        list == "lead" & dac=="Yes" ~ "Yes",
        list == "lead" & dac=="No" ~ "No",
        list == "ec" & dac=="dac" ~ "Yes",
        list == "ec" & is.na(dac) ~ "No",
        TRUE ~ "No Information"
      ),
      population = dplyr::case_when(
        is.na(population) ~ clean_numeric_string(pop),
        .default = clean_numeric_string(population)),
      project_cost = clean_numeric_string(project_cost),
      project_id = str_squish(project_number),
      community_served = str_squish(county),
      borrower = str_squish(system_name_borrower),
      project_score = str_squish(score),
      project_score = dplyr::case_when(
        project_score == "H" ~ "No Information",
        .default = project_score
      ),
      project_description = str_squish(description),
      state = "New York",
      state_fiscal_year = "2025",
      pwsid = as.character(NA),
      project_name = as.character(NA),
      requested_amount = as.character(NA),
      funding_amount = dplyr::case_when(
        list %in% c("gs", "ec", "lead") & is.na(funding_amount) ~ "No Information",
        !list %in% c("annual", "multi") ~ funding_amount,
        .default = "No Information"
      ),
      principal_forgiveness = dplyr::case_when(
        list %in% c("gs", "ec", "lead") & is.na(principal_forgiveness) ~ "No Information",
        !list %in% c("annual", "multi") ~ principal_forgiveness,
        .default = "No Information"
      ),
      project_rank = as.character(NA),
      expecting_funding = ifelse(is.na(expecting_funding), "No", expecting_funding)
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
  
  
  # Amendment 2 update (March 26, 2025)
  ny_clean <- ny_clean %>%
    mutate(project_cost = case_when(
      project_id == "18656" ~ "3000000",
      project_id == "17299" ~ "2000000",
      project_id == "18631" ~ "11000000",
      TRUE ~ project_cost
    ))
  
  # Amendment 3 update (June 11, 2025)
  ny_clean <- ny_clean %>%
    mutate(project_cost = case_when(
      project_id == "75704" ~ "4000000",
      project_id == "18214" ~ "1500000",
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