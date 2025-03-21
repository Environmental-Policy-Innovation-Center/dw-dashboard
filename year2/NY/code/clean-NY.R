source("resources.R")

clean_ny_y2 <- function() {
  base_path <- file.path("year2", "NY", "data")
  
  # Annual List, includes some funded projects, and some confirmed not funded projects based on funding line at 163
  ny_annual <- fread(file.path(base_path, "ny-y2-annual-list.csv"),
                     colClass="character", na.strings="") %>%
    clean_names()
  
  # Multi-year list, intended to be comprehensive, but may not be due to amendments or docs coming out at different times
  ny_multi_year <- fread(file.path(base_path, "ny-y2-multi-year.csv"),
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(list = "multi")
  
  
  ny_lead <- fread(file.path(base_path, "ny-y2-bil-lead.csv"),
                   colClasses="character", na.strings=c("", "NA")) %>%
    clean_names() %>%
    mutate(
      project_type = "Lead",
      list = "lead",
      on_annual = project_number %in% ny_annual$project_number)
  
  ny_lead_awards <- fread(file.path(base_path, "ffy24-bil-lslr-grant-awards.csv"),
                          colClasses="character", na.strings=c("", "NA")) %>%
    clean_names() %>%
    # projects on lead awards are expecting funding, but dac is determined by PPL 'dac' column
    mutate(expecting_funding = "Yes",
           funding_amount = clean_numeric_string(total_bil_lslr_funding_award),
           principal_forgiveness = clean_numeric_string(bil_lslr_grant_award)
           ) %>%
    rename(project_number = srf_number) %>%
    select(project_number, expecting_funding, funding_amount, principal_forgiveness)
  
  # all 27 lead awards are on lead ppl
  ny_lead_combined <- ny_lead %>%
    left_join(ny_lead_awards, by="project_number")
  
  # some but not all projects in Lead PPL are on annual list
  # for where both appear, keep only needed columns
  ny_lead_on_annual <- ny_lead_combined %>%
    filter(on_annual == TRUE) %>% 
    select(project_number, project_type, dac, list, expecting_funding, funding_amount, principal_forgiveness)
  # for projects only on Lead PPL, keep all columns
  
  ny_lead_other <- ny_lead_combined %>%
    filter(on_annual == FALSE) %>%
    # if they are not already Yes for expecting funding, fill for No
    # because they are not on annual list and thus are not considered for the funding line
    mutate(expecting_funding = replace_na(expecting_funding, "No"))
  
  # EC PPL: Projects addressing emerging contaminants (e.g., PFAS)
  ny_ec <- fread(file.path(base_path, "ny-y2-bil-ec.csv"),
                 colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(
      project_type = "Emerging Contaminants",
      list = "ec",
    ) %>%
    select(project_number, project_type, dac, list)
  
  ny_ec_awards <- fread(file.path(base_path, "ffy24-bil-ec-grant-awards.csv"),
                        colClass="character", na.strings="") %>%
    clean_names() %>% 
    # projects on ec awards list are expecting funding, but dac is determined on ec ppl
    mutate(expecting_funding = "Yes",
           principal_forgiveness = clean_numeric_string(bil_ec_grant_award_ffy24_iup),
           funding_amount = principal_forgiveness) %>%
    rename(project_number = srf_number) %>%
    select(project_number, expecting_funding, funding_amount, principal_forgiveness)
  
  # all 11 ec awards on ec ppl
  ny_ec_combined <- ny_ec %>%
    left_join(ny_ec_awards, by="project_number")
  
# GS PPL  
  ny_gs <- fread(file.path(base_path, "ny-y2-bil-gs-ppl.csv"),
                 colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(list="gs",
           disadvantaged = "Yes")
    
# GS Awards
    ny_gs_awards <- fread(file.path(base_path, "ffy24-bil-gs-grant-awards.csv"),
                          colClass="character", na.strings="") %>%
    clean_names() %>%
    # projects on GS award list are expecting funding
    mutate(
           expecting_funding = "Yes",
           funding_amount = convert_to_numeric(bil_gs_grant_award_ffy24_iup, TRUE) + convert_to_numeric(bil_gs_0_percent_loan_award_ffy24_iup, TRUE),
           funding_amount = clean_numeric_string(funding_amount),
           principal_forgiveness = clean_numeric_string(bil_gs_grant_award_ffy24_iup)
    ) %>%
    rename(project_number = srf_number) %>%
    select(project_number, expecting_funding, funding_amount, principal_forgiveness)
  
  # all 13 gs award projects on gs ppl
  ny_gs_combined <- ny_gs %>%
    left_join(ny_gs_awards, by="project_number") %>%
    select(project_number, disadvantaged, expecting_funding, funding_amount, principal_forgiveness, list) %>%
    # drop projects that also appear on the ec list where it has more details than this list
    filter(!project_number %in% ny_ec_combined$project_number)
  
  # combine the minimal lists of needed columns where joining on existing projects
  # where all ec and gen projects are on annual, and some lead are
  ny_combined <- bind_rows(ny_ec_combined, ny_lead_on_annual, ny_gs_combined)
  
  # for projects already on annual list, join only columns needed
  annual_projects <- ny_annual %>%
    left_join(ny_combined, by="project_number")
  
  # for projects only on lead ppl, bind all columns
  annual_projects <- bind_rows(annual_projects, ny_lead_other)
    

  # Process Annual List projects
  annual_projects <- annual_projects %>%
    mutate(
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        str_detect(tolower(description), "lead") | code=="BLSLR" ~ "Lead",
        str_detect(tolower(description), "pfas|dioxane|cyanotoxins|mn") | code=="BEC" ~ "Emerging Contaminants",
        TRUE ~ "General"),
      expecting_funding = case_when(
        # inherit expecting funding where known already
        !is.na(expecting_funding) ~ expecting_funding,
        # if not, look for code and above funding line
        code %in% c("BLSLR", "BEC", "BGS") ~ "Yes",
        row_number() <= 163 ~ "Yes",
        # if neither of those apply and expecting_funding is still NA, set to No
        TRUE ~ "No"),
      list = replace_na(list, "annual")
    )

    multiyear_projects <- ny_multi_year %>%
    filter(!project_number %in% annual_projects$project_number) %>%
    mutate(
      # For remaining projects, check descriptions
      project_type = case_when(
        str_detect(tolower(description), "lead") ~ "Lead",
        str_detect(tolower(description), "pfas|dioxane|cyanotoxins|mn") ~ "Emerging Contaminants",
        TRUE ~ "General"),
      expecting_funding = "No",  # Not on any funding list.
      list = "multi"
    )
  
  # Step 5: Combine all projects and check distributions
  ny_clean <- bind_rows(
    annual_projects,
    multiyear_projects
  )
  
  ny_clean <- ny_clean %>%
    mutate(disadvantaged = case_when(
      !is.na(disadvantaged) ~ disadvantaged,
      score == "H" ~ "Yes",
      dac == "DAC" ~ "Yes",
      list == 'lead' & is.na(dac) ~ "No",
      list == "ec" & is.na(dac) ~ "No",
      # these are the remaining projects on the annual list above the funding line, but without an H score or code
      expecting_funding == "Yes" ~ "No",
      # no other matches, if on multi list, no info
      list == "multi" ~ "No Information",
      list == "annual" & expecting_funding == "No" ~ "No Information",
      # used for testing
      TRUE ~ "!Missing"
    ))

  
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
      funding_amount = replace_na(funding_amount, "No Information"),
      principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
      pwsid = as.character(NA),
      project_name = as.character(NA),
      requested_amount = as.character(NA),
      project_rank = as.character(NA),
      #expecting_funding = replace_na(expecting_funding, "No")
    )
  
  # Step 7: Implement Amendment 1
  ny_clean <- ny_clean %>%
    mutate(
      project_cost = ifelse(project_id == "17629", "1800000", project_cost),
      project_score = ifelse(project_id == "18854" & project_score == "40", "80", project_score),
      # changing the project score makes it above fundable line
      expecting_funding = ifelse(project_id == "18854" & project_score == "80", "Yes", expecting_funding),
      # when it is expecting funding, above rules suggest that DAC status should be "No" instead of "No Information"
      disadvantaged = ifelse(project_id == "18854", "No", disadvantaged)
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
  
  
  