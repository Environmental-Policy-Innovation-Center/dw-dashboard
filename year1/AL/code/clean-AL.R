clean_al_y1 <- function() {
  
  # (7,17)
  al_base <- fread("year1/AL/data/1-Alabama_Base-PPL.csv",
                   colClasses = "character", na.strings = "") |>
    clean_names() %>%
    mutate(project_rank = as.character(row_number()),)
  
  # (19,17)
  al_bil <- fread("year1/AL/data/1-Alabama_BIL-PPL.csv",
                  colClasses = "character", na.strings = "") |>
    clean_names() %>%
    mutate(project_rank = as.character(row_number()),)
  
  # (3,17)
  al_lead <- fread("year1/AL/data/1-Alabama_Lead-PPL.csv",
                   colClasses = "character", na.strings = "") |>
    clean_names() |>
    mutate(project_type = "Lead",
           project_rank = as.character(row_number()),)
  
  # process base, bil, and lead together with uniform structure
  # -> (31,14)
  al_combined <- bind_rows(al_base, al_bil, al_lead) 
  
  al_combined <- al_combined |>
    select(-gpr_component_cost, -gpr_type, -gpr_project, -estimated_construction_start_date) |>
    # process numeric columns
    mutate(funding_amount = clean_numeric_string(assistance_amount),
           principal_forgiveness = clean_numeric_string(additional_subsidization_principal_forgiveness),
           project_cost = as.character(NA),
    ) |>
    # process text columns
    mutate(community_served = str_squish(county_served),
           borrower = str_squish(str_replace_all(applicant_name, "\\*", "")),
           project_score = str_squish(priority_point_rank),
           population = clean_numeric_string(population_served),
           disadvantaged = case_when(
             disadvantaged_criteria == "Y" ~ "Yes",
             TRUE ~ "No"),
           pwsid = str_squish(pwsid_number),
           pwsid = str_replace(pwsid, "AL000341", "AL0000341"),
           project_name = str_squish(project_name),
           project_description = str_squish(project_description),
           project_id = str_squish(project_id),
           project_type = case_when(
             !is.na(project_type) ~ project_type,
             grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           # ALL IUP projects are expected to be funded
           expecting_funding = "Yes",
           project_cost = clean_numeric_string(convert_to_numeric(assistance_amount,TRUE) + 
             convert_to_numeric(unfunded_match_portion, TRUE))
    )
  
  # handle EC IUP separate since it has its a unique structure
  # (1,10)
  al_ec <- fread("year1/AL/data/al-y1-ec-ppl-1.csv",
                 colClasses = "character", na.strings = "") |>
    clean_names() |>
    mutate(project_type = "Emerging Contaminants", 
           funding_amount = clean_numeric_string(project_amount),
           principal_forgiveness = funding_amount,
           expecting_funding = "Yes",
           community_served = str_squish(city_town),
           borrower = str_squish(applicant_name),
           pwsid = str_squish(permit_number),
           principal_forgiveness = "No Information",
           population = clean_numeric_string(population),
           project_name = "No Information",
           project_id = "No Information",
           project_description = str_squish(project_description),
           project_id = "No Information",
           project_score = str_squish(priority_ranking),
           project_rank = as.character(row_number()),
           disadvantaged = case_when(
             as.numeric(disadvantaged_ranking) > 1 ~ "Yes",
             TRUE ~ "No"),
           project_cost = "No Information") |>
    select(community_served, borrower, pwsid, project_type, project_name, project_id,
           funding_amount, principal_forgiveness, population, project_description,
           project_score, expecting_funding, disadvantaged, project_cost, project_rank)
  
  # combine and add final columns
  al_clean <- bind_rows(al_combined, al_ec) |>
  mutate(
           requested_amount = as.character(NA),
           state = "Alabama",
           state_fiscal_year = "2023"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(al_clean)
  rm(list=setdiff(ls(), "al_clean"))

  return(al_clean)
}
