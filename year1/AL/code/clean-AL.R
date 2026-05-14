clean_al_y1 <- function() {
  
  # (7,17)
  al_base <- data.table::fread("year1/AL/data/1-Alabama_Base-PPL.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(project_rank = as.character(NA)) |>
    dplyr::mutate(list = "SFY23 Base IUP")
  
  # (19,17)
  al_bil <- data.table::fread("year1/AL/data/1-Alabama_BIL-PPL.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(project_rank = as.character(NA)) |>
    dplyr::mutate(list = "SFY23 Supplemental IUP")
  
  # (3,17)
  al_lead <- data.table::fread("year1/AL/data/1-Alabama_Lead-PPL.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(project_type = "Lead",
           project_rank = as.character(NA)) |>
    dplyr::mutate(list = "SFY23 LSLR IUP")
  
  # process base, bil, and lead together with uniform structure
  # -> (31,14)
  al_combined <- dplyr::bind_rows(al_base, al_bil, al_lead) 
  
  al_combined <- al_combined |>
    select(-gpr_component_cost, -gpr_type, -gpr_project, -estimated_construction_start_date) |>
    # process numeric columns
    mutate(funding_amount = clean_numeric_string(assistance_amount),
           principal_forgiveness = clean_numeric_string(additional_subsidization_principal_forgiveness)
    ) |>
    # process text columns
    mutate(community_served = str_squish(county_served),
           borrower = str_squish(str_replace_all(applicant_name, "\\*", "")),
           project_score = str_squish(priority_point_rank),
           population = clean_numeric_string(population_served),
           disadvantaged = case_when(
             disadvantaged_criteria %in% c("Y", "y") ~ "Yes",
             TRUE ~ "No"),
           pwsid = str_squish(pwsid_number),
           pwsid = str_replace(pwsid, "AL000341", "AL0000341"),
           project_name = str_squish(project_name),
           project_description = str_squish(project_description),
           project_id = str_squish(project_id),
           project_type = case_when(
             !is.na(project_type) ~ project_type,
             grepl("lsl|lead", project_description, ignore.case=TRUE) ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           # ALL IUP projects are expected to be funded
           expecting_funding = "Yes",
           project_cost = clean_numeric_string(convert_to_numeric(assistance_amount,TRUE) + 
             convert_to_numeric(unfunded_match_portion, TRUE))
    )
  
  # handle EC IUP separate since it has its a unique structure
  # (1,10)
  al_ec <- data.table::fread("year1/AL/data/al-y1-ec-ppl-1.csv",
                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY23 EC PPL") |>
    dplyr::mutate(project_type = "Emerging Contaminants", 
           funding_amount = clean_numeric_string(project_amount),
           principal_forgiveness = clean_numeric_string(project_amount),
           expecting_funding = "Yes",
           community_served = str_squish(city_town),
           borrower = str_squish(applicant_name),
           pwsid = str_squish(permit_number),
           principal_forgiveness = clean_numeric_string(project_amount),
           population = clean_numeric_string(population),
           project_name = str_squish(project_description),
           project_id = "No Information",
           project_description = str_squish(project_description),
           project_score = str_squish(priority_ranking),
           project_rank = as.character(NA),
           disadvantaged = case_when(
             as.numeric(disadvantaged_ranking) > 1 ~ "Yes",
             TRUE ~ "No"),
           project_cost = "No Information") |>
    dplyr::select(community_served, borrower, pwsid, project_type, project_name, project_id,
           funding_amount, principal_forgiveness, population, project_description,
           project_score, expecting_funding, disadvantaged, project_cost, project_rank, list)
  
  # combine and add final columns
  al_clean <- dplyr::bind_rows(al_combined, al_ec) |>
  dplyr::mutate(
           requested_amount = as.character(NA),
           state = "Alabama",
           state_fiscal_year = "2023"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year, list)
  
####### SANITY CHECKS START #######
# Check pwsid length
# al_clean[stringr::str_count(al_clean$pwsid) != 9, pwsid]

# Hone in on project id duplication
# al_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

# al_clean |> dplyr::filter(project_id == "FS010096-08") 
####### Decision: sum amounts - keep one project

# al_clean |> dplyr::filter(project_id == "FS010096-09")
####### Decision: sum amounts - keep one project
  
#[keep]  this group by strategy produces a warning for No Information project id (only applicable to the single project in the "SFY23 EC PPL") confirmed there are no unexpected downstream effects for this project on the al_clean dataframe
al_clean <- al_clean |>
  dplyr::group_by(project_id) |>
  dplyr::mutate(
    project_cost = clean_numeric_string(sum(as.numeric(project_cost))),
    funding_amount = clean_numeric_string(sum(as.numeric(funding_amount)))
  ) |>
  dplyr::slice(1) |> #keep first row of the group
  dplyr::ungroup()

# Check for disinfection byproduct in description
# al_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision: True EC Project, no change
  
  # Check for lead subtypes: Both
  # al_clean |>
  #   dplyr::filter(project_type=="Lead") |>
  #   dplyr::mutate(
  #     lead_type = dplyr::case_when(
  #       stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
  #       stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
  #       stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
  #       # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
  #       stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
  #       stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
  #       TRUE ~ "unknown"
  #     )
  #   ) |>
  #   dplyr::filter(lead_type == "both")

  ####### Decision: No lead projects classified as both
  
  # Check for lead subtypes: Unknown
  # al_clean |>
  #   dplyr::filter(project_type=="Lead") |>
  #   dplyr::mutate(
  #     lead_type = dplyr::case_when(
  #       stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
  #       stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
  #       stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
  #       # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
  #       stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
  #       stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
  #       TRUE ~ "unknown"
  #     )
  #   ) |>
  #   dplyr::filter(lead_type == "unknown") 

  ### 1 unknown --> LSLI

  al_clean <- al_clean |>
    dplyr::mutate(
      project_description = ifelse(
        project_id == "FS010096-07", 
        paste0(project_description, " | FT: LSLI"),
        project_description
      )
    )
  
  ####### SANITY CHECKS END #######

  run_tests(al_clean)
  rm(list=setdiff(ls(), "al_clean"))

  return(al_clean)
}


