clean_il_y0 <- function() {
  # Fundable Lists -----
  ## Fundable PPL -----
  il_base_fundable <- data.table::fread("./year0/IL/data/Base Fundable List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      list = "Base Fundable"
    )
  
  il_lslr_fundable <- data.table::fread("./year0/IL/data/LSLR Fundable List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      list = "LSLR Fundable",
      project_type = "Lead"
    )
  
  # Other Lists -----
  ## Exhausted Funding List -----
  il_base_exhausted <- data.table::fread("./year0/IL/data/Base Exhausted Funding List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "No",
      list = "Base Exhausted"
    )
  
  ## Ineligible Funding List -----
  il_base_ineligible <- data.table::fread("./year0/IL/data/Base Ineligible Funding Cap List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "No",
      list = "Base Ineligible"
    )

  ## Planning Approval List -----
  il_base_planning_approval <- data.table::fread("./year0/IL/data/Base Planning Approval List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "No",
      list = "Base Planning"
    )

  ## No Planning Approval List -----
  il_base_no_planning_approval <- data.table::fread("./year0/IL/data/Base No Planning Approval List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "No",
      list = "Base No Planning"
    )

  ## Lead Planning Approval List -----
  il_lslr_planning_approval <- data.table::fread("./year0/IL/data/LSLR Planning Approval List.csv", colClasses = "character") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "No",
      list = "LSLR Planning",
      project_type = "Lead"
    )

  # Bind all lists (note: any data frame that starts with il_ will be bound) -----
  dfs <- mget(ls(pattern = "^il_"), envir = .GlobalEnv)
  dfs <- dfs[sapply(dfs, is.data.frame)]
  
  il_merge <- dplyr::bind_rows(dfs) |>
    dplyr::mutate(
      facility_no = ifelse(is.na(facility_no), "No Information", stringr::str_replace(facility_no, "^..", "IL")),
      facility_no = stringr::str_replace_all(facility_no, "!", "1"),
      facility_no = stringr::str_replace_all(facility_no, "l", "1"),
      facility_no = stringr::str_replace_all(facility_no, "S", "5"),
      facility_no = stringr::str_replace_all(facility_no, "D", "0"),
      facility_no = stringr::str_replace_all(facility_no, "O", "0"),
      facility_no = stringr::str_replace_all(facility_no, "B", "8"),
      facility_no = ifelse(
        facility_no == "No Information", 
        facility_no,
        paste0(
          stringr::str_sub(facility_no, 1,2),
          stringr::str_replace_all(stringr::str_sub(facility_no, 3, -1), "[^0-9]", "") #remove any special characters
        )    
      )
    )
  
  # Check project id length and inspect suspects
  # il_merge |> 
  #   dplyr::filter(stringr::str_count(facility_no) > 9)
  # il_merge |> 
  #   dplyr::filter(stringr::str_count(facility_no) < 9)
  
  il_clean <- il_merge |>
    # process numeric cols: 
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = str_squish(loan_applicant),
      borrower = str_to_title(borrower, locale="en"),
      pwsid = str_squish(facility_no), 
      project_id = ifelse(l17_number %in% c("TSD", "TSO", "TBD"), "No Information", l17_number),
#     unique(il_combined$l17_number[grepl("S", il_combined$l17_number)])
# [1] "552S" "TSD"  "S186" "3S12" "TSO"
      project_id = dplyr::case_when(
        project_id == "552S" ~ "5525",
        project_id == "S186" ~ "5186",
        project_id == "3S12" ~ "3512",
        .default = project_id
      ),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = as.character(NA),
      funding_amount = dplyr::case_when(
        list == "Base Fundable" ~ clean_numeric_string(projected_loan_amount),
        list == "LSLR Fundable" ~ clean_numeric_string(estimated_loan_amount),
        .default = "No Information"
      ),
      funding_amount = ifelse(funding_amount == "0", "No Information", funding_amount),
      principal_forgiveness = dplyr::case_when(
        list == "Base Fundable" & is.na(convert_to_numeric(disadvantaged_community_principal_forgiveness)) ~ "0",
        list == "Base Fundable" ~ clean_numeric_string(disadvantaged_community_principal_forgiveness),
        list == "LSLR Fundable" ~ clean_numeric_string(estimated_loan_amount),
        .default = "No Information"
      ),
      project_description = str_squish(project_description),
      project_description = str_to_sentence(project_description),
      project_type =  dplyr::case_when(
        !is.na(project_type) ~ project_type,
        grepl(lead_str, project_description, ignore.case=TRUE)  ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE)  ~ "Emerging Contaminants",
        TRUE ~ "General"),
      population = dplyr::case_when(
        list == "Base Fundable" ~ service_population,
        list == "Base Exhausted" ~ service_population,
        list == "Base Ineligible" ~ service_population,
        .default = "No Information"
      ),
      disadvantaged = dplyr::case_when(
        list %in% c("Base Fundable", "Base Exhausted", "Base Ineligible") & (!is.na(convert_to_numeric(disadvantaged_community_principal_forgiveness)) | disadvantaged_community_principal_forgiveness == "Funding Exhausted") ~ "Yes",
        list %in% c("Base Fundable", "Base Exhausted", "Base Ineligible") & grepl("N/E", disadvantaged_community_principal_forgiveness)  ~ "No",
        .default = "No Information"
      ),
      project_rank = as.character(NA),
      project_score = dplyr::case_when(
        list == "Base Fundable" ~ clean_numeric_string(loan_priority_score),
        list == "Base Exhausted" ~ clean_numeric_string(loan_priority_score),
        list == "Base Ineligible" ~ clean_numeric_string(loan_priority_score),
        list == "LSLR Fundable" ~ clean_numeric_string(loan_priority_score),
        .default = "No Information"
      ), 
      expecting_funding = ifelse(is.na(expecting_funding), "No", expecting_funding),
      state = "Illinois", 
      state_fiscal_year = "2022") 
  
  il_clean <- il_clean |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  # Check any inconsistencies between borrower and pwsid
  # inconsistent_borrower_pwsid <- il_clean |> 
  #   dplyr::group_by(borrower) |>
  #   dplyr::filter(dplyr::n_distinct(pwsid) > 1) |>
  #   dplyr::ungroup()

  # inconsistent_borrower_population <- il_clean |> 
  #   dplyr::group_by(borrower) |>
  #   dplyr::filter(dplyr::n_distinct(population) > 1) |>
  #   dplyr::ungroup()
    
  # Hone in on project id duplication
  il_clean |> dplyr::group_by(project_id) |> dplyr::tally() |> dplyr::arrange(dplyr::desc(n)) |> dplyr::filter(n>1)

  # Check project id
  il_clean |> dplyr::filter(project_id == "5085")
  # Decision: Default to Base Fundable project
  il_clean <- il_clean |>
    dplyr::group_by(project_id) |>
    dplyr::filter(!(project_id == "5085" & list =="Base Ineligible")) |>
    dplyr::ungroup()

  # Check project id
  il_clean |> dplyr::filter(project_id == "5652")
  # Decision: Keep both, they are different projects, may have been a typo

  # Check project id
  il_clean |> dplyr::filter(project_id == "5788")
  # Decision: Keep both, based on project description               
            
  ####### Decision:  
  # if between planning approval and exhausted funding, default to exhausted funding (more recent, more info)
  # if between fundable and any other list, default to fundable; if project description differ, then keep both projects
  # if from the same list, keep both (we are not certain they are not different phases of the same project)
  
  # Check for disinfection byproduct in description
  il_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string

  # Check for lead subtypes: Both
  il_clean |>
    dplyr::filter(project_type=="Lead") |>
    dplyr::mutate(
      lead_type = dplyr::case_when(
        stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
        stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
        stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
        # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
        stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
        stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::filter(lead_type == "both")

  ####### Decision: No lead projects classified as both
  
  # Check for lead subtypes: Unknown
  il_clean |>
    dplyr::filter(project_type=="Lead") |>
    dplyr::mutate(
      lead_type = dplyr::case_when(
        stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
        stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
        stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
        # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
        stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
        stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::filter(lead_type == "unknown")

  ####### Decision: No lead projects classified as unknonw
    
  ####### SANITY CHECKS END #######
  
  il_clean <- il_clean |>
    dplyr::select(-list)

  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)
}