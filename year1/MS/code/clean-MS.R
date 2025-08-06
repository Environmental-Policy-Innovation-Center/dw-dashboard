clean_ms_y1 <- function() {
 
  # read in data (extracted with tabula, further inspected and curated)
  priority_list<- data.table::fread("year1/MS/data/PPL_MS_FFY2022_raw_curated.csv",
                  colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::mutate(source_list="Priority")
  
  planning_list<- data.table::fread("year1/MS/data/PPlaL_MS_FFY2022_raw_curated.csv",
                  colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::mutate(source_list="Planning")  
  
  # sanity check
  colnames(priority_list) == colnames(planning_list)

  # order matters, priority list should be read in first
  combined_list<- priority_list |> 
    dplyr::bind_rows(planning_list) |>
    # drop category rows
    dplyr::filter(!grepl("Category", project)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, "#VALUE!"))) 

  ms_clean <- combined_list |>
    dplyr::mutate(
      community_served = as.character(NA), #TODO change everywhere to geographic_reference
      borrower = stringr::str_squish(project),
      pwsid = as.character(NA),
      project_id = as.character(NA),
      project_name = as.character(NA)
    ) |>
    dplyr::mutate(
      project_description = stringr::str_squish(project_description),
      project_type = dplyr::case_when(
            grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
            grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
            TRUE ~ "General"
           )
    ) |>
    dplyr::mutate(
      project_cost = as.character(NA),
      requested_amount = clean_numeric_string(loan_amount_requested),
      funding_amount = as.character(NA)
    )|>
    dplyr::mutate(
      row_id = dplyr::row_number(),
      # this might be overly complicated and it may be best to add logical in the curated source list
      principal_forgiveness = dplyr::if_else(
        row_id %in% {
          funding_lines <- which(stringr::str_detect(project, "Funding Line--"))
          unlist(lapply(funding_lines, function(x) seq_len(x - 1)))
        },
        clean_numeric_string(eligible_pf_amount),
        # checked in w Danielle about this on 2025_07_17, 
        # we use NA instead of 0, because projects below the funding line 
        # are not expecting funding and principal forgiveness would not apply 
        # to those cases.
        # Updating in 2025_08_06 considering encoding explained by Phil
        "No Information") 
      ) |>
    dplyr::mutate(
      population = clean_numeric_string(service_area_population),
      disadvantaged = ifelse(
             as.numeric(str_replace_all(eligible_pf_amount,"[^0-9.]","")) > 0, 
             "Yes", 
             "No"), 
      project_score = str_replace_all(priority_points,"[^0-9.]",""),
      # determine expecting funding based on funding line (regardless of amount)
      expecting_funding = dplyr::if_else(
        row_id %in% {
          funding_lines <- which(stringr::str_detect(project, "Funding Line--"))
          unlist(lapply(funding_lines, function(x) seq_len(x - 1)))
        },
        "Yes",
        "No") 
    )|>
    dplyr::filter(!stringr::str_detect(project, "Funding Line--")) |>
    dplyr::group_by(source_list) |>
    dplyr::mutate(project_rank = paste0(dplyr::row_number(), "-",source_list)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      state = "Mississippi",
      state_fiscal_year = "2023"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  
  ms_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: No information for project id
  
  # Check for disinfection byproduct in description
  ms_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: No disinfection byproduct string, no EC projects
    
  ####### SANITY CHECKS END #######
  
  run_tests(ms_clean)
  rm(list=setdiff(ls(), "ms_clean"))
  
  return(ms_clean)
}