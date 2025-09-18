clean_ms_y4 <- function() {

  curated_file_names <- stringr::str_subset(list.files("year4/MS/data"), "curated")
  
  # read in data (extracted with tabula, further inspected and curated)
  # there are many data files, so this read in structure may be more efficient.
  # However, the base funding file first must be read in first, because it has 
  # a "funding line" element that provides special information about it's projects.
  # Alternatively, in the data curation process, we should add a logical column to denote if it's considered for the funding line (Expecting Funding).
  # 2025-08-07 added expecting funding column

  # list files
  PPL_files <- stringr::str_subset(curated_file_names, "PPL")  
  PPlaL_files <- stringr::str_subset(curated_file_names, "PPlaL")

  # read file
  priority_list <- dplyr::tibble()
  for (file_name in PPL_files){
    tmp_priority_list <- data.table::fread(paste0("year4/MS/data/", file_name), colClasses = "character", na.strings = "") |> 
      janitor::clean_names() |>
      dplyr::rename(
        project_title = any_of("project_description"),
        loan_amount_requested = any_of("loan_amount_request")
      ) |>
      dplyr::mutate(
        source_list="Priority",
        source_file = dplyr::case_when(
          stringr::str_detect(file_name, "_LSLR") ~ "LSLR",
          stringr::str_detect(file_name, "_EC") ~ "EC",
          TRUE ~ "Base"
        )
      )
    priority_list<- priority_list |>
      dplyr::bind_rows(tmp_priority_list)
  }
  
  
  planning_list <- dplyr::tibble()
  for (file_name in PPlaL_files){
    tmp_planning_list <- data.table::fread(paste0("year4/MS/data/", file_name), colClasses = "character", na.strings = "") |> 
      janitor::clean_names() |>
      dplyr::rename(
        project_title = any_of("project_description"),
        loan_amount_requested = any_of("loan_amount_request")
      ) |>
      dplyr::mutate(
        source_list="Planning",
        source_file = dplyr::case_when(
          stringr::str_detect(file_name, "_LSLR") ~ "LSLR",
          stringr::str_detect(file_name, "_EC") ~ "EC",
          TRUE ~ "Base"
        )
      )
    
    planning_list<- planning_list |>
      dplyr::bind_rows(tmp_planning_list)
    
  }

  # sanity check
  colnames(priority_list) %in% colnames(planning_list)
  colnames(planning_list) %in% colnames(priority_list)

  # order matters, priority list should be read in first
  combined_list<- priority_list |> 
    dplyr::bind_rows(planning_list) |>
    # drop category rows
    dplyr::filter(!grepl("Category", project))

  ms_clean <- combined_list |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = stringr::str_squish(project),
      pwsid = as.character(NA),
      project_id = as.character(NA),
      project_name = as.character(NA)
    ) |>
    dplyr::mutate(
      project_description = stringr::str_squish(project_title),
      project_type = dplyr::case_when(
            stringr::str_detect(source_file, "LSLR") ~ "Lead",
            stringr::str_detect(source_file, "EC") ~ "Emerging Contaminants",
            grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
            grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
            TRUE ~ "General"
           )
    ) |>
    dplyr::mutate(
      project_cost = as.character(NA), 
      requested_amount = clean_numeric_string(loan_amount_requested),
      funding_amount = as.character(NA),
      principal_forgiveness = dplyr::case_when(
        expecting_funding == "Yes" ~ clean_numeric_string(eligible_pf_amount),
        .default = "No Information"
      )
    )|> 
    dplyr::mutate(
      population = clean_numeric_string(service_area_population),
      disadvantaged = ifelse(
             as.numeric(str_replace_all(eligible_pf_amount,"[^0-9.]","")) > 0 & source_file == "Base", 
             "Yes", 
             "No"),
      project_score = str_replace_all(priority_points,"[^0-9.]","")
    ) |>
    dplyr::mutate(  
      expecting_funding = dplyr::case_when(
        source_list == "Priority" & !is.na(expecting_funding) ~ expecting_funding,
        source_list == "Planning" ~ "No",
        .default = "No Information") #these are EC projects
      )|>
    dplyr::group_by(source_list) |>
    dplyr::mutate(project_rank = paste0(dplyr::row_number(), "-", source_list)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      state = "Mississippi",
      state_fiscal_year = "2026"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, 
           project_rank, 
           project_score, expecting_funding, state, state_fiscal_year)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  
  ms_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: No information for project id
  
  # Check for disinfection byproduct in description
  ms_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: No disinfection byproduct string
    
  ####### SANITY CHECKS END #######
  
  run_tests(ms_clean)
  rm(list=setdiff(ls(), "ms_clean"))
  
  return(ms_clean)
}