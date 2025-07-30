clean_ny_y0 <- function() {
  
 base_path <- "year0/NY/data/"
  
 # list files
  PPL_files <- stringr::str_subset(list.files(base_path), "curated")

  # read file
  priority_list <- dplyr::tibble()
  for (file_name in PPL_files){
    tmp_priority_list <- data.table::fread(paste0(base_path, file_name), colClasses = "character", na.strings = "") |> 
      janitor::clean_names() |>
      dplyr::mutate(
        source_list="Priority", ##??
        source_file = dplyr::case_when(
          stringr::str_detect(file_name, "annual") ~ "Annual",
          stringr::str_detect(file_name, "multiyear") ~ "Multiyear"
        )
      )
    priority_list<- priority_list |>
      dplyr::bind_rows(tmp_priority_list)
  }

  deduplicated_list <- priority_list |>
    dplyr::mutate(cumulative_total = as.numeric(clean_numeric_string(cumulative_total))) |>
    dplyr::mutate(
      above_hardship_eligibility = dplyr::if_else(
        source_file == "Annual" & cumulative_total <= 456970469, TRUE, FALSE
      )
    ) |>
    dplyr::mutate(
      above_expanded_subsidized_interest_rate = dplyr::if_else(
      source_file == "Annual" & cumulative_total <= 1016320370, TRUE, FALSE
      )
    ) |>
    dplyr::group_by(project_number) |>
    dplyr::slice(1) |> 
    dplyr::ungroup()



  ny_clean <-  deduplicated_list |>
    dplyr::mutate(
      community_served = stringr::str_squish(county),
      borrower = stringr::str_squish(system_name_borrower),
      pwsid = as.character(NA),
      project_id = stringr::str_squish(project_number),
      project_name = as.character(NA),
      project_type = dplyr::case_when(
        grepl(lead_str, description) ~ "Lead",
        grepl(ec_str, description) ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      project_cost = clean_numeric_string(project_cost)
    ) |>
    dplyr::filter(!as.numeric(project_cost) == 0) |>
    dplyr::mutate(
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      project_description = stringr::str_squish(description),
      population = clean_numeric_string(pop)
    ) |>
    dplyr::mutate(
      disadvantaged = dplyr::case_when(
        source_file == "Annual" & score == "H" ~ "Yes",
        source_file == "Annual" & ! score == "H" & isTRUE(above_hardship_eligibility) ~ "No",
        .default = "No Information"
      )
    ) |>
    dplyr::mutate(
      project_rank = as.character(NA),
      project_score = clean_numeric_string(score),
    ) |>
    dplyr::mutate(
      project_score = dplyr::case_when(
        project_score == "H" ~ "No Information",
        .default = project_score
      )
    ) |>
    dplyr::mutate(
      expecting_funding = dplyr::case_when(
        source_file == "Annual" & above_expanded_subsidized_interest_rate ~ "Yes",
        .default = "No"
      )
    ) |>
    dplyr::mutate(
      state = "New York",
      state_fiscal_year = "2022"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  
  ny_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: No duplicates
  
  # Check for disinfection byproduct in description
  ny_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
    
  ####### SANITY CHECKS END #######
  
  
  run_tests(ny_clean)
  rm(list=setdiff(ls(), "ny_clean"))
  
  return(ny_clean)
}