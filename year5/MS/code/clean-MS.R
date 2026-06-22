clean_ms_y5 <- function() {
  
  base_funding_list <- data.table::fread("year5/MS/data/ms-sfy27-base-gen-supp-fundable-list.csv",
                                         colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      requested_amount = clean_numeric_string(loan_funds_requested),
      project_description = stringr::str_squish(project_title),
      list = "base fundable",
      disadvantaged = ifelse(dc == "Y", "Yes", "No")
    )
    
    
  base_planning_list <- data.table::fread("year5/MS/data/ms-sfy27-base-gen-supp-planning-list.csv",
                                          colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "No",
      requested_amount = clean_numeric_string(loan_funds),
      project_description = stringr::str_squish(project_name),
      disadvantaged = "No Information",
      list = 'base planning'
    )
  
  ms_merge <- bind_rows(base_funding_list, base_planning_list)
  
  ms_clean <- ms_merge |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = stringr::str_split_i(project, "-", 2),
      borrower = stringr::str_squish(borrower),
      pwsid = as.character(NA),
      project_id = stringr::str_split_i(project, "-", 1),
      project_id = stringr::str_squish(project_id),
      project_name = as.character(NA),
      project_type = case_when(
        grepl("lsl|lead", project_title, ignore.case = T) ~ "Lead",
        grepl(ec_str, project_title, ignore.case = T) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      project_description = replace_na(project_description, "No Information"),
      population = as.character(NA),
      project_rank = as.character(NA),
      project_score = clean_numeric_string(priority_points),
      state = "Mississippi",
      state_fiscal_year = "2027"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  
  # ms_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: 
  # project id 0180011 and 0230001 are kept as different projects
  # 0490006 and 0540013 are treated as same project, drop from planning list
  
  ms_clean <- ms_clean |>
    dplyr::filter(!(project_id == "0540013" & list == "base planning")) |>
    dplyr::filter(!(project_id == "0490006" & list == "base planning"))
    
  
  # Check for disinfection byproduct in description
  # ms_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: No disinfection byproduct string
  
  ####### SANITY CHECKS END #######
  
  run_tests(ms_clean)
  rm(list=setdiff(ls(), "ms_clean"))
  
  return(ms_clean)
}