all_projects <- s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") %>%
  janitor::clean_names() |>
  dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year))


lead_type_unknown <- all_projects |>
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
  dplyr::filter(lead_type == "unknown") |>
  dplyr::select(
    state,
    state_fiscal_year,
    pwsid,
    project_id,
    project_name,
    project_description,
    project_cost,
    requested_amount,
    list,
    lead_type
  ) 

googlesheets4::write_sheet(
  lead_type_unknown,
  ss = "https://docs.google.com/spreadsheets/d/1E4oxz1HU1gtVckEavCyFGoRi_ZvBPzJvZGkgdaTtgQk"
)


dup_df <- all_projects |>
      dplyr::filter(project_id != "No Information", !is.na(project_id)) |>
      dplyr::count(state, state_fiscal_year, project_id) |>
      dplyr::filter(n > 1) 

duplicate_project_list <- all_projects |>
  dplyr::filter(project_id %in% dup_df$project_id) |>
  arrange(state, state_fiscal_year, project_id) |>
  dplyr::select(
    state, state_fiscal_year, project_id, project_name, project_description, project_cost, requested_amount, project_type,list
  ) |>
  dplyr::group_by(state, state_fiscal_year, project_id) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup()


  
googlesheets4::write_sheet(
  duplicate_project_list,
  ss = "https://docs.google.com/spreadsheets/d/1I259Ob6aiIn6tMA4a5pesVOu9V3Zb5k-JRzU_7OcueQ"
)
