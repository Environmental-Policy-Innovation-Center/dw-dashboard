# import project-level data
all_projects <- s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") %>%
  clean_names() |>
  dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year))

all_projects |> 
  dplyr::filter(grepl("galvanized|grr", project_description, ignore.case = TRUE)) |>
  dplyr::filter(
    !project_type == "Lead"
  ) |>
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1PSnCxR4s3QHEtfkKzL2ug0v5dg0s5kSCJl9bCRwa8ng",
    sheet = "2026_09_17_galvanized_ggr_in_project_description"
  )