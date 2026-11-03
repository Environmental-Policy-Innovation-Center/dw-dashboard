
TX_SFY2027_projects <- s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") %>%
  clean_names() |>
  dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year)) |>
  dplyr::filter(state == "Texas") |>
  dplyr::filter(state_fiscal_year == "2027") |>
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1Lz2cEwZCIx9wore3mZngLyCV1Y63Y5kcFQmdXMjdtV4",
    sheet = "TX_SFY2027"
  )
