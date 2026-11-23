NY_subset_borrowers <- ft_data_clean_ss_complete_cases |>
  dplyr::filter(state == "New York") |>
  dplyr::select(borrower, state_fiscal_year, system_size)


We prompted Claude Sonnet 5 to:
  For the NY_subset_borrowers dataframe, can you determine how many distinct borrowers per state_fiscal_year and system_size. 
  There are instances where capitalization and whitespace, or other small changes could flag borrower as distinct. 
  Make assumptions to assign a unique borrower and provide the reasoning for it.


# collapsed_long <- readr::read_csv("resources/NY_borrower_collapsed_groups_long.csv")

googlesheets4::write_sheet(
  collapsed_long,
  ss = "https://docs.google.com/spreadsheets/d/1IB11F2GBpiUzwPNf8TYXLVvQTJlLvH3JjAifkvWy2wg",
  sheet = "NY_collapsed_borrower_unique_claude"
)


NY_subset_borrowers_all <- ft_data_clean_ss_complete_cases |>
  dplyr::filter(state == "New York") |>
  dplyr::select(borrower, state_fiscal_year, system_size, expecting_funding)

# Comprehensive
NY_subset_borrowers_all |>
  dplyr::left_join(collapsed_long, by = "borrower") |>
  dplyr::mutate(borrower_key = dplyr::coalesce(borrower_key, borrower)) |>
  dplyr::group_by(borrower_key, state_fiscal_year, system_size) |>
  dplyr::tally() |>
  dplyr::ungroup() |>
  dplyr::group_by(state_fiscal_year, system_size) |>
  dplyr::tally() |>
  tidyr::pivot_wider(
    names_from = system_size,
    values_from =  n
  ) |>
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1IB11F2GBpiUzwPNf8TYXLVvQTJlLvH3JjAifkvWy2wg",
    sheet = "NY_borrower_unique_claude_comprehensive"
  )

# Fundable
NY_subset_borrowers_all |>
  dplyr::left_join(collapsed_long, by = "borrower") |>
  dplyr::mutate(borrower_key = dplyr::coalesce(borrower_key, borrower)) |>
  dplyr::filter(expecting_funding == "Yes") |>
  dplyr::group_by(borrower_key, state_fiscal_year, system_size) |>
  dplyr::tally() |>
  dplyr::ungroup() |>
  dplyr::group_by(state_fiscal_year, system_size) |>
  dplyr::tally() |>
  tidyr::pivot_wider(
    names_from = system_size,
    values_from =  n
  ) |>
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1IB11F2GBpiUzwPNf8TYXLVvQTJlLvH3JjAifkvWy2wg",
    sheet = "NY_borrower_unique_claude_fundable"
  )

