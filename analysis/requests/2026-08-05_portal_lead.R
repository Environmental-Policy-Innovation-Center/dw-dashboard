summary_by_approp <- raw_dwsrf_map |>
  dplyr::filter(grepl("BIL Lead", supplemental_appropriation)) |>
  dplyr::filter(current_agreement_amount > 0 ) |>
  dplyr::group_by(supplemental_appropriation) |>
  dplyr::summarise(
    `Project Counts` = dplyr::n(),
    `Total Agreement Amount` = sum(current_agreement_amount)
  )

totals_summary <- summary_by_approp |>
  dplyr::summarise(
    `Project Counts` = sum(`Project Counts`),
    `Total Agreement Amount` = sum(`Total Agreement Amount`)
  )
