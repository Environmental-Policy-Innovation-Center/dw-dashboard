
f_c <- readxl::read_excel("~/Downloads/OH_Y5.xlsx", sheet = "SFY27_Fundable_Comprehensive") |>
  janitor::clean_names()

dac_reg <- readxl::read_excel("~/Downloads/OH_Y5.xlsx", sheet = "SFY27_DAC_Regionalization") |>
  janitor::clean_names() |>
  dplyr::mutate(
    row_number = row_number() + 1
  ) |>
  dplyr::left_join(
    f_c 
  )

ec <- readxl::read_excel("~/Downloads/OH_Y5.xlsx", sheet = "SFY27_EC ") |>
  janitor::clean_names() |>
  dplyr::mutate(
    row_number = row_number() + 1
  ) |>
  dplyr::left_join(
    f_c 
  )

lead <- readxl::read_excel("~/Downloads/OH_Y5.xlsx", sheet = "SFY27_Lead") |>
  janitor::clean_names() |>
  dplyr::mutate(
    row_number = row_number() + 1
  ) |>
  dplyr::left_join(
    f_c 
  ) 

dis <- readxl::read_excel("~/Downloads/OH_Y5.xlsx", sheet = "SFY27_Discount") |>
  janitor::clean_names() |>
  dplyr::mutate(
    row_number = row_number() + 1
  ) |>
  dplyr::left_join(
    f_c 
  )


sdc <- readxl::read_excel("~/Downloads/OH_Y5.xlsx", sheet = "EC_SDC") |>
  janitor::clean_names() |>
  dplyr::mutate(
    row_number = row_number() + 1
  ) |>
  dplyr::left_join(
    f_c 
  )


