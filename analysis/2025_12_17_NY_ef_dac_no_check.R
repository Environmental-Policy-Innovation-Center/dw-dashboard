# Year 2 SFY2024
ny_annual |>
  dplyr::filter(grepl("lead", description, ignore.case = TRUE)) |>
  dplyr::filter(subsidized == "Yes") |>
  dplyr::filter(hardship == "Yes")


ny_lead_awards |> 
  dplyr::filter(project_number %in% 
    c(ny_lead |> dplyr::filter(is.na(dac)) |> dplyr::pull(project_number))
)

ny_ec_awards |> 
  dplyr::filter(project_number %in% 
    c(ny_ec |> dplyr::filter(is.na(dac)) |> dplyr::pull(project_number))
)

# Year 3 SFY2025
ny_annual |>
  dplyr::filter(grepl("lead", description, ignore.case = TRUE)) |>
  dplyr::filter(expecting_funding == "Yes") |>
  dplyr::filter(hardship == "Yes" & score != "H") |> #potentially 10 expecting funding and non dac, but need to check these ids against IIJA Gen Supp and LSLS PPLs to see if they move to the disadvantaged yes category
  #dplyr::filter(project_number %in% ny_gs$project_number) # they are all not on the IIJA Gen Supp
  dplyr::filter(project_number %in% 
    c(ny_lead |> dplyr::filter(dac == "No") |> dplyr::pull(project_number)))

#These 10 have 0 project cost and so they are drop from the dataset!

ny_lead_awards |> 
  dplyr::filter(project_number %in% 
    c(ny_lead |> dplyr::filter(dac == "No") |> dplyr::pull(project_number))
)

# Year 4 SFY2026
ny_annual |>
  dplyr::filter(
    expecting_funding == "Yes",
    hardship == "Yes" & score != "H"
  ) |>
  dplyr::filter(grepl("lead", description, ignore.case = TRUE)) #15 potential non dac expecting funding, but they are all project cost 0
  
ny_annual |> 
  dplyr::filter(expecting_funding == "Yes") |>
  dplyr::filter(project_number %in% 
    c(ny_lead |> dplyr::filter(is.na(codes)) |> dplyr::pull(project_number))
)

