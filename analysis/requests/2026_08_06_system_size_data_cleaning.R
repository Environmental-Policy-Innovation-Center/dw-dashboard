# Diagnose PWSID not 9 characters in length
pwsid_nchar_diagnostic <- ft_data_clean |>
  dplyr::filter(
    !is.na(pwsid) & pwsid != "No Information" & pwsid != ""
  ) |>
  dplyr::mutate(
    count_pwsid_char = nchar(pwsid)
  ) |>
  dplyr::filter(!count_pwsid_char == 9) |> 
  dplyr::relocate(
    pwsid, 
    count_pwsid_char
  ) |>
  dplyr::arrange(count_pwsid_char)

# Data cleaning - PWSID is 9 characters
# Compare with SDWIS data and impute values
ft_data_clean_pwsid <- ft_data_clean |>
  dplyr::mutate(
    checked_pwsid = ifelse(pwsid %in% pwsid_nchar_diagnostic$pwsid , "Yes", "No"),
    pwsid = dplyr::case_when(
      pwsid == "IL970200" ~ "IL0970200",
      pwsid == "IL50050" & borrower == "Argenta" ~ "IL1150050",
      pwsid == "IL50020" ~ "IL0050020",
      pwsid == "IL10100" ~ "IL0110100",
      pwsid == "IL850100" ~ "IL0850100",
      pwsid == "IL434140" ~ "IL0434140",
      pwsid == "IL110250" ~ "IL0110250",
      pwsid == "IL971250" ~ "IL0971250",
      pwsid == "IL03010510" ~ "IL0310510",
      pwsid == "IL00895550" ~ "IL0895550",
      pwsid == "IL850400" ~ "IL0850400",
      pwsid == "IL810300" ~ "IL0810300",
      pwsid == "IL034850" ~ "IL0374850",
      pwsid == "IL031330" ~ "IL0310330",
      pwsid == "IL210150" ~ "IL0210150",
      pwsid == "IL210050" ~ "IL0210050",
      pwsid == "IL630600" ~ "IL0630600",
      pwsid == "IL111100" ~ "IL0111100",
      pwsid == "IL150330" ~ "IL0150300",
      pwsid == "IL20105000"~ "IL2010500",
      pwsid == "IL210550" ~ "IL0210550",
      pwsid == "IL310540" ~ "IL0310540",
      pwsid == "IL312850" ~ "IL0312850",
      pwsid == "IL03013000" ~ "IL0313000",
      pwsid == "IL07500200" ~ "IL0750200",
      pwsid == "IL08550050" ~ "IL0850050",
      #cannot elucidate remaining IL (2)
      pwsid == "OH700612" & borrower == "Bridgeport" ~ "OH0700612",
      pwsid == "OH500603" & borrower == "Coolville" ~ "OH0500603",
      pwsid == "OH-4300411" ~ "OH4300411",
      pwsid == "OH36000614" ~ "OH3600614",
      pwsid == "OH903012" ~ "OH0903012",
      pwsid == "OH701212" ~ "OH5701212",
      pwsid == "OH79001711" ~ "OH7901711",
      pwsid == "OH450612" ~ "OH4500612",
      pwsid == "OH34001121" ~ "OH3400112",
      pwsid == "OH760011" ~ "OH7600011",
      pwsid == "OH701012" ~ "OH5701012",
      pwsid == "OH64022703" ~ "OH6402703",
      pwsid == "OH501111" ~ "OH0501111",
      pwsid == "OH403203" ~ "OH8403203",
      pwsid == "OH402703" ~ "OH6402703",
      pwsid == "OH201312" ~ "OH0201312",
      #cannot elucidate remaining OH (2, 1 only "OH")
      pwsid == "TX011013" ~ "TX0110013",
      pwsid == "TX00150018" & borrower == "San Antonio Water System" ~ "TX0150018",
      #cannot elucidate remaining TX (2 1composite, other pending)
      pwsid == "MD300002" ~ "MD0300002",
      pwsid == "MD210010" ~ "MD0210010",
      pwsid == "MD023006" ~ "MD0230006",
      pwsid == "MD020003" ~ "MD0200003",
      #cannot elucidate remaining MD (1)
      pwsid == "LA106007" ~ "LA1069007",
      pwsid == "IN528004" ~ "IN5280004",
      pwsid == "IN36003/52360" ~ "IN5236003",
      pwsid == "IN5236003/, IN5236008"  ~ "IN5236003",
      pwsid == "IN5245047, IN, IN5245067" ~ "IN5245047",
      pwsid == "IN524015" ~ "IN5245015",
      pwsid == "IN5236003, IN5263008" ~ "IN5236003", 
      pwsid == "TN0000531/ TN0000520" ~ "TN0000531", #composite, but LS; no project id
      .default = pwsid
    )
  ) 
# 111 observations where checked for PWSID nchar =/= 9 
# Review PWSID
ft_data_clean_pwsid |>
  dplyr::filter(
    !is.na(pwsid) & pwsid != "No Information" & pwsid != ""
  ) |>
  dplyr::mutate(
    count_pwsid_char = nchar(pwsid)
  ) |>
  dplyr::filter(!count_pwsid_char == 9) |> 
  dplyr::relocate(
    pwsid, 
    count_pwsid_char
  ) |>
  dplyr::arrange(count_pwsid_char) |>
  nrow()
# 12/111 observation have PWSID nchar =/= 9 still

# Diagnose inconsistencies for PWSID values within the same project ID
pwsid_diagnostics <- ft_data_clean_pwsid |>
  dplyr::filter(!project_id == "No Information") |>
  dplyr::group_by(state, project_id) |>
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_pwsid = dplyr::n_distinct(pwsid, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Address cases that appear over multiple SFYs (repeated project_ids)
  dplyr::filter(
    n_obs > 1
  )

# Subset single project id associated with multiple PWSIDs
single_project_id_multiple_pwsid <- pwsid_diagnostics |>
  dplyr::filter(
    n_pwsid > 1
  ) |>
  dplyr::arrange(desc(n_pwsid)) |>
  dplyr::select(state, project_id) |>
  dplyr::mutate(multiple_pwsid = "Yes")
# 74 unique non na project ids that had more than 1 PWSID associated with it


# Write dataframe to csv to compare and reconcile PWSIDs
ft_data_clean_pwsid |>
  dplyr::filter(!project_id == "No Information") |>
  dplyr::left_join(single_project_id_multiple_pwsid, by= c("state", "project_id")) |>
  dplyr::filter(multiple_pwsid == "Yes" ) |>
  dplyr::arrange(state, project_id) |>
  readr::write_csv("~/Desktop/2006_08_06_pwsid_reconciliation.csv")

# 196 observations affected PWSID inconsistency: manually fixed PWSID, project_id, and population, and noted original values in column name prefixed by base_

reconciled_pwsid <- readr::read_csv("~/Downloads/2026_08_06_pwsid_reconciled.csv", col_types = readr::cols(.default = "c")) |>
  dplyr::select(borrower, pwsid, base_pwsid, project_id, base_project_id, population, base_population, state, list, state_fiscal_year) |>
  dplyr::mutate(reconciled_pwsid = "Yes")

# clean data set with respect to PWSIDs
ft_data_clean_pwsid_full <- ft_data_clean_pwsid |>
  dplyr::left_join(
    reconciled_pwsid, by = c("project_id"="base_project_id", "pwsid"= "base_pwsid", "population" = "base_population", "borrower", "list", "state", "state_fiscal_year")
  ) |>
  dplyr::mutate(
    base_project_id = project_id,
    project_id = dplyr::coalesce(project_id.y, project_id),
    base_pwsid = pwsid,
    pwsid = dplyr::coalesce(pwsid.y, pwsid),
    base_population = population,
    population = dplyr::coalesce(population.y, population)
  ) |>
  dplyr::mutate(system_size = dplyr::case_when(
    population == "No Information" ~ "No Information",
    is.na(population) ~ "No Information", 
    # convert_to_numeric(population, TRUE) == 0 ~ "No Information",
    convert_to_numeric(population, TRUE) >= 0 & convert_to_numeric(population, TRUE) <= 3300 ~ "VSS (<= 3,300)" ,
    convert_to_numeric(population, TRUE) > 3300 & convert_to_numeric(population, TRUE) <= 10000 ~ "SS (3,301-10,000)",
    convert_to_numeric(population, TRUE) > 10000 ~ "LS (>10,000)",
    .default = "check"
  ))

# Review
ft_data_clean_pwsid_full |>
  dplyr::filter(!project_id == "No Information") |>
  dplyr::group_by(state, project_id) |>
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_pwsid = dplyr::n_distinct(pwsid, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Address cases that appear over multiple SFYs (repeated project_ids)
  dplyr::filter(
    n_obs > 1
  ) |>
  dplyr::filter(
    n_pwsid > 1
  ) |>
  View()


# Diagnose population issues
pop_diagnostics <- ft_data_clean_pwsid_full |>   
  dplyr::filter(!project_id == "No Information") |>
  dplyr::group_by(state, project_id) |>
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_pop = dplyr::n_distinct(system_size, na.rm = TRUE),
    n_missing_pop = sum(is.na(system_size)),
    .groups = "drop"
  ) |>
  # Address cases that appear over multiple SFYs (repeated project_ids)
  dplyr::filter(
    n_obs > 1
  )

# Subset single project id associated with multiple system size
single_project_id_multiple_pop <- pop_diagnostics |>
  dplyr::filter(
    n_pop > 1
  ) |>
  dplyr::arrange(desc(n_pop)) |>
  # dplyr::select(state, project_id) |>
  dplyr::mutate(multiple_pop = "Yes")


# 1019 unique non na project ids that had more than 1 population value associated with it (decided to look at sizes category, rather than pop due to time constraints, will take this back up later)
# 570 unique non na project ids that had more than 1 system size category associated with it

# Write dataframe to csv to compare and reconcile system size
ft_data_clean_pwsid_full |>
  dplyr::filter(!project_id == "No Information") |>
  dplyr::left_join(single_project_id_multiple_pop, by= c("state", "project_id")) |>
  dplyr::filter(multiple_pop == "Yes" ) |>
  dplyr::arrange(state, project_id) |>
  dplyr::relocate(system_size) |>
  readr::write_csv("~/Desktop/2006_08_06_ss_reconciliation.csv")


# use the latest system size category for each project_id

reconciled_system_size <- readr::read_csv("~/Downloads/2026_08_07_system_size_reconciled.csv", col_types = readr::cols(.default = "c")) |>
  dplyr::select(borrower, pwsid, project_id, intermediate_project_id, system_size, base_system_size, state, list, state_fiscal_year) |>
  dplyr::mutate(reconciled_ss = "Yes")

# clean data set with respect to PWSIDs and system size
ft_data_clean_pwsid_pop <- ft_data_clean_pwsid_full |>
  dplyr::select(-c(pwsid.y, project_id.y, population.y)) |>
  dplyr::rename(base_system_size = system_size) |>
  dplyr::left_join(
    reconciled_system_size, 
    by = c("project_id"="intermediate_project_id", "pwsid", "borrower", "list", "state", "state_fiscal_year")
  ) |>
  dplyr::mutate(
    intermediate_project_id = project_id,
    project_id = dplyr::coalesce(project_id.y, project_id),
    system_size =  dplyr::coalesce(system_size, base_system_size.x),
  ) |>
  dplyr::rename(
    base_system_size = base_system_size.x
  ) |>
  dplyr::select(
    -c(project_id.y, base_system_size.y)
  )


# Review
ft_data_clean_pwsid_pop |>  
  dplyr::filter(!project_id == "No Information") |>
  dplyr::group_by(state, project_id) |>
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_pop = dplyr::n_distinct(system_size, na.rm = TRUE),
    n_missing_pop = sum(is.na(system_size)),
    .groups = "drop"
  ) |>
  # Address cases that appear over multiple SFYs (repeated project_ids)
  dplyr::filter(
    n_obs > 1
  ) |>
  dplyr::filter(
    n_pop > 1
  ) |>
  dplyr::arrange(desc(n_pop)) |>
  View()



# Check population column equal to 0, and inspect cases where pwsid and system size categories have not been addressed

ft_data_clean_pwsid_pop |>
  dplyr::filter(!project_id == "No Information") |>
  dplyr::filter(population == "0") |>
  dplyr::filter(is.na(reconciled_pwsid) & is.na(reconciled_ss)) |>
  View()

# There are only 2 observations with project_id that meet this criteria, both in NJ SFY26. and both with PWSID, will default to system size category in SABs
ft_data_clean_full <- ft_data_clean_pwsid_pop |>
  dplyr::mutate(
    system_size = dplyr::case_when(
      project_id == "0810004-005" & state == "New Jersey" & state_fiscal_year == "2026" ~ "LS (>10,000)",
      project_id == "1009001-010" & state == "New Jersey" & state_fiscal_year == "2026" ~ "SS (3,301-10,000)",
      .default =   system_size
    ),
    reconciled_zero_pop = dplyr::case_when(
      project_id == "0810004-005" & state == "New Jersey" & state_fiscal_year == "2026" ~  "Yes",
      project_id == "1009001-010" & state == "New Jersey" & state_fiscal_year == "2026" ~  "Yes",
      .default = NA_character_
    )
  )


# System size data imputation 

data_imputation <- ft_data_clean_full |>
  dplyr::filter(
    grepl("No Information", system_size, fixed = TRUE),
    !grepl("No Information", pwsid, fixed = TRUE),
    !is.na(pwsid)
  ) |>
  dplyr::arrange(desc(project_id)) |>
  dplyr::left_join(ndwt_data_clean |> dplyr::select(pwsid, system_size), by = "pwsid") |>
  dplyr::mutate(
    system_size = dplyr::coalesce(system_size.y, system_size.x),
    reconciled_no_info_pop_w_pwsid = "Yes"
  ) |>
  dplyr::select(
    -c(system_size.y, system_size.x)
  )

ft_data_clean_imputed <- ft_data_clean_full |>
  dplyr::mutate(
    drop_val = dplyr::case_when(
      (grepl("No Information", system_size, fixed = TRUE)) & (!grepl("No Information", pwsid, fixed = TRUE)) & (!is.na(pwsid)) ~ "Yes",
      .default = "No"
    )
  ) |>
  dplyr::filter(drop_val == "No") |>
  dplyr::select(-drop_val) |>
  dplyr::bind_rows(data_imputation)

ft_data_clean_ss_complete_cases <- ft_data_clean_imputed |>
  dplyr::filter(
    # !is.na(project_id), 
    # !project_id == "No Information",
    !system_size == "No Information"
   )

unique(ft_data_clean_ss_complete_cases$state) # No population or pwsid for Michigan, the rest of the states here

# # There are 10 states with some degree of data completeness required to track agreements in different system sizes (i.e., project_id, system_size)
# # [1] "Alabama"      "Illinois"     "Indiana"      "Louisiana"    "Maryland"     "Michigan"     "Minnesota"    "Mississippi"  "New Jersey"  
# # [10] "New York"     "Ohio"         "Pennsylvania" "Tennessee"    "Texas"        "Wisconsin"   
# # Of note, MN and TX offer project ids in the IUPs, but we cannot map them to the portal data as they seem unrelated

# # The analysis can be performed for 8 states
# # "Alabama" "Illinois" "Indiana" "Louisiana" "Maryland" "Michigan" "Mississippi" "New Jersey" "New York" "Ohio" "Pennsylvania" "Tennessee" "Wisconsin"   

# ft_data_clean_complete_cases_8_states <- ft_data_clean_complete_cases |>
#   dplyr::filter(!state %in% c("Minnesota", "Texas"))

# Write dataset
ft_data_clean_ss_complete_cases |>
  readr::write_csv("~/Downloads/2026_08_11_ft_data_clean_ss_complete_cases.csv")
