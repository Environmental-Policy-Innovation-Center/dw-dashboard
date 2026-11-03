# Load Data ###############################

# From NDW tool; to compare system counts
national_water_system <- aws.s3::s3read_using(
  readRDS,
  object = "s3://tech-team-data/national-dw-tool/clean/national/national_water_system.RData"
)

# Latest SDWIS data: July 14th; not yet incorporated to NDWT (but will be soon)
national_sdwis <- aws.s3::s3read_using(
  readr::read_csv,
  object = "s3://tech-team-data/national-dw-tool/clean/national/sdwis_viols.csv"
)

# Funding tracker TX data
texas_projects <- aws.s3::s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") |>
  janitor::clean_names() |>
  dplyr::mutate(
    state_fiscal_year = as.character(state_fiscal_year)
  ) |>
  dplyr::filter(state == "Texas")


# Explore Data ###############################
num_system_sabs <- national_water_system$epa_sabs |>
  dplyr::select(pwsid, primacy_agency, pop_cat_5, service_connections_count) |>
  dplyr::filter(primacy_agency == "TX") |>
  dplyr::summarise(
    number_of_systems = dplyr::n_distinct(pwsid)
  )

# 1.A: Texas systems with compliance challenges, including systems 
# 1) currently (e.g., within the most recent quarter where data is available) out of compliance with federal regulatory standards (health and M&R), and 
# 2) in compliance but experienced health-based violations in the relatively recent past (e.g., within the last three years)


TX_sdwis <- national_sdwis |>
  dplyr::filter(grepl("TX", pwsid))

# Sanity checks counts of pwsid
nrow(TX_sdwis)
length(unique(TX_sdwis$pwsid))
num_system_sabs

TX_sdwis_active <- TX_sdwis |>
  dplyr::filter(pws_activity_code == "A")

nrow(TX_sdwis_active)

nrow(TX_sdwis_active)/nrow(TX_sdwis)

# Clean Data ###############################
TX_sdwis_active_sufficient_data <- TX_sdwis |>
  dplyr::filter(pws_activity_code == "A")|>
  dplyr::filter(!grepl("Not Enough", total_coliform_rules_healthbased_5yr))

# sanity checks
nrow(TX_sdwis_active_sufficient_data)

nrow(TX_sdwis_active) - nrow(TX_sdwis_active_sufficient_data) #32 systems
nrow(TX_sdwis_active_sufficient_data)/nrow(TX_sdwis_active)

nrow(TX_sdwis_active_sufficient_data)/nrow(TX_sdwis)


# Write relevant columns of SDWIS data filtered by systems active and with sufficient data (i.e., removing Not Enough Data - Operating < 5 years)
TX_sdwis_active_sheet <- TX_sdwis_active_sufficient_data |>
  dplyr::select(
    pwsid, 
    pws_activity_code, 
    total_coliform_rules_healthbased_5yr,
    lead_and_copper_rule_healthbased_5yr,
    volatile_organic_chemicals_healthbased_5yr,
    surface_water_treatment_rules_healthbased_5yr,
    groundwater_rule_healthbased_5yr,
    inorganic_chemicals_healthbased_5yr,
    stage_2_disinfectants_and_disinfection_byproducts_rule_healthbased_5yr,
    radionuclides_and_revised_rad_rule_healthbased_5yr,
    stage_1_disinfectants_and_disinfection_byproducts_rule_healthbased_5yr,
    synthetic_organic_chemicals_healthbased_5yr,
    health_viols_5yr,
    paperwork_viols_5yr,
    total_viols_5yr
  ) 

# googlesheets4::write_sheet(
#   TX_sdwis_active_sheet,
#   ss = "https://docs.google.com/spreadsheets/d/14CdXunN9GMwnR-Mvq5PVVrBxJiOkyN3fW8IxLXg4vTY/edit?gid=2062650869#gid=2062650869",
#   sheet = "sdwis_filtered"
# )

# Summarize Data ###############################
sdwis_summary <- TX_sdwis_active_sheet |>
  dplyr::summarise(
    total_active_systems_TX = dplyr::n_distinct(pwsid), #confirmed each observation unique
    total_active_system_health_viol = sum(as.numeric(health_viols_5yr) > 0), 
    total_active_system_paper_viol = sum(as.numeric(paperwork_viols_5yr) > 0), 
    total_active_system_any_viol = sum(as.numeric(health_viols_5yr) > 0 | as.numeric(paperwork_viols_5yr) > 0)
    # ,
    # total_viols_5yr = sum(as.numeric(total_viols_5yr) > 0)
  ) |>
  dplyr::mutate(
    perc_systems_viol_health = scales::percent_format()(total_active_system_health_viol/total_active_systems_TX),
    perc_systems_viol_paper = scales::percent_format()(total_active_system_paper_viol/total_active_systems_TX),
    perc_total_viol = scales::percent_format()(total_active_system_any_viol/total_active_systems_TX)
  )

# googlesheets4::write_sheet(
#   sdwis_summary,
#   ss = "https://docs.google.com/spreadsheets/d/14CdXunN9GMwnR-Mvq5PVVrBxJiOkyN3fW8IxLXg4vTY/edit?gid=2062650869#gid=2062650869",
#   sheet = "sdwis_summary"
# )

# Extract PWSIDs to compare to FT data
pwsid_active_with_any_violations <- TX_sdwis_active_sheet |>
  dplyr::mutate(active_with_any_violations = as.numeric(total_viols_5yr) >0 ) |>
  dplyr::filter(active_with_any_violations) |>
  dplyr::pull(pwsid)

length(pwsid_active_with_any_violations)
# 1.B: Systems with compliance challenges that have/have not made it onto the IUP. 
# For those that haven't, we also seek to determine whether they've either submitted past funding applications but fell off somewhere in TWDB's process, or determine that they've never submitted a funding application period.
texas_projects_compliance_challenges <- texas_projects |>
  dplyr::filter(
    pwsid %in% pwsid_active_with_any_violations
  )

ft_sdwis_xwalk <- texas_projects_compliance_challenges |>
  dplyr::group_by(state_fiscal_year, pwsid) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = state_fiscal_year, values_from = n) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) tidyr::replace_na(x, 0)))

# googlesheets4::write_sheet(
#   ft_sdwis_xwalk,
#   ss = "https://docs.google.com/spreadsheets/d/14CdXunN9GMwnR-Mvq5PVVrBxJiOkyN3fW8IxLXg4vTY/edit?gid=2062650869#gid=2062650869",
#   sheet = "ft_crosswalk"
# )


counts_viol_xwalked <- TX_sdwis_active_sheet |>
  dplyr::filter(pwsid %in% texas_projects_compliance_challenges$pwsid)

# googlesheets4::write_sheet(
#   counts_viol_xwalked,
#   ss = "https://docs.google.com/spreadsheets/d/14CdXunN9GMwnR-Mvq5PVVrBxJiOkyN3fW8IxLXg4vTY/edit?gid=2062650869#gid=2062650869",
#   sheet = "num_violations_on_iup"
# )


counts_viol_xwalked |>
  dplyr::mutate(
    health_viols_5yr = as.numeric(health_viols_5yr),
    paperwork_viols_5yr = as.numeric(paperwork_viols_5yr),
    total_viols_5yr = as.numeric(total_viols_5yr)
  ) |>
  dplyr::summarise(
    ave_num_health_viol = mean(health_viols_5yr[health_viols_5yr >0]),
    ave_num_paper_viol = mean(paperwork_viols_5yr[paperwork_viols_5yr>0]),
    ave_num_viol = mean(total_viols_5yr),
    median_num_viol = median(total_viols_5yr),

    total_num_sys_paper_viol = sum(paperwork_viols_5yr>0),
    total_num_sys_health_viol = sum(health_viols_5yr>0),
    total_num_sys_g1_viol = sum(total_viols_5yr>1)
  )
