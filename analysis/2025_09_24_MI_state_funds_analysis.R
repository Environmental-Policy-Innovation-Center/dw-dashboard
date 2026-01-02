# Constants ----
state <- "Michigan"
state_abbr <- "MI"
save_plots <- FALSE

# Read in raw data -----
mi_y0_raw <-  data.table::fread("year0/MI/data/mi-fy22-iup-final.csv", 
          colClasses = "character", na.strings = "") |>
  janitor::clean_names() |>
  dplyr::mutate(state_fiscal_year = "2022")

mi_y1_raw <- data.table::fread("year1/MI/data/22-Michigan_PPL.csv",
                  colClasses = "character", na.strings = "") |>
  janitor::clean_names() |>
  dplyr::mutate(state_fiscal_year = "2023")

mi_y2_raw <- data.table::fread("year2/MI/data/mi-sfy24-iup.csv",
                  colClasses = "character", na.strings = "") |>
  janitor::clean_names() |>
  dplyr::mutate(state_fiscal_year = "2024") |>
  dplyr::rename(project_description = "project_components")

mi_y3_raw <-  data.table::fread("year3/MI/data/MI-FY2025-DWSRF-Final-IUP.csv",
                  colClasses = "character", na.strings = "") |>
  janitor::clean_names() |>
  dplyr::mutate(state_fiscal_year = "2025") |>
  dplyr::rename(project_description = "project_scope")

mi_y4_raw <- data.table::fread("year4/MI/data/Final-SFY26 Comprehensive PPL.csv",
                  colClasses = "character", na.strings = "") |>
  janitor::clean_names() |>
  dplyr::mutate(
    state_fiscal_year = "2026",
    list = "comprehensive"
  ) |>
  dplyr::rename(project_description = "scope_of_work")

mi_y4_raw_lead <- data.table::fread("year4/MI/data/Final-SFY26 LSLR PPL.csv",
                  colClasses = "character", na.strings = "") |>
  janitor::clean_names() |>
  dplyr::mutate(
    state_fiscal_year = "2026",
    project_type = "Lead",
    list = "lead"
  ) |>
  dplyr::rename(project_description = "scope_of_work")

# Bind all IUPs (note: any data frame that starts with mi_y will be bound) -----
dfs <- mget(ls(pattern = "^mi_y"), envir = .GlobalEnv)
dfs <- dfs[sapply(dfs, is.data.frame)]
mi_raw <- dplyr::bind_rows(dfs)

# Clean raw data -----
#colnames check
colnames(mi_raw)[grepl("ec_", colnames(mi_raw))]
colnames(mi_raw)[grepl("emerging", colnames(mi_raw))]

mi_clean <- mi_raw |>
  mutate(project_type =  dplyr::case_when(
        !is.na(project_type) ~ project_type,
        grepl(lead_str, project_description, ignore.case=TRUE) | convert_to_numeric(bil_lslr_eligible_costs, TRUE)>0 | !is.na(lead_service_line_costs) | !is.na(lslr_costs) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE)  | convert_to_numeric(emerging_contaminant_costs)>0 |convert_to_numeric(ec_related_costs)>0 ~ "Emerging Contaminants",
        TRUE ~ "General")
  ) |>
  dplyr::filter(!is.na(project_description)) |>
  dplyr::filter(project_description != "")  
 

# Data Viz -----

## VIZ 1: Demand for lead -----
# Lauren Kwan Oct 29th at 3:11 PM
# we need a data viz that shows amount of lead funds available for projects vs. amount of lead funds going to meet demand vs. amount of general funds going towards lead projects
# For amount of lead funds available for projects, this would be the total_funding_available column for LSLR fed_cap_grant
# For amount of lead funds going to meet demand, this would be the items we'd discussed a while back, copying below so you don't have to scroll:
# Year 4: Projects with Funding Amounts for LSLR BIL Loan and LSLR BIL PF (from the LSLR PPL only, since there are two PPLs this year)
# Year 3: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# Year 2: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# Year 1: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# And lastly, for general funds going towards lead projects, this would be:
# Year 4: Lead projects with Funding Amounts for DWSRF Loan Allocation, DWSRF PF Allocation, BIL Loan Allocation, and BIL PF Allocation in the Comprehensive PPL. Also, lead projects with DWSRF Loan, BIL PF, BIL Loan Allocation, and BIL PF Allocation in the LSLR PPL.
# Year 3: Lead projects with Funding Amounts for DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF
# Year 2: Lead projects with Funding Amounts for DWSRF Traditional Loan, DWSRF Traditional PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF
# Year 1: Lead projects with Funding Amounts for DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF


## Amount of lead fund available (financial sheet) 
financial_lslr <- get_financial("Michigan") |>
  dplyr::filter(fed_cap_grant == "LSLR") |>
  dplyr::select(state_fiscal_year, total_funding_available) 

## Amount of lead funds going to meet demand 

# Year 4: Projects with Funding Amounts for LSLR BIL Loan and LSLR BIL PF (from the LSLR PPL only, since there are two PPLs this year) [2026]
# Year 3: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF [2025]
# Year 2: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF [2024]
# Year 1: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF [2023]
demand_lslr <- mi_clean |>
  dplyr::filter(
    !state_fiscal_year == "2022", #Y0
    project_type == "Lead" 
  ) |>
  dplyr::select(
    state_fiscal_year,
    list,
    lslr_bil_loan,
    lslr_bil_pf,
    bil_dwsrf_lslr_loan,
    bil_dwsrf_lslr_pf
  ) |>
  dplyr::mutate(
    dplyr:: across(
      .cols = -c(state_fiscal_year, list),
      .fns = ~ convert_to_numeric(.x, TRUE)
    )
  ) |>
  dplyr::group_by(state_fiscal_year) |>
  dplyr::summarise(
    lead_funds = sum(
      bil_dwsrf_lslr_loan * (state_fiscal_year %in% c("2023", "2024", "2025")) +
      bil_dwsrf_lslr_pf * (state_fiscal_year %in% c("2023", "2024", "2025")) +
      lslr_bil_pf * (state_fiscal_year == "2026" & list == "lead") +
      lslr_bil_loan * (state_fiscal_year == "2026" & list == "lead"),
      na.rm = TRUE
    )
  )

## Amount of general funds going towards lead projects
# Year 4: Lead projects with Funding Amounts for DWSRF Loan Allocation, DWSRF PF Allocation, BIL Loan Allocation, and BIL PF Allocation in the Comprehensive PPL. Also, lead projects with DWSRF Loan, BIL PF, BIL Loan Allocation, and BIL PF Allocation in the LSLR PPL. [2026]
# Year 3: Lead projects with Funding Amounts for DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF [2025]
# Year 2: Lead projects with Funding Amounts for DWSRF Traditional Loan, DWSRF Traditional PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF [2024]
# Year 1: Lead projects with Funding Amounts for DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF [2023]
general_to_lead <- mi_clean |>
  dplyr::filter(
    !state_fiscal_year == "2022",
    project_type == "Lead"
) |>
  dplyr::select(
    state_fiscal_year,
    list, 
    dwsrf_loan_allocation,
    dwsrf_pf_allocation,
    bil_loan_allocation,
    bil_pf_allocation,
    dwsrf_loan,
    bil_pf,
    dwsrf_pf,
    bil_dwsrf_supplemental_loan,
    bil_dwsrf_supplemental_pf,
    dwsrf_traditional_loan,
    dwsrf_traditional_pf
  ) |>
  dplyr::mutate(
    dplyr:: across(
      .cols = -c(state_fiscal_year, list),
      .fns = ~ convert_to_numeric(.x, TRUE)
    )
  ) |>
  dplyr::group_by(state_fiscal_year) |>
  dplyr::summarise(
    general_funds = sum(
      dwsrf_loan * (state_fiscal_year %in% c("2023", "2025", "2026")) +
      dwsrf_pf * (state_fiscal_year %in% c("2023", "2025")) +
      bil_dwsrf_supplemental_loan * (state_fiscal_year %in% c("2023", "2024","2025")) +
      bil_dwsrf_supplemental_pf * (state_fiscal_year %in% c("2023", "2024", "2025")) +
      dwsrf_traditional_loan * (state_fiscal_year == "2024") +
      dwsrf_traditional_pf  * (state_fiscal_year == "2024") +
      dwsrf_loan_allocation * (state_fiscal_year == "2026") +
      dwsrf_pf_allocation * (state_fiscal_year == "2026") +
      bil_loan_allocation * (state_fiscal_year == "2026") +
      bil_pf * (state_fiscal_year == "2026") +
      bil_pf_allocation * (state_fiscal_year == "2026"),
      na.rm = TRUE
    )
  )

# Combine all summaries
lead_summary <- financial_lslr |>
  dplyr::left_join(demand_lslr) |>
  dplyr::left_join(general_to_lead) |>
  tidyr::pivot_longer(-state_fiscal_year) |>
    dplyr::mutate(
      name = dplyr::case_when(
        name == "total_funding_available" ~ "Available LSLR Funds", 
        name == "lead_funds" ~ "LSLR Funds Demand", 
        name == "general_funds" ~ "General Funds Demand", 
      )) |>
    dplyr::mutate(
      name = forcats::as_factor(name),
      name = forcats::fct_relevel(name, "LSLR Funds Demand", "General Funds Demand", "Available LSLR Funds"),
      plt_str = paste0(name, ", ", state_fiscal_year, ": ", format_currency(value))
    ) 

funding_sources_demand_available_lead_p <- lead_summary |>
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(
    state_fiscal_year,
    value,
    fill= name,
    text = plt_str
  ),
    position = "dodge") +
  ggplot2::scale_y_continuous(labels=label_dollar()) + 
  ggplot2::scale_fill_manual(values = c("Available LSLR Funds" = "#172f60", "LSLR Funds Demand" = "#82AB6E", "General Funds Demand" = "gray")) +
  ggplot2::labs(x="State Fiscal Year",
         y="",
         title="Comparison of LSLR and General Demand to Available LSLR Funds",
         subtitle=get_subtitle_str(lead_summary$state_fiscal_year, "Michigan"),
        fill = ""
        ) +
  ggplot2::guides(
    label = FALSE
  ) +
    epic_chart_theme

funding_sources_demand_available_lead_gp <- plotly::ggplotly(funding_sources_demand_available_lead_p,  tooltip = "text") |>
   plotly::layout(title = list(text = paste0('Comparison of LSLR and General Demand to Available LSLR Funds',
                                      '<br>',
                                      get_subtitle_str(lead_summary$state_fiscal_year, "Michigan")))
           )


  if (save_plots) {
    run_save_plots(gg_plot=funding_sources_demand_available_lead_p,
                   gp_object=funding_sources_demand_available_lead_gp,
                   name="state-funds-funding-sources-demand-available-lead-funds-yoy")
  }

## VIZ 2: Funding amount DACs using DWSRF funds vs state funds -----
# Lauren Kwan Nov 12th at 1:59 PM
# Generating a new viz that compares the funding amounts for projects serving DACs using DWSRF funds vs state funds. 
# The columns we'd want for this are:
# Year 0: for projects with Disadvantaged Community = Yes, one bar with sum of DWSRF Loan Amount and Total Principal Forgiveness (non WIFTA), and one bar with Drinking Water Infrastructure (DWI) Grant Amount
# Year 1: for projects with Disadvantaged = Yes, one bar with sum of DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, BIL DWSRF Supplemental PF, BIL DWSRF Emerging Contaminants PF, BIL DWSRF LSLR Loan, and BIL DWSRF LSLR PF. Then one bar with ARP Grant
# Year 2: for projects with Disadvantaged Status = "Overburdened" or "Significantly Overburdened", one bar with sum of DWSRF Traditional Loan, DWSRF Traditional PF, BIL DWSRF Supplemental Loan, BIL DWSRF Supplemental PF, BIL DWSRF Emerging Contaminants PF, BIL DWSRF LSLR Loan, and BIL DWSRF LSLR PF. Then one bar with sum of ARPA Grant, DWI Grant, and LSLR + WM ARPA (new).
# Year 3: for projects with Overburdened Determination = "Overburdened" or "Significantly Overburdened," one bar with sum of DWSRF Loan, DWSRF PF, BIL Emerging Contaminant PF, BIL DWSRF Supplemental Loan, BIL DWSRF Supplemental PF, BIL DWSRF Loan, and BIL DWSRF LSLR PF. Then one bar with State LSLR + WM Grant
# Year 4: for projects with Disadvantaged Status in Comprehensive PPL or Disadvantaged in LSLR PPL =  "Overburdened" or "Significantly Overburdened", one bar with sum of Total Award from Comprehensive PPL and LSLR BIL Loan, LSLR BIL PF, DWSRF Loan and BIL PF from LSLR PPL. Then one bar with Lead & Infrastructure Grant

mi_dacs <- mi_clean |>
  dplyr::select(
    state_fiscal_year,
    list, 
    disadvantaged_community, #y0
    disadvantaged, #y1
    disadvantaged_status, #y2, y4
    overburdened_determination, #y3
    dwsrf_loan_amount, #y0
    total_principal_forgiveness_non_wifta, #y0
    drinking_water_infrastructure_dwi_grant_amount, #y0 
    dwsrf_loan, #y1, y3, y4 (lslr)
    dwsrf_pf, #y1, y2, y4 (lslr)
    bil_dwsrf_supplemental_loan, #y1, y2, y3
    bil_dwsrf_supplemental_pf, #y1, y2, y3
    bil_dwsrf_emerging_contaminants_pf, #y1, y2
    bil_dwsrf_lslr_loan, #y1, y2, y3
    bil_dwsrf_lslr_pf, #y1, y2, y3
    arp_grant, #y1
    dwsrf_traditional_loan, #y2
    dwsrf_traditional_pf, #y2
    bil_dwsrf_emerging_contaminants_pf, #y2
    arpa_grant, #y2
    dwi_grant, #y2
    lslr_wm_arpa_new, #y2
    bil_emerging_contaminant_pf, #y3
    bil_dwsrf_supplemental_loan, #y3
    state_lslr_wm_grant, #y3
    total_award, #y4 comprehensive
    lslr_bil_loan, #y4,
    lslr_bil_pf, #y4
    bil_pf,
    lead_infrastructure_grant #y4
  ) |>
  dplyr::mutate(
    dplyr:: across(
      .cols = -c(state_fiscal_year, list, disadvantaged_community, disadvantaged, disadvantaged_status, overburdened_determination),
      .fns = ~ convert_to_numeric(.x, TRUE)
    )
  ) |>
  dplyr::mutate(
    dplyr:: across(
      .cols = c(disadvantaged_community, disadvantaged, disadvantaged_status, overburdened_determination),
      .fns = ~  stringr::str_replace_all(.x, "[\r\n]", " ")
    )
  ) |>
  dplyr::mutate(
    dplyr:: across(
      .cols = c(disadvantaged_community, disadvantaged, disadvantaged_status, overburdened_determination),
      .fns = ~  stringr::str_squish(.x)
    )
  ) |>
  dplyr::group_by(state_fiscal_year) |>
  dplyr::summarise(
    dwsrf_funds = sum(
      dwsrf_loan_amount * (disadvantaged_community == "Yes" & state_fiscal_year == "2022") +
      total_principal_forgiveness_non_wifta * (disadvantaged_community == "Yes" & state_fiscal_year == "2022") +
      dwsrf_loan * (disadvantaged == "Yes" & state_fiscal_year == "2023") +
      dwsrf_pf * (disadvantaged == "Yes" & state_fiscal_year == "2023") +
      bil_dwsrf_supplemental_loan * (disadvantaged == "Yes" & state_fiscal_year == "2023") + 
      bil_dwsrf_supplemental_pf * (disadvantaged == "Yes" & state_fiscal_year == "2023") +
      bil_dwsrf_emerging_contaminants_pf * (disadvantaged == "Yes" & state_fiscal_year == "2023") +
      bil_dwsrf_lslr_loan * (disadvantaged == "Yes" & state_fiscal_year == "2023") +
      bil_dwsrf_lslr_pf  * (disadvantaged == "Yes" & state_fiscal_year == "2023") +
      dwsrf_traditional_loan * (disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2024") +
      dwsrf_traditional_pf * (disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2024") +
      bil_dwsrf_supplemental_loan * (disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2024") +
      bil_dwsrf_supplemental_pf * (disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2024") +  
      bil_dwsrf_emerging_contaminants_pf * (disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2024") +
      bil_dwsrf_lslr_loan * (disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2024") +
      bil_dwsrf_lslr_pf * (disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2024") +  
      dwsrf_loan * (overburdened_determination %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2025") +
      dwsrf_pf * (overburdened_determination %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2025") +
      bil_emerging_contaminant_pf  * (overburdened_determination %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2025") +
      bil_dwsrf_supplemental_loan * (overburdened_determination %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2025") +  
      bil_dwsrf_supplemental_pf * (overburdened_determination %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2025") +
      bil_dwsrf_lslr_loan * (overburdened_determination %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2025") +
      bil_dwsrf_lslr_pf * (overburdened_determination %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2025") +  
      total_award  * (disadvantaged_status %in% c("Overburdened","Significantly Overburdened") & state_fiscal_year == "2026" & list == "comprehensive") +
      lslr_bil_loan  * (disadvantaged %in% c("Overburdened","Significantly Overburdened") & state_fiscal_year == "2026" & list == "lead") +
      lslr_bil_pf  * (disadvantaged %in% c("Overburdened","Significantly Overburdened") & state_fiscal_year == "2026" & list == "lead") +
      dwsrf_loan * (disadvantaged %in% c("Overburdened","Significantly Overburdened") & state_fiscal_year == "2026" & list == "lead") +
      bil_pf * (disadvantaged %in% c("Overburdened","Significantly Overburdened") & state_fiscal_year == "2026" & list == "lead"),
    na.rm = TRUE),
    state_funds = sum(
      drinking_water_infrastructure_dwi_grant_amount  * (disadvantaged_community == "Yes" & state_fiscal_year == "2022") +
      arp_grant * (disadvantaged == "Yes" & state_fiscal_year == "2023") +  
      arpa_grant * (disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2024") +
      dwi_grant * (disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2024") +
      lslr_wm_arpa_new * (disadvantaged_status %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2024") +  
      state_lslr_wm_grant * (overburdened_determination %in% c("Overburdened", "Significantly Overburdened") & state_fiscal_year == "2025") +
      lead_infrastructure_grant  * (disadvantaged %in% c("Overburdened","Significantly Overburdened") & state_fiscal_year == "2026"),
      na.rm = TRUE
    )
  )

dac_p <- mi_dacs |>
  tidyr::pivot_longer(-state_fiscal_year) |>
  dplyr::mutate(
    name = dplyr::case_when(
      name == "dwsrf_funds" ~ "DWSRF Funds" ,
      name == "state_funds" ~ "Other Federal and State Funds"
    ),
    plt_str = paste0(name, ", ", state_fiscal_year, ": ", format_currency(value))
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(
    state_fiscal_year,
    value,
    fill= name,
    text = plt_str
  ),
    position = "dodge") +
  ggplot2::scale_y_continuous(labels=label_dollar()) + 
  ggplot2::scale_fill_manual(values = c("DWSRF Funds" = "#172f60", "Other Federal and State Funds" = "#527CAF")) +
  ggplot2::labs(x="State Fiscal Year",
         y="",
         title="Funding amounts for projects serving DACs using DWSRF funds compared to other federal and state funds",
         subtitle=get_subtitle_str(mi_dacs$state_fiscal_year, "Michigan"),
        fill = ""
        ) +
  ggplot2::guides(
    label = FALSE
  ) +
    epic_chart_theme


dac_gp <- plotly::ggplotly(dac_p,  tooltip = "text") |>
   plotly::layout(title = list(text = paste0('Funding amounts for projects serving DACs using DWSRF funds compared to other federal and state funds',
                                      '<br>',
                                      get_subtitle_str(mi_dacs$state_fiscal_year, "Michigan")))
           )


  if (save_plots) {
    run_save_plots(gg_plot=dac_p,
                   gp_object=dac_gp,
                   name="state-funds-dac-dwsrf-state-funds-yoy")
  }


## VIZ 3: State Funds -----

mi_state_funds <- mi_clean |>
  dplyr::select(
    state_fiscal_year,
    drinking_water_infrastructure_dwi_grant_amount,
    arp_grant,
    arpa_grant,
    dwi_grant,
    state_lslr_wm_grant,
    lead_infrastructure_grant,
    lslr_wm_arpa_new
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = -state_fiscal_year, 
      .fns = ~ convert_to_numeric(.x, TRUE)
    )
  ) |>
  dplyr::group_by(state_fiscal_year) |>
  dplyr::summarise(
    state_funds = sum(
      drinking_water_infrastructure_dwi_grant_amount * (state_fiscal_year %in% c("2022")) +
      arp_grant * (state_fiscal_year %in% c("2023")) +
      lslr_wm_arpa_new  * (state_fiscal_year %in% c("2024")) +
      arpa_grant * (state_fiscal_year %in% c("2024")) +
      dwi_grant * (state_fiscal_year %in% c("2024")) +
      state_lslr_wm_grant * (state_fiscal_year == "2025") +
      lead_infrastructure_grant * (state_fiscal_year == "2026"),
      na.rm = TRUE
    )
  )

mi_project_dollars <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1MtnflPSLXBvcPMGLmwwCtMe21wtXxGGKswzaE6m-GBw", sheet="Project Dollars")

mi_total_expected_funding_summary <- mi_project_dollars |>
  dplyr::filter(state == "Michigan") |>
  dplyr::select(state_fiscal_year, total_funding_amount) |>
  dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year)) |>
  dplyr::left_join(mi_state_funds) |>
  dplyr::mutate(expected_funding = total_funding_amount + state_funds ) |>
  dplyr::select(state_fiscal_year, expected_funding )  |>
  tidyr::pivot_longer(
    -state_fiscal_year,
    names_to = "funding_category"
  ) 

mi_total_demand_summary <- mi_project_dollars |>
  dplyr::filter(state == "Michigan") |>
  dplyr::select(state_fiscal_year, total_project_cost, total_requested_amount) |>
  dplyr::mutate(total_demand = coalesce(total_project_cost, total_requested_amount)) |>
  dplyr::select(state_fiscal_year, total_demand) |>
  tidyr::pivot_longer(
    -state_fiscal_year,
    names_to = "funding_category"
  )

mi_total_available_summary <- get_financial("Michigan") |>
  dplyr::select(state_fiscal_year, total_funding_available,	total_funding_available_state_funds) |>
  dplyr::group_by(state_fiscal_year) |>
  dplyr::summarise(total_funding_available =sum(total_funding_available, na.rm = TRUE),	total_funding_available_state_funds = sum(total_funding_available_state_funds, na.rm = TRUE)) |>
  tidyr::pivot_longer(
    -state_fiscal_year,
    names_to = "funding_category"
  )


state_funds <- dplyr::bind_rows(
  mi_total_expected_funding_summary, 
  mi_total_demand_summary |> dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year)),
  mi_total_available_summary
  ) |>
    dplyr::mutate(
      funding_type = dplyr::case_when(
        funding_category == "total_demand" ~ "Demand for Funds",
        stringr::str_detect(funding_category, "expected") ~ "On Fundable List",
        .default = "Available Funds"
      )
    ) |>
  dplyr::mutate(
    funding_type = forcats::as_factor(funding_type),
    funding_type = forcats::fct_relevel(funding_type, "Available Funds", "On Fundable List", "Demand for Funds")
  ) |>
  dplyr::mutate(
    category_label = dplyr::case_when(
      stringr::str_detect(funding_category, "state") ~ "Available Other Federal and State Funds",
      .default = "Available DWSRF Funds"
  )) 

dodge_width <- 0.9
n_funding_types <- 3
state_abbr <- "MI"

state_funds_p <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = state_funds |> 
      dplyr::group_by(state_fiscal_year, funding_type) |> 
      dplyr::summarise(value = sum(value), .groups = "drop"),
    ggplot2::aes(
      x = state_fiscal_year,
      y = value,
      fill = funding_type,
      text = paste0(funding_type, " ", state_fiscal_year, ":<br>", scales::dollar(value))
    ),
    color = "black",
    position = ggplot2::position_dodge(width = dodge_width)
  ) +
  ggplot2::geom_col(
    data = state_funds |> 
      dplyr::filter(funding_type == "Available Funds") |>
      dplyr::mutate(x_pos = as.numeric(factor(state_fiscal_year)) - dodge_width / 3),
    ggplot2::aes(
      x = x_pos,
      y = value,
      fill = category_label,
      group = state_fiscal_year,
      text = paste0(category_label, " ", state_fiscal_year, ":<br>", scales::dollar(value))
    ),
    color = "black",
    width = dodge_width / n_funding_types,
    position = "stack"
  ) +
  ggplot2::scale_y_continuous(labels = scales::label_dollar()) + 
  ggplot2::labs(
    x = "State Fiscal Year",
    y = "",
    title = "Comparison of demand for funds, available funds, and funds on fundable list",
    subtitle = get_subtitle_str(state_funds$state_fiscal_year, "Michigan")
  ) +
  #ggplot2::scale_fill_discrete(breaks = c("Available Funds", "Available SRF Funds", "Available State Funds", "On Fundable List", "Demand for Funds")) + 
  ggplot2::scale_fill_manual(
    breaks = c("Available Funds", "Available DWSRF Funds", "Available Other Federal and State Funds", "On Fundable List", "Demand for Funds"),
    values = 
    c(
      "Available Funds" = "#4ea324",
        "Demand for Funds" = "#172f60",
        "On Fundable List" = "#791a7b",
        "Available DWSRF Funds" = "#6bc842",
        "Available Other Federal and State Funds" = "#3d7f1c"
    ), name = "") +
  epic_chart_theme

# # Convert to plotly and remove specific legend items
state_funds_gp <- plotly::ggplotly(state_funds_p,  tooltip = "text") |>
   plotly::layout(title = list(text = paste0('Comparison of demand for funds, available funds, and funds on fundable list',
                                      '<br>',
                                      get_subtitle_str(state_funds$state_fiscal_year, "Michigan")))
           )
# # Reorder the traces to match your desired legend order
# desired_order <- c("Available Funds", "Available SRF Funds", "Available State Funds", 
#                    "On Fundable List", "Demand for Funds")

# # Get current trace names and reorder
# trace_names <- sapply(state_funds_gp$x$data, function(tr) tr$name)
# new_order <- match(desired_order, trace_names)
# new_order <- new_order[!is.na(new_order)]  # Remove any NA values

# state_funds_gp$x$data <- state_funds_gp$x$data[new_order]

# Define desired order without "Available Funds"
desired_order <- c("Available DWSRF Funds", "Available Other Federal and State Funds", 
                   "On Fundable List", "Demand for Funds")

# Get current trace names
trace_names <- sapply(state_funds_gp$x$data, function(tr) tr$name)

# Find indices of traces we want to keep
keep_indices <- which(trace_names %in% desired_order)

# Filter and reorder traces
state_funds_gp$x$data <- state_funds_gp$x$data[keep_indices]
new_order <- match(desired_order, trace_names[keep_indices])
state_funds_gp$x$data <- state_funds_gp$x$data[new_order]

# # # Find and hide the SRF Funds and State Funds legend entries
# for (i in seq_along(state_funds_gp$x$data)) {
#   if (!is.null(state_funds_gp$x$data[[i]]$name) && 
#       state_funds_gp$x$data[[i]]$name %in% c("SRF Funds", "State Funds")) {
#     state_funds_gp$x$data[[i]]$showlegend <- FALSE
#   }
# }

state_funds_gp

  if (save_plots) {
    run_save_plots(gg_plot=state_funds_p,
                   gp_object=state_funds_gp,
                   name="state-funds-compare-demand-available-funds-expected-funds-yoy")
  }


## VIZ 3A: State Funds: Available and On Fundable breakdown ----

state_fund_breakdown <- mi_total_available_summary |> #from financial worksheet
  dplyr::mutate(
    category_label = dplyr::case_when(
      stringr::str_detect(funding_category, "state") ~ "Available Other Federal and State Funds",
      .default = "Available DWSRF Funds"
    )
  ) |>
  dplyr::bind_rows(
    mi_project_dollars |> #from standardized project level data
      dplyr::filter(state == "Michigan") |>
      dplyr::select(state_fiscal_year, total_funding_amount) |>
      dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year)) |>
      dplyr::left_join(mi_state_funds) |> #from state funds as defined by comms
      dplyr::rename(DWSRF = total_funding_amount) |>
      tidyr::pivot_longer(
        -state_fiscal_year,
        names_to = "funding_category"
      ) |>
      dplyr::mutate(
        category_label = dplyr::case_when(
          grepl("DWSRF", funding_category) ~ "On Fundable List DWSRF Funds",
          .default = "On Fundable List Other Federal and State Funds"
        )
      )
  ) |>
  dplyr::mutate(
    funding_category_ = dplyr::case_when(
      grepl("total_funding", funding_category) ~ "Available Funds",
      .default = "On Fundable List"
    )
  )

state_fund_breakdown_p <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = state_fund_breakdown,
    ggplot2::aes(
      x = state_fiscal_year,
      y = value,
      fill = funding_category_
    ),
    color = "black",
    position = ggplot2::position_dodge(width = dodge_width)
  ) +
  ggplot2::geom_col(
    data = state_fund_breakdown |>
      dplyr::filter(funding_category_ == "Available Funds") |>
      dplyr::mutate(x_pos = as.numeric(factor(state_fiscal_year)) - dodge_width / 4),
    ggplot2::aes(
      x = x_pos,
      y = value,
      fill = category_label,
      group = state_fiscal_year,
      text = paste0(category_label, " ", state_fiscal_year, ":<br>", scales::dollar(value))
    ),
    color = "black",
    width = dodge_width / 2,
    position = "stack"
  ) +
  ggplot2::geom_col(
    data = state_fund_breakdown |>
      dplyr::filter(funding_category_ == "On Fundable List") |>
      dplyr::mutate(x_pos = as.numeric(factor(state_fiscal_year)) + dodge_width / 4),
    ggplot2::aes(
      x = x_pos,
      y = value,
      fill = category_label,
      group = state_fiscal_year,
      text = paste0(category_label, " ", state_fiscal_year, ":<br>", scales::dollar(value))
    ),
    color = "black",
    width = dodge_width / 2,
    position = "stack"
  ) +
  ggplot2::scale_y_continuous(labels = scales::label_dollar()) + 
  ggplot2::labs(
    x = "State Fiscal Year",
    y = "",
    title = "Comparison of available funds to funds on funding lists from DWSRF and other federal and state funds",
    subtitle = get_subtitle_str(state_funds$state_fiscal_year, "Michigan")
  ) +
  #ggplot2::scale_fill_discrete(breaks = c("Available Funds", "Available SRF Funds", "Available State Funds", "On Fundable List", "Demand for Funds")) + 
  ggplot2::scale_fill_manual(
    breaks = c("Available DWSRF Funds", "Available Other Federal and State Funds", "On Fundable List DWSRF Funds", "On Fundable List Other Federal and State Funds"),
    values = 
    c(
        "On Fundable List DWSRF Funds" = "#791a7b",
        "On Fundable List Other Federal and State Funds" = "#e948ebff",
        "Available DWSRF Funds" = "#6bc842",
        "Available Other Federal and State Funds" = "#3d7f1c"
    ), name = "") +
  epic_chart_theme


state_fund_breakdown_gp <- plotly::ggplotly(state_fund_breakdown_p,  tooltip = "text") |>
   plotly::layout(title = list(text = paste0('Comparison of available funds to funds on funding lists from DWSRF and other federal and state funds',
                                      '<br>',
                                      get_subtitle_str(state_fund_breakdown$state_fiscal_year, "Michigan")))
           )

# Define desired order without "Available Funds"
desired_order <- c("Available DWSRF Funds", "Available Other Federal and State Funds", 
                   "On Fundable List DWSRF Funds", "On Fundable List Other Federal and State Funds")

# Get current trace names
trace_names <- sapply(state_fund_breakdown_gp$x$data, function(tr) tr$name)

# Find indices of traces we want to keep
keep_indices <- which(trace_names %in% desired_order)

# Filter and reorder traces
state_fund_breakdown_gp$x$data <- state_fund_breakdown_gp$x$data[keep_indices]
new_order <- match(desired_order, trace_names[keep_indices])
state_fund_breakdown_gp$x$data <- state_fund_breakdown_gp$x$data[new_order]

state_fund_breakdown_gp

  if (save_plots) {
    run_save_plots(gg_plot=state_fund_breakdown_p,
                   gp_object=state_fund_breakdown_gp,
                   name="state-funds-compare-breakdown-available-funds-expected-funds-yoy")
  }
   

## VIZ 3 B ----

state_fund_ef_breakdown_p <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = state_fund_breakdown |>
      dplyr::filter(funding_category_ == "On Fundable List"),
    ggplot2::aes(
      x = state_fiscal_year,
      y = value,
      fill = category_label,
      group = state_fiscal_year,
      text = paste0(category_label, " ", state_fiscal_year, ":<br>", scales::dollar(value))
    ),
    color = "black",
    position = "stack"
  ) +
  ggplot2::scale_y_continuous(labels = scales::label_dollar()) + 
  ggplot2::labs(
    x = "State Fiscal Year",
    y = "",
    title = "Total amount of funding for projects on fundable list from DWSRF and other federal and state funds",
    subtitle = get_subtitle_str(state_funds$state_fiscal_year, "Michigan")
  ) +
  ggplot2::scale_fill_manual(
    values = 
    c(
        "On Fundable List DWSRF Funds" = "#791a7b",
        "On Fundable List Other Federal and State Funds" = "#e948ebff"
    ), name = "") +
  epic_chart_theme


state_fund_ef_breakdown_gp <- plotly::ggplotly(state_fund_ef_breakdown_p,  tooltip = "text") |>
   plotly::layout(title = list(text = paste0('Total amount of funding for projects on fundable list from DWSRF and other federal and state funds',
                                      '<br>',
                                      get_subtitle_str(state_fund_breakdown$state_fiscal_year, "Michigan")))
           )

state_fund_ef_breakdown_gp

  if (save_plots) {
    run_save_plots(gg_plot=state_fund_ef_breakdown_p,
                   gp_object=state_fund_ef_breakdown_gp,
                   name="state-funds-compare-breakdown-expected-funds-yoy")
  }


## VIZ 4 General Lead funds to lead -----

# Lauren Kwan update on Oct 29 
# For amount of lead funds going to meet demand, this would be the items we'd discussed a while back, copying below so you don't have to scroll:
# Year 4: Projects with Funding Amounts for LSLR BIL Loan and LSLR BIL PF (from the LSLR PPL only, since there are two PPLs this year)
# Year 3: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# Year 2: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# Year 1: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# And lastly, for general funds going towards lead projects, this would be:
# Year 4: Lead projects with Funding Amounts for DWSRF Loan Allocation, DWSRF PF Allocation, BIL Loan Allocation, and BIL PF Allocation in the Comprehensive PPL. Also, lead projects with DWSRF Loan, BIL PF, BIL Loan Allocation, and BIL PF Allocation in the LSLR PPL.
# Year 3: Lead projects with Funding Amounts for DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF
# Year 2: Lead projects with Funding Amounts for DWSRF Traditional Loan, DWSRF Traditional PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF
# Year 1: Lead projects with Funding Amounts for DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF

# Lauren Kwan Sep 24th at 3:30 PM : # General Purpose funds
# Lauren Kwan Sep 25th at 12:10 PM : # LSLR dedicated funds 

# And lastly, for general funds going towards lead projects, this would be:
# Year 4: Lead projects with Funding Amounts for DWSRF Loan Allocation, DWSRF PF Allocation, BIL Loan Allocation, and BIL PF Allocation in the Comprehensive PPL. Also, lead projects with DWSRF Loan, BIL PF, BIL Loan Allocation, and BIL PF Allocation in the LSLR PPL.
# Year 3: Lead projects with Funding Amounts for DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF
# Year 2: Lead projects with Funding Amounts for DWSRF Traditional Loan, DWSRF Traditional PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF
# Year 1: Lead projects with Funding Amounts for DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF
mi_general_funds_lead <- mi_clean  |>
  dplyr::filter(project_type == "Lead") |>
  dplyr::select(
    state_fiscal_year,
    dwsrf_loan, 
    dwsrf_pf,
    bil_dwsrf_supplemental_loan, 
    bil_dwsrf_supplemental_pf, 
    dwsrf_traditional_loan, 
    dwsrf_traditional_pf, 
    dwsrf_loan_allocation, 
    dwsrf_pf_allocation, 
    bil_loan_allocation, 
    bil_pf_allocation,
    bil_pf,
    list
  ) |>
  dplyr::mutate(
    dplyr:: across(
      .cols = -c(state_fiscal_year, list), 
      .fns = ~ convert_to_numeric(.x, TRUE)
    )
  ) |>
  dplyr::group_by(state_fiscal_year) |>
  dplyr::summarise(
    general_purpose_funds = sum(
      dwsrf_loan * (state_fiscal_year %in% c("2023","2025")) +
      dwsrf_pf * (state_fiscal_year %in% c("2023","2025")) +
      bil_dwsrf_supplemental_loan * (state_fiscal_year %in% c("2023","2024","2025")) +
      bil_dwsrf_supplemental_pf * (state_fiscal_year %in% c("2023","2024","2025")) +
      dwsrf_traditional_loan * (state_fiscal_year == "2024") +
      dwsrf_traditional_pf * (state_fiscal_year == "2024") +
      dwsrf_loan * (state_fiscal_year == "2026" & list == "lead") +  
      bil_pf * (state_fiscal_year == "2026" & list == "lead") +
      dwsrf_loan_allocation * (state_fiscal_year == "2026") +
      dwsrf_pf_allocation * (state_fiscal_year == "2026") +
      bil_loan_allocation * (state_fiscal_year == "2026") +
      bil_pf_allocation * (state_fiscal_year == "2026"),
      na.rm = TRUE
    )
  )

# LSLR dedicated funds as follows:
# Year 4: Projects with Funding Amounts for LSLR BIL Loan and LSLR BIL PF (from the LSLR PPL only, since there are two PPLs this year)
# Year 3: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# Year 2: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# Year 1: Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF

mi_lead_funds_lead <- mi_clean  |>
  dplyr::filter(project_type == "Lead") |>
  dplyr::select(
    state_fiscal_year,
    list,
    bil_dwsrf_lslr_loan,
    bil_dwsrf_lslr_pf,
    lslr_bil_loan,
    lslr_bil_pf
  ) |>
  dplyr::mutate(
    dplyr:: across(
      .cols = -c(state_fiscal_year, list), 
      .fns = ~ convert_to_numeric(.x, TRUE)
    )
  ) |>
  dplyr::group_by(state_fiscal_year) |>
  dplyr::summarise(
    lead_specific_funds = sum(
      bil_dwsrf_lslr_loan * (state_fiscal_year %in% c("2023","2024", "2025")) +
      bil_dwsrf_lslr_pf * (state_fiscal_year %in% c("2023","2024", "2025"))  +
      lslr_bil_loan * (state_fiscal_year == "2026" & list == "lead") +
      lslr_bil_pf * (state_fiscal_year == "2026" & list == "lead"),  
      na.rm = TRUE
    )
  )

## Financial data for general purpose funds ----
financial_general <- get_financial("Michigan") |>
  dplyr::filter(fed_cap_grant %in% c("Base", "IIJA Gen Supp")) |>
  dplyr::select(state_fiscal_year, total_funding_available) |>
  dplyr::group_by(state_fiscal_year) |>
  dplyr::summarise(total_funding_available = sum(total_funding_available, na.rm = TRUE))

## Amount of lead fund available (financial sheet) 
financial_lslr <- get_financial("Michigan") |>
  dplyr::filter(fed_cap_grant == "LSLR") |>
  dplyr::select(state_fiscal_year, total_funding_available) 

## Merged Viz -----
funds_lead_p <- mi_general_funds_lead  |>
  dplyr::left_join(mi_lead_funds_lead) |>
  tidyr::pivot_longer(-state_fiscal_year, names_to = "fund_source") |>
    dplyr::mutate(
    fund_source = dplyr::case_when(
      fund_source == "general_purpose_funds" ~ "General",
      fund_source == "lead_specific_funds" ~ "Lead"
    ),
    fund_source = forcats::as_factor(fund_source),
    fund_source = forcats::fct_relevel(fund_source, "Lead", "General")
) |>
  dplyr::left_join(financial_general |> dplyr::mutate(fund_source = "General") |> dplyr::rename(funding_general = "total_funding_available")) |>
  dplyr::left_join(financial_lslr |> dplyr::mutate(fund_source = "Lead") |> dplyr::rename(funding_lead = "total_funding_available")) |>
  tidyr::unite("total_funding_available", funding_general:funding_lead, na.rm = TRUE) |>
  dplyr::mutate(
    total_funding_available = as.numeric(total_funding_available),
    prc_source_lead = round(100*value/total_funding_available, 2),
    plt_str = ifelse(state_fiscal_year == "2026", 
    paste0(fund_source, ", ", state_fiscal_year, ": ", format_currency(value)),
    paste0(fund_source, ", ", state_fiscal_year, ": ", format_currency(value), " (", prc_source_lead, "%)"))
    ) |>
  dplyr::filter(!state_fiscal_year == "2022") |>
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(state_fiscal_year, value, alpha = fund_source, text = plt_str), fill = "#82AB6E", position = "dodge") +
  ggplot2::scale_alpha_manual(values = c("Lead" = 0.6, "General" = 1)) +
  ggplot2::scale_y_continuous(labels=label_dollar()) + 
  ggplot2::guides(alpha = ggplot2::guide_legend(title = "Source")) +
  ggplot2::labs(x="State Fiscal Year",
         y="",
         title="Lead specific and General purpose funds towards Lead projects",
         subtitle=get_subtitle_str(mi_lead_funds_lead$state_fiscal_year, "Michigan")) +
    epic_chart_theme
  

funds_lead_gp <- ggplotly(funds_lead_p, tooltip="text") |>
  layout(title = list(text = paste0('Lead specific and General purpose funds towards Lead projects',
                                      '<br>',
                                      get_subtitle_str(mi_general_funds_lead$state_fiscal_year[-1], "Michigan")
                                      )))

  if (save_plots) {
    run_save_plots(gg_plot = funds_lead_p,
                   gp_object = funds_lead_gp, 
                   name = "merged-general-lead-funds-to-lead-yoy")
    }

## VIZ 5 Lead purpose funds: allocated, unallocated -----

lead_allocated_unallocated <- mi_lead_funds_lead |>
  dplyr::left_join(financial_lslr) |>
  dplyr::mutate(
    lead_lead_perc =round(100*lead_specific_funds/total_funding_available, 2),
    unallocated_perc = 100-lead_lead_perc,
    unallocated_funds = unallocated_perc*total_funding_available/100,
  ) |>
  dplyr::select(
    -total_funding_available
  ) |>
 tidyr::pivot_longer(
    cols = c(ends_with("_funds")),
    names_to = c("category"),       
    values_to = "value"
  ) |>
  dplyr::mutate(
    category = dplyr::case_when(
      category =="lead_specific_funds" ~ "Allocated",
      category == "unallocated_funds" ~ "Unallocated"
    )
  ) |>
  pivot_longer(
    cols = c(ends_with("_perc")),
    names_to = c("perc_cat"),       
    values_to = "perc"
  ) |>
  dplyr::mutate(
    perc_cat = dplyr::case_when(
      perc_cat =="lead_lead_perc" ~ "Allocated",
      perc_cat == "unallocated_perc" ~ "Unallocated"
    )
  ) |>
  dplyr::mutate(cat_same = category == perc_cat) |>
  dplyr::filter(cat_same==TRUE) |>
  dplyr::select(-perc_cat, -cat_same) |>
   dplyr::mutate(
    plt_str = ifelse(state_fiscal_year == "2026", 
    paste0("LSLR ", category, " ", state_fiscal_year, ": ", format_currency(value)),
    paste0("LSLR ", category, " ",state_fiscal_year, ": ", format_currency(value), " (", perc, "%)")),
    category = forcats::as_factor(category),
    category = forcats::fct_relevel(category, "Unallocated", "Allocated")
    ) |>
  dplyr::filter(!state_fiscal_year == "2022")


lead_allocated_unallocated_p <- ggplot2::ggplot(lead_allocated_unallocated) +
  ggplot2::geom_col(ggplot2::aes(state_fiscal_year, value, alpha = category, text = plt_str), fill = "#82AB6E") +
  ggplot2::scale_alpha_manual(values = c("Unallocated" = 0.6, "Allocated" = 1)) +
  ggplot2::scale_y_continuous(labels=label_dollar()) + 
  ggplot2::guides(alpha = ggplot2::guide_legend(title = "")) +
  ggplot2::labs(x="State Fiscal Year",
         y="",
         title="Allocated and Unallocated LSLR Funds",
         subtitle=get_subtitle_str(unique(lead_allocated_unallocated$state_fiscal_year), "Michigan")) +
    epic_chart_theme 

lead_allocated_unallocated_gp <- ggplotly(lead_allocated_unallocated_p, tooltip="text") |>
  layout(title = list(text = paste0('Allocated and Unallocated LSLR Funds',
                                      '<br>',
                                      get_subtitle_str(lead_allocated_unallocated$state_fiscal_year, "Michigan")
                                      )))

  if (save_plots) {
    run_save_plots(gg_plot = lead_allocated_unallocated_p,
                   gp_object = lead_allocated_unallocated_gp, 
                   name = "allocated-unallocated-lead-funds-to-lead-yoy")
    }

## VIZ 6: Comparison of General Purpose Demand for Lead Projects to Available General Purpose Funds -----

general_demand_p <- mi_general_funds_lead  |>
  dplyr::left_join(financial_general) |> 
  dplyr::mutate(perc_of_available = round(100*general_purpose_funds/total_funding_available, 2)) |>
  tidyr::pivot_longer(-c(state_fiscal_year, perc_of_available)) |>  
  dplyr::mutate(
    funding_category = ifelse(grepl("general_purpose", name), "Demand for Funds", "Available Funds"),
    funding_category = forcats::as_factor(funding_category),
    funding_category = forcats::fct_relevel(funding_category, "Demand for Funds", "Available Funds"),
    plt_str = paste0(funding_category, ", SFY ", state_fiscal_year, ": ", format_currency(value), ifelse(name == "general_purpose_funds" &  perc_of_available !=Inf, paste0(" (", perc_of_available, "%)"), ""))
    ) |>
  dplyr::filter(!state_fiscal_year == "2022") |>
  ggplot2::ggplot(ggplot2::aes(state_fiscal_year, value, fill = funding_category, text = plt_str)) +
  ggplot2::geom_col(position = "dodge") +
  epic_chart_theme +
  ggplot2::scale_fill_manual(values = c("Available Funds" = "#4ea324",
        "Demand for Funds" = "#172f60")) +
  ggplot2::labs(
    title = "Comparison of General Purpose Demand for Lead Projects to Available General Purpose Funds",
    y = "",
    x = "State Fiscal Year",
    fill = ""
  ) +
  ggplot2::scale_y_continuous(labels=label_dollar())


general_demand_gp <- ggplotly(general_demand_p, tooltip="text") |>
  layout(title = list(text = paste0('Comparison of General Purpose Demand for Lead Projects to Available General Purpose Funds',
                                      '<br>',
                                      get_subtitle_str(mi_general_funds_lead$state_fiscal_year[-1], "Michigan")
                                      )))

  if (save_plots) {
    run_save_plots(gg_plot = general_demand_p,
                   gp_object = general_demand_gp, 
                   name = "general-demand-available-state-funds-to-lead-yoy")
    }

## Commented out ----

# # breakdown_gp <- ggplotly(breakdown_p, tooltip="text") |>
# #   layout(title = list(text = paste0('Breakdown of Lead specific and General purpose funds towards Lead projects',
# #                                       '<br>',
# #                                       get_subtitle_str(breakdown$state_fiscal_year, "Michigan")
# #                                       )))

# #   if (save_plots) {
# #     run_save_plots(gg_plot = breakdown_p,
# #                    gp_object = breakdown_gp, 
# #                    name = "breakdown-general-lead-funds-to-lead-yoy")
# #     }



# # Bind rows for general purpose funds -----

# # Lauren Kwan
# # Sep 24th at 3:30 PM
# # I'm thinking, to capture lead projects expecting funding from General Purpose funds for Years 1-4:
# # Year 4:
# # Lead projects with Funding Amounts for DWSRF Loan Allocation, DWSRF PF Allocation, BIL Loan Allocation, and BIL PF Allocation
# # Year 3:
# # Lead projects with Funding Amounts for DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF
# # Year 2:
# # Lead projects with Funding Amounts for DWSRF Traditional Loan, DWSRF Traditional PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF
# # Year 1:
# # Lead projects with Funding Amounts for DWSRF Loan, DWSRF PF, BIL DWSRF Supplemental Loan, and BIL DWSRF Supplemental PF

# mi_raw_full_gen <- dplyr::bind_rows(
#   mi_y1_raw,
#   mi_y2_raw,
#   mi_y3_raw,
#   mi_y4_raw,
#   mi_y4_raw_lead
# ) |>
#   dplyr::select(
#     project_description, 
#     project_scope,
#     project_components,
#     scope_of_work,
#     dwsrf_loan_allocation, 
#     dwsrf_pf_allocation, 
#     bil_loan_allocation, 
#     bil_pf_allocation, 
#     dwsrf_loan,
#     dwsrf_pf,
#     bil_dwsrf_supplemental_loan, 
#     bil_dwsrf_supplemental_pf, 
#     dwsrf_traditional_loan,
#     dwsrf_traditional_pf,
#     state_fiscal_year,
#     project_type,
#     emerging_contaminant_ec_cost,
#     emerging_contaminant_costs,
#     ec_related_costs,
#     bil_lslr_eligible_costs,
#     lead_service_line_costs,
#     lslr_costs
#   ) |>
#   tidyr::unite("project_description", project_description:scope_of_work, sep = "", na.rm = TRUE) |>
#   dplyr::mutate(
#   project_description = stringr::str_squish(project_description),
#   project_type =  case_when(
#         !is.na(project_type) ~ project_type,
#         grepl(lead_str, project_description, ignore.case=TRUE) | convert_to_numeric(bil_lslr_eligible_costs, TRUE)>0 | !is.na(lead_service_line_costs) | !is.na(lslr_costs) ~ "Lead",
#         grepl(ec_str, project_description, ignore.case=TRUE) | convert_to_numeric(emerging_contaminant_ec_cost)>0 | convert_to_numeric(emerging_contaminant_costs)>0 |convert_to_numeric(ec_related_costs)>0 ~ "Emerging Contaminants",
#         TRUE ~ "General")
#   ) |>
#   dplyr::filter(!is.na(project_description)) |>
#   dplyr::filter(project_description != "") |>
#   dplyr::select(
#     -emerging_contaminant_ec_cost,
#     -emerging_contaminant_costs,
#     -ec_related_costs,
#     -bil_lslr_eligible_costs,
#     -lead_service_line_costs,
#     -lslr_costs
#   )

# mi_general_funds_lead <- mi_raw_full_gen  |>
#   dplyr::filter(project_type == "Lead") |>
#   dplyr::select(
#     -project_type,
#     -project_description
#   ) |>
#   dplyr::mutate(
#     dplyr:: across(
#       .cols = -state_fiscal_year, 
#       .fns = ~ convert_to_numeric(.x, TRUE)
#     )
#   ) |>
#   dplyr::group_by(state_fiscal_year) |>
#   dplyr::summarise(
#     general_purpose_funds = sum(
#       dwsrf_loan * (state_fiscal_year %in% c("2023","2025")) +
#       dwsrf_pf * (state_fiscal_year %in% c("2023","2025")) +
#       bil_dwsrf_supplemental_loan * (state_fiscal_year %in% c("2023","2024","2025")) +
#       bil_dwsrf_supplemental_pf * (state_fiscal_year %in% c("2023","2024","2025")) +
#       dwsrf_traditional_loan * (state_fiscal_year == "2024") +
#       dwsrf_traditional_pf * (state_fiscal_year == "2024") +
#       dwsrf_loan_allocation * (state_fiscal_year == "2026") +
#       dwsrf_pf_allocation * (state_fiscal_year == "2026") +
#       bil_loan_allocation * (state_fiscal_year == "2026") +
#       bil_pf_allocation * (state_fiscal_year == "2026"),
#       na.rm = TRUE
#     )
#   )

# ## Financial data for general purpose funds ----
# financial_general <- get_financial("Michigan") |>
#    dplyr::filter(fed_cap_grant %in% c("Base", "IIJA Gen Supp")) |>
#   dplyr::select(state_fiscal_year, total_funding_available) |>
#   dplyr::group_by(state_fiscal_year) |>
#   dplyr::summarise(total_funding_available = sum(total_funding_available, na.rm = TRUE))

# ## Viz general purpose funds ----
# gen_funds_lead_p <- mi_general_funds_lead |>
#   dplyr::left_join(financial_general) |>
#   dplyr::mutate(
#     perc_general_lead =round(100*general_purpose_funds/total_funding_available, 2),
#     plt_str = ifelse(state_fiscal_year == "2026", 
#     paste0(state_fiscal_year, ": ", format_currency(general_purpose_funds)),
#     paste0(state_fiscal_year, ": ", format_currency(general_purpose_funds), " (", perc_general_lead, "%)"))
#     ) |>
#   ggplot2::ggplot() +
#   ggplot2::geom_col(ggplot2::aes(x=state_fiscal_year, y=general_purpose_funds, text = plt_str), fill = "#82AB6E") +
#   ggplot2::scale_y_continuous(labels=label_dollar()) + 
#   ggplot2::labs(x="State Fiscal Year",
#          y="",
#          title="General purpose funds towards Lead projects",
#          subtitle=get_subtitle_str(mi_general_funds_lead$state_fiscal_year, "Michigan")) +
#     epic_chart_theme
  

# gen_funds_lead_gp <- ggplotly(gen_funds_lead_p, tooltip="text") |>
#   layout(title = list(text = paste0('General purpose funds towards Lead projects',
#                                       '<br>',
#                                       get_subtitle_str(mi_general_funds_lead$state_fiscal_year, "Michigan")
#                                       )))

#   if (save_plots) {
#     run_save_plots(gg_plot = gen_funds_lead_p,
#                    gp_object = gen_funds_lead_gp, 
#                    name = "general-funds-to-lead-yoy")
#     }



# # Bind rows for lead purpose funds -----
# # Lauren Kwan
# #   Sep 25th at 12:10 PM
# # Thanks, Maria!
# # LSLR dedicated funds as follows:
# # Year 4:
# # Projects with Funding Amounts for BIL LSLR Loan Allocation and BIL LSLR PF Allocation
# # Year 3:
# # Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# # Year 2:
# # Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# # Year 1:
# # Projects with Funding Amounts for BIL DWSRF LSLR Loan and BIL DWSRF LSLR PF
# mi_raw_full_lead <- dplyr::bind_rows(
#   #mi_y0_raw,
#   mi_y1_raw,
#   mi_y2_raw,
#   mi_y3_raw,
#   mi_y4_raw,
#   mi_y4_raw_lead
# ) |>
#   dplyr::select(
#     #project_name, 
#     project_description, 
#     project_scope,
#     project_components,
#     scope_of_work,
#     bil_lslr_loan_allocation, 
#     bil_lslr_pf_allocation,
#     bil_dwsrf_lslr_loan, 
#     bil_dwsrf_lslr_pf,    
#     lead_infrastructure_grant,
#     state_fiscal_year,
#     project_type,
#     emerging_contaminant_ec_cost,
#     emerging_contaminant_costs,
#     ec_related_costs,
#     bil_lslr_eligible_costs,
#     lead_service_line_costs,
#     lslr_costs
#   ) |>
#   tidyr::unite("project_description", project_description:scope_of_work, sep = "", na.rm = TRUE) |>
#   dplyr::mutate(
#   project_description = stringr::str_squish(project_description),
#   project_type =  case_when(
#         !is.na(project_type) ~ project_type,
#         grepl(lead_str, project_description, ignore.case=TRUE) | convert_to_numeric(bil_lslr_eligible_costs, TRUE)>0 | !is.na(lead_service_line_costs) | !is.na(lslr_costs) ~ "Lead",
#         grepl(ec_str, project_description, ignore.case=TRUE) | convert_to_numeric(emerging_contaminant_ec_cost)>0 | convert_to_numeric(emerging_contaminant_costs)>0 |convert_to_numeric(ec_related_costs)>0 ~ "Emerging Contaminants",
#         TRUE ~ "General")
#   ) |>
#   dplyr::filter(!is.na(project_description)) |>
#   dplyr::filter(project_description != "") |>
#   dplyr::select(
#     -emerging_contaminant_ec_cost,
#     -emerging_contaminant_costs,
#     -ec_related_costs,
#     -bil_lslr_eligible_costs,
#     -lead_service_line_costs,
#     -lslr_costs
#   )


# ## Viz lead purpose funds ----
# lead_funds_lead_p <- mi_lead_funds_lead |>
#   dplyr::left_join(financial_lslr) |>
#   dplyr::mutate(
#     perc_lead_lead =round(100*lead_specific_funds/total_funding_available, 2),
#     plt_str = ifelse(state_fiscal_year == "2026", 
#     paste0(state_fiscal_year, ": ", format_currency(lead_specific_funds)),
#     paste0(state_fiscal_year, ": ", format_currency(lead_specific_funds), " (", perc_lead_lead, "%)"))
#     ) |>
#   ggplot2::ggplot() +
#   ggplot2::geom_col(ggplot2::aes(x=state_fiscal_year, y=lead_specific_funds, text = plt_str), fill = "#82AB6E") +
#   ggplot2::scale_y_continuous(labels=label_dollar()) + 
#   ggplot2::labs(x="State Fiscal Year",
#          y="",
#          title="Lead specific funds towards Lead projects",
#          subtitle=get_subtitle_str(mi_lead_funds_lead$state_fiscal_year, "Michigan")) +
#     epic_chart_theme
  

# lead_funds_lead_gp <- ggplotly(lead_funds_lead_p, tooltip="text") |>
#   layout(title = list(text = paste0('Lead specific funds towards Lead projects',
#                                       '<br>',
#                                       get_subtitle_str(mi_general_funds_lead$state_fiscal_year, "Michigan")
#                                       )))

#   if (save_plots) {
#     run_save_plots(gg_plot = lead_funds_lead_p,
#                    gp_object = lead_funds_lead_gp, 
#                    name = "lead-funds-to-lead-yoy")
#     }





# # Breakdown by loan and pf ----

# mi_general_funds_lead_breakdown <- mi_raw_full_gen  |>
#   dplyr::filter(project_type == "Lead") |>
#   dplyr::select(
#     -project_type,
#    # -project_name,
#     -project_description
#   ) |>
#   dplyr::mutate(
#     dplyr:: across(
#       .cols = -state_fiscal_year, 
#       .fns = ~ convert_to_numeric(.x, TRUE)
#     )
#   ) |>
#   dplyr::group_by(state_fiscal_year) |>
#   dplyr::summarise(
#     general_purpose_funds_loan = sum(
#       dwsrf_loan * (state_fiscal_year %in% c("2023","2025")) +
#       bil_dwsrf_supplemental_loan * (state_fiscal_year %in% c("2023","2024","2025")) + 
#       dwsrf_traditional_loan * (state_fiscal_year == "2024") +
#       dwsrf_loan_allocation * (state_fiscal_year == "2026") +
#       bil_loan_allocation * (state_fiscal_year == "2026"),
#       na.rm = TRUE
#     ),
#     general_purpose_funds_pf = sum(
#       dwsrf_pf * (state_fiscal_year %in% c("2023","2025")) +
#       bil_dwsrf_supplemental_pf * (state_fiscal_year %in% c("2023","2024","2025")) +
#       dwsrf_traditional_pf * (state_fiscal_year == "2024") +
#       dwsrf_pf_allocation * (state_fiscal_year == "2026") +
#       bil_pf_allocation * (state_fiscal_year == "2026"),
#       na.rm = TRUE
#     )
#   ) |>
#   dplyr::mutate(
#     general_purpose_total = general_purpose_funds_loan + general_purpose_funds_pf
#   )

# mi_lead_funds_lead_breakdown <- mi_raw_full_lead  |>
#   dplyr::filter(project_type == "Lead") |>
#   dplyr::select(
#     -project_type,
#    # -project_name,
#     -project_description
#   ) |>
#   dplyr::mutate(
#     dplyr:: across(
#       .cols = -state_fiscal_year, 
#       .fns = ~ convert_to_numeric(.x, TRUE)
#     )
#   ) |>
#   dplyr::group_by(state_fiscal_year) |>
#   dplyr::summarise(
#     lead_specific_funds_loan = sum(
#       bil_dwsrf_lslr_loan * (state_fiscal_year %in% c("2023","2024", "2025")) +
#       bil_lslr_loan_allocation * (state_fiscal_year == "2026") ,
#       na.rm = TRUE
#     ),
#      lead_specific_funds_pf = sum(
#       bil_dwsrf_lslr_pf * (state_fiscal_year %in% c("2023","2024", "2025")) +
#       bil_lslr_pf_allocation * (state_fiscal_year == "2026") ,
#       na.rm = TRUE
#     )
#   ) |>
#   dplyr::mutate(
#     lead_specific_total = lead_specific_funds_loan + lead_specific_funds_pf
#   )

# breakdown <- mi_general_funds_lead_breakdown |>
#   tidyr::pivot_longer(
#     cols = general_purpose_funds_loan:general_purpose_funds_pf,
#     names_to = "category") |>
#   dplyr::mutate(source = "General") |>
#   dplyr::mutate(perc = round(100*value/general_purpose_total, 2)) |>
#   dplyr::select(-general_purpose_total) |>
#   dplyr::bind_rows(
#     mi_lead_funds_lead_breakdown |>
#       tidyr::pivot_longer(
#         cols = lead_specific_funds_loan:lead_specific_funds_pf,
#         names_to = "category") |>
#       dplyr::mutate(source = "Lead") |>
#       dplyr::mutate(perc = round(100*value/lead_specific_total, 2)) |>
#       dplyr::select(-lead_specific_total)
#   ) |>
#   dplyr::mutate(
#     category = dplyr::case_when(
#       category == "general_purpose_funds_loan" ~ "Loan",
#       category == "general_purpose_funds_pf" ~ "PF",
#       category == "lead_specific_funds_loan" ~ "Loan",
#       category == "lead_specific_funds_pf" ~ "PF"
#     ),
#     plt_str = ifelse(
#       source == "General", 
#       paste0(source," ", category, ", ", state_fiscal_year, ": ", format_currency(value), "\n % of total available ", source, " purpose funds: ", perc, "%"),
#       paste0(source," ", category, ", ", state_fiscal_year, ": ", format_currency(value), "\n % of total available ", source, " specific funds: ", perc, "%")
#     )
#   ) 

# breakdown_p <- ggplot2::ggplot(breakdown |> dplyr::filter(value>0)) +
#   ggplot2::geom_col(
#     ggplot2::aes(
#       state_fiscal_year, value,
#       alpha = category, text = plt_str,
#       fill = source, group = source
#     ),
#     color = "black",
#     position = position_stack()
#   ) +
#   ggplot2::geom_text(
#     ggplot2::aes(state_fiscal_year, value, label = category, group = source),
#     position = position_stack(vjust = 0.5),
#     show.legend = FALSE
#   ) +
#   ggplot2::scale_alpha_manual(values = c("PF" = 0.5, "Loan" = 1)) +
#   ggplot2::scale_y_continuous(labels = scales::label_dollar()) +
#   ggplot2::guides(
#     fill = ggplot2::guide_legend(title = ""),
#     color = ggplot2::guide_legend(title = "")
#   ) +
#   ggplot2::scale_fill_manual(values = c("Lead" = "#82AB6E", "General" = "gray")) +
#   ggplot2::labs(x="State Fiscal Year",
#          y="",
#          title="Breakdown of Lead specific and General purpose funds towards Lead projects",
#          subtitle=get_subtitle_str(test$state_fiscal_year, "Michigan")) +
#   ggplot2::guides(
#     alpha = FALSE,
#     label = FALSE) +
#     epic_chart_theme


# breakdown_gp <- ggplotly(breakdown_p, tooltip="text") |>
#   layout(title = list(text = paste0('Breakdown of Lead specific and General purpose funds towards Lead projects',
#                                       '<br>',
#                                       get_subtitle_str(breakdown$state_fiscal_year, "Michigan")
#                                       )))

#   if (save_plots) {
#     run_save_plots(gg_plot = breakdown_p,
#                    gp_object = breakdown_gp, 
#                    name = "breakdown-general-lead-funds-to-lead-yoy")
#     }

# # Bind rows for STATE funds -----

