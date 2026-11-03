## Imports & Settings
library(here)
source(here("resources", "inputs.R"))
source(here("resources", "styling.R"))
source(here("resources", "importing.R"))
source(here("resources", "exporting.R"))
run_code_from_file(here("aws.txt"))

# 1. Load required libraries
library(ggplot2)
library(showtext)

# 2. Add your preferred Google Font 
# font_add_google(name = "Exact Google Font Name", family = "Your R nickname")
font_add_google("Poppins", "poppins")
font_add_google("Lato", "lato")

# 3. Automatically enable showtext font rendering
showtext_auto()

## Load Data

# Project-level data (clean full dataset)
all_projects <- aws.s3::s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") |>
  janitor::clean_names() |>
  dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year))

focus_state <- all_projects |> 
  dplyr::select(state) |>
  dplyr::distinct() |>
  dplyr::pull(state)

# Financial dataset (full)

all_financial <- map_df(focus_state, get_financial)

financial_by_project_type <- all_financial |>
  dplyr::mutate(
    project_type = case_when(
      fed_cap_grant %in% c("LSLR", "LSLR_FFY22","LSLR_FFY23") ~ "Lead",
      fed_cap_grant %in% c("EC", "EC_FFY22", "EC_FFY23") ~ "Emerging Contaminants (EC)",
      fed_cap_grant %in% c("Base", "IIJA Gen Supp", "Base & IIJA Gen Supp", "Base and IIJA Gen Supp") ~ "General", 
      TRUE ~ paste0("Combined: ", fed_cap_grant)
  )) |>
  dplyr::group_by(state, state_fiscal_year, project_type) |>
  dplyr::summarize(
    total_funding_available = sum(total_funding_available, na.rm=TRUE),
    .groups = "drop"
  ) 


# Funding Data Aggregates (state level sheet)
project_type_df <- aws.s3::s3read_using(read.csv, object="s3://funding-tracker/state-stats/dwsrf-funding-tracker-project-types.csv") |>
  janitor::clean_names() |>
  dplyr::mutate(
    state_fiscal_year = as.factor(state_fiscal_year),
    project_type = ifelse(project_type == "Emerging Contaminants", "Emerging Contaminants (EC)", project_type),
    project_type = factor(project_type, levels=c("Both Lead and EC","Emerging Contaminants (EC)", "Lead", "General")),
    )

# Funding Available (financial sheet)
all_funds <- all_financial |>
  dplyr::select(state, state_fiscal_year, total_funding_available) |>
  dplyr::group_by(state, state_fiscal_year) |>
  dplyr::summarize(total_funding_available = sum(total_funding_available, na.rm=TRUE))


# Demand for funds project type breakdown (Updated)

## funding amount
funding_amount_df <- all_projects |>
  dplyr::select(
    state,
    state_fiscal_year,
    project_type,
    expecting_funding,
    funding_amount,
    requested_amount,
    project_cost
  ) |>
   dplyr::mutate(
    # convert $ data to numeric (No Information --> NA)
    dplyr::across(c(funding_amount, requested_amount, project_cost), as.numeric),
    # Make any $0 NA for coalesce operation
    dplyr::across(c(funding_amount, requested_amount, project_cost), \(x) ifelse(x == 0, NA, x)),
    requested_amount_ef = requested_amount * (expecting_funding == "Yes"),
    total_project_cost_ef = project_cost * (expecting_funding == "Yes"),
    # Choose first non NA value following hierarchy
    selected_funding_amount = dplyr::coalesce(funding_amount, requested_amount_ef, total_project_cost_ef),
    # Note funding source of selected funding amount (string check will flag any issue)
    source_funding = dplyr::case_when(
      funding_amount > 0 ~ "funding_amount", 
      requested_amount_ef > 0 ~ "requested_amount_ef", 
      total_project_cost_ef > 0 ~ "total_project_cost_ef",
      .default = "check"
    )
  )

funding_amount_df_summary <- funding_amount_df |>
  dplyr::group_by(state, state_fiscal_year, project_type, source_funding) |>
  dplyr::summarise(funding_amount=sum(selected_funding_amount, na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::filter(!funding_amount == 0) |>
  tidyr::pivot_longer(cols = "funding_amount") |>
  tidyr::pivot_wider(
    names_from = name,
    values_from = value
  ) 

# demand
demand_df <- all_projects |>
  dplyr::select(
    state,
    state_fiscal_year,
    project_type,
    expecting_funding,
    funding_amount,
    requested_amount,
    project_cost
  ) |>
   dplyr::mutate(
    dplyr::across(c(funding_amount, requested_amount, project_cost), as.numeric),
    dplyr::across(c(funding_amount, requested_amount, project_cost), \(x) ifelse(x == 0, NA, x)),
    selected_demand = coalesce(requested_amount, project_cost, funding_amount), 
    source_demand = dplyr::case_when(
      requested_amount > 0 ~ "requested_amount", 
      project_cost > 0 ~ "project_cost", 
      funding_amount > 0 ~ "funding_amount", 
      .default = "check"
    )
  )

demand_df_summary <- demand_df |>
  dplyr::group_by(state, state_fiscal_year, project_type, source_demand) |>
  dplyr::summarise(demand=sum(selected_demand, na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::filter(!demand == 0) |>
  tidyr::pivot_longer(cols = "demand") |>
  tidyr::pivot_wider(
    names_from = name,
    values_from = value
  ) 


xx <- demand_df_summary |>
  dplyr::filter(grepl("lead|lsl", project_type, ignore.case = TRUE)) |>
  dplyr::group_by(state, state_fiscal_year, project_type) |>
  dplyr::summarise(
     value = sum(demand)
  ) |>
  dplyr::mutate(
    category = "Demand for Funds"
  )

yy <- funding_amount_df_summary |>
  dplyr::filter(grepl("lead|lsl", project_type, ignore.case = TRUE)) |>
  dplyr::group_by(state, state_fiscal_year, project_type) |>
  dplyr::summarise(
     value = sum(funding_amount)
  ) |>
  dplyr::mutate(
    category = "On Fundable List"
  )

zz <- financial_by_project_type |>
  dplyr::filter(grepl("lead|lsl", project_type, ignore.case = TRUE)) |>
  dplyr::rename(
    value = total_funding_available
  ) |>
  dplyr::mutate(
    category = "Available Funds"
  )

df <- xx |>
  dplyr::bind_rows(yy) |>
  dplyr::bind_rows(zz) 


df |>
  dplyr::mutate(
    category = forcats::fct(category, levels = c("Available Funds", "On Fundable List", "Demand for Funds")),
    star = ifelse(grepl("Combined", project_type), "*", "")
    )  |>
  ggplot() +
  geom_col(
    aes(
      state_fiscal_year,
      value,
      color = project_type,
      group = category,
      fill = category
    ),
    position = position_dodge(
      width = 0.9,
      preserve = "single"
    )
  ) +
  geom_text(
    aes(
      state_fiscal_year,
      value,
      label = star
    ),
    position = position_dodge(
      width = 0.9,
      preserve = "single"
    )
  ) +
  facet_wrap(~state, scales = "free_y") +
  theme_minimal() +
  ggplot2::scale_fill_manual(values = c(
    "Available Funds"              = "#4ea324",
    "Demand for Funds"             = "#172f60",
    "On Fundable List"             = "#791a7b"
  )) +
  ggplot2::scale_y_continuous(labels = scales::label_currency(scale_cut = scales::cut_short_scale())) 



df |>
  dplyr::filter(!grepl("Combined", project_type))|>
  dplyr::filter(!grepl("Both", project_type))|>
  dplyr::filter(state_fiscal_year %in% c("2023", "2024", "2025", "2026")) |>
  dplyr::filter(!state %in% c("Louisiana", "Maryland", "New Jersey", "Ohio", "Pennsylvania", "Tennessee", "Mississippi")) |>
  dplyr::mutate(
    category = forcats::fct(category, levels = c("Available Funds", "On Fundable List", "Demand for Funds"))
    )  |>
  ggplot() +
  geom_col(
    aes(
      state_fiscal_year,
      value,
      group = category,
      fill = category
    ),
    position = position_dodge(
      width = 0.9,
      preserve = "single"
    )
  ) +
  facet_wrap(~state, scales = "free_y", ncol =  4) +
  theme_minimal() +
  ggplot2::scale_fill_manual(values = c(
    "Available Funds"              = "#4ea324",
    "Demand for Funds"             = "#172f60",
    "On Fundable List"             = "#791a7b"
  )) +
  ggplot2::scale_y_continuous(labels = scales::label_currency(scale_cut = scales::cut_short_scale())) +
  labs(
    title = "Comparing DWSRF Demand for Funds to Available Funds and Funds for projects on Fundable List: Lead",
    x = "",
    y = "",
    fill = ""
  ) +
   theme(
    plot.title = element_text(size = 24, face = "bold", family = "poppins"),
    axis.text = element_text(size = 18, family = "lato"),
    strip.text = element_text(size = 20, family = "lato"),
    legend.text = element_text(size = 14, family = "lato")
  ) 

ggsave("~/Desktop/2026_09_25_lead_demand_multiples.jpeg", width = 11 , height = 8)






















# # Combine into one data frame with a category column
# combined_bars <- dplyr::bind_rows(
#   all_funds |> 
#     dplyr::transmute(state_fiscal_year, value = total_funding_available, category = "Available Funds", alpha_var = NA_character_),
#   funding_amount_by_source |> 
#     dplyr::transmute(state_fiscal_year, value = `On fundable list`, category = "On Fundable List", alpha_var = source_funding),
#   demand_by_source |> 
#     dplyr::transmute(state_fiscal_year, value = Demand, category = "Demand for Funds", alpha_var = source_demand)
# )

# # Stacked layers (the ones with alpha)
# stacked_bars <- dplyr::bind_rows(
#   funding_amount_by_source |>
#     dplyr::transmute(state_fiscal_year, value = `On fundable list`, category = "On Fundable List", stack_var = source_funding),
#   demand_by_source |>
#     dplyr::transmute(state_fiscal_year, value = Demand, category = "Demand for Funds", stack_var = source_demand)
# )

# # Pre-compute stacked positions within each category
# stacked_positioned <- stacked_bars |>
#   dplyr::group_by(state_fiscal_year, category) |>
#   dplyr::mutate(
#     ymax = cumsum(value),
#     ymin = ymax - value,
#     stack_var = dplyr::case_when(
#       stack_var == "requested_amount" ~ "Requested Amount",
#       stack_var == "project_cost" ~ "Project Cost",
#       stack_var == "funding_amount" ~ "Funding Amount", 
#       stack_var == "requested_amount_ef" ~ "Requested Amount", 
#       stack_var == "total_project_cost_ef" ~ "Project Cost",
#       .default = "check"
#     )
#   ) |>
#   dplyr::ungroup() |>
# dplyr::mutate(
#   state_fiscal_year = as.factor(
#     state_fiscal_year
#   ),
#   plt_str = paste0(category, ": ", scales::label_currency()(value), " (", stack_var, ")")
# ) 

