# Lookups ----
epa_region_lookup <- tibble::tribble(
  ~state, ~epa_region, ~epa_region_label,
  "Alabama", "Region 4", "EPA Region 4: Southeast",
  "Mississippi", "Region 4", "EPA Region 4: Southeast",
  "Tennessee", "Region 4", "EPA Region 4: Southeast",

  "Illinois", "Region 5", "EPA Region 5: Great Lakes",
  "Indiana", "Region 5", "EPA Region 5: Great Lakes",
  "Michigan", "Region 5", "EPA Region 5: Great Lakes",
  "Minnesota", "Region 5", "EPA Region 5: Great Lakes",
  "Ohio", "Region 5", "EPA Region 5: Great Lakes",
  "Wisconsin", "Region 5", "EPA Region 5: Great Lakes",

  "Louisiana", "Region 6", "EPA Region 6: South Central",
  "Texas", "Region 6", "EPA Region 6: South Central",

  "New Jersey", "Region 2", "EPA Region 2: Northeast",
  "New York", "Region 2", "EPA Region 2: Northeast",

  "Maryland", "Region 3", "EPA Region 3: Mid-Atlantic",
  "Pennsylvania", "Region 3", "EPA Region 3: Mid-Atlantic"
)

geo_cluster_lookup <- tibble::tribble(
  ~state, ~geo_cluster,
  "Illinois", "Great Lakes",
  "Indiana", "Great Lakes",
  "Michigan", "Great Lakes",
  "Minnesota", "Great Lakes",
  "Ohio", "Great Lakes",
  "Wisconsin", "Great Lakes",

  "Maryland", "Northeast & Mid-Atlantic",
  "New Jersey", "Northeast & Mid-Atlantic",
  "New York", "Northeast & Mid-Atlantic",
  "Pennsylvania", "Northeast & Mid-Atlantic",

  "Alabama", "Deep South",
  "Mississippi", "Deep South",
  "Tennessee", "Deep South",
  "Louisiana", "Deep South",

  "Texas", "Texas"
)

# Load Data ----
## Project-level data (clean full dataset)
all_projects <- aws.s3::s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") |>
  janitor::clean_names() |>
  dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year))

## Edit function to get financial info for all states
get_financial <- function(state_name) {
  
  URL <- "https://docs.google.com/spreadsheets/d/10NcgSJAZedRNDTq7_9-UJUefF6tQ6RYhvVSib_Dc-3Y/edit?usp=sharing"
  
  financial <- data.frame(read_sheet(URL, sheet="Financial Data", skip=1))
  
  financial <- financial %>%
    clean_names() %>%
    filter(!grepl("total", ignore.case=TRUE, fed_cap_grant)) %>%
    select(-notes, -assignee, -reviewed, -questions, -vlookup) %>%
    # ensure state_fiscal_year is string, arrange the dataframe by year, then set it to factor, levels in ascending order
    mutate(state_fiscal_year = as.character(state_fiscal_year)) %>%
    arrange(state_fiscal_year) %>%
    mutate(state_fiscal_year = factor(state_fiscal_year)) %>%
    # each of these are mutated individually because some rows come in as text, numerical or a list, depending on their contents
    # and mutating with across() produces errors or warnings depending on the context, here at least it is only warnings
    # that can be muted, but the resulting dataframe is still as intended
    mutate(total_fcg = convert_to_numeric(total_fcg),
           ffy21_fcg = convert_to_numeric(ffy21_fcg),
           ffy22_fcg = convert_to_numeric(ffy22_fcg),
           ffy23_fcg = convert_to_numeric(ffy23_fcg),
           ffy24_fcg = convert_to_numeric(ffy24_fcg),
           ffy25_fcg = convert_to_numeric(ffy25_fcg),
           ffy26_fcg = convert_to_numeric(ffy26_fcg),
           nonsrf_state_funds = convert_to_numeric(nonsrf_state_funds),
           arpa_funds = convert_to_numeric(arpa_funds),
           reallotments = convert_to_numeric(reallotments),
           cw_transferred_to_dw = convert_to_numeric(cw_transferred_to_dw),
           dw_transferred_to_cw = convert_to_numeric(dw_transferred_to_cw),
           unutilized_fcg = convert_to_numeric(unutilized_fcg),
           pf_carryover = convert_to_numeric(pf_carryover),
           total_carryover = convert_to_numeric(total_carryover),
           leveraged_funds = convert_to_numeric(leveraged_funds),
           funds_reserved_leveraging = convert_to_numeric(funds_reserved_leveraging),
           total_funding_available = convert_to_numeric(total_funding_available),
           total_funding_available_state_funds = convert_to_numeric(total_funding_available_state_funds),
           gpr_cost = convert_to_numeric(gpr_cost))
  
  return(financial)
}

## Funding Available (financial sheet)
financial <- get_financial()

all_funds <- financial |>
  dplyr::select(state, state_fiscal_year, total_funding_available) |>
  dplyr::group_by(state, state_fiscal_year) |>
  dplyr::summarize(total_funding_available = sum(total_funding_available, na.rm=TRUE)) |>
  dplyr::left_join(geo_cluster_lookup) |>
  dplyr::left_join(epa_region_lookup) |>
  dplyr::filter(!is.na(geo_cluster))

#channeled through SRF
all_funds_state <- financial |>
  dplyr::select(state, state_fiscal_year, total_funding_available_state_funds) |>
  dplyr::group_by(state, state_fiscal_year) |>
  dplyr::summarize(total_funding_available_state_funds = sum(total_funding_available_state_funds, na.rm=TRUE)) |>
  dplyr::left_join(geo_cluster_lookup) |>
  dplyr::left_join(epa_region_lookup) |>
  dplyr::filter(!is.na(geo_cluster))

# Demand for funds project type breakdown -----
## Separate by bar
## funding amount ----
funding_amount_df <- all_projects |>
  dplyr::select(
    state,
    state_fiscal_year,
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
  dplyr::group_by(state, state_fiscal_year, source_funding) |>
  dplyr::summarise(funding_amount=sum(selected_funding_amount, na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::filter(!funding_amount == 0) |>
  tidyr::pivot_longer(cols = "funding_amount") |>
  tidyr::pivot_wider(
    names_from = name,
    values_from = value
  ) |>
  dplyr::left_join(geo_cluster_lookup) |>
  dplyr::left_join(epa_region_lookup)

## demand ----
demand_df <- all_projects |>
  dplyr::select(
    state,
    state_fiscal_year,
    expecting_funding,
    funding_amount,
    requested_amount,
    project_cost
  ) |>
   dplyr::mutate(
    dplyr::across(c(funding_amount, requested_amount, project_cost), as.numeric),
    dplyr::across(c(funding_amount, requested_amount, project_cost), \(x) ifelse(x == 0, NA, x)),
    selected_demand = dplyr::coalesce(requested_amount, project_cost, funding_amount), 
    source_demand = dplyr::case_when(
      requested_amount > 0 ~ "requested_amount", 
      project_cost > 0 ~ "project_cost", 
      funding_amount > 0 ~ "funding_amount", 
      .default = "check"
    )
  )

demand_df_summary <- demand_df |>
  dplyr::group_by(state, state_fiscal_year, source_demand) |>
  dplyr::summarise(demand=sum(selected_demand, na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::filter(!demand == 0) |>
  tidyr::pivot_longer(cols = "demand") |>
  tidyr::pivot_wider(
    names_from = name,
    values_from = value
  ) |>
  dplyr::left_join(geo_cluster_lookup) |>
  dplyr::left_join(epa_region_lookup)

# concatenate summary dfs
# summary all -----
funding_amount_by_source <- funding_amount_df_summary |>
  dplyr::group_by(state, state_fiscal_year, source_funding, epa_region_label) |>
  dplyr::summarise(`On fundable list` = sum(funding_amount)) |>
  dplyr::ungroup()

demand_by_source <- demand_df_summary |>
  dplyr::group_by(state, state_fiscal_year, source_demand, epa_region_label) |>
  dplyr::summarise(Demand = sum(demand)) |>
  dplyr::ungroup()

# Combine into one data frame with a category column
combined_bars <- dplyr::bind_rows(
  all_funds |> 
    dplyr::transmute(epa_region_label, state, state_fiscal_year, value = total_funding_available, category = "Available Funds", alpha_var = NA_character_),
  funding_amount_by_source |> 
    dplyr::transmute(epa_region_label, state, state_fiscal_year, value = `On fundable list`, category = "On Fundable List", alpha_var = source_funding),
  demand_by_source |> 
    dplyr::transmute(epa_region_label, state, state_fiscal_year, value = Demand, category = "Demand for Funds", alpha_var = source_demand)
)

# Stacked layers (the ones with alpha)
stacked_bars <- dplyr::bind_rows(
  funding_amount_by_source |>
    dplyr::transmute(epa_region_label, state, state_fiscal_year, value = `On fundable list`, category = "On Fundable List", stack_var = source_funding),
  demand_by_source |>
    dplyr::transmute(epa_region_label, state, state_fiscal_year, value = Demand, category = "Demand for Funds", stack_var = source_demand)
)

all_fy_levels <- sort(unique(c(
  as.character(stacked_bars$state_fiscal_year),
  as.character(all_funds$state_fiscal_year)
)))

stacked_bars <- stacked_bars |>
  dplyr::mutate(state_fiscal_year = factor(state_fiscal_year, levels = all_fy_levels))

all_funds <- all_funds |>
  dplyr::mutate(state_fiscal_year = factor(state_fiscal_year, levels = all_fy_levels))


# Demand Plots by EPA Region: Available, Fundable List, Demand ----
for (region in unique(epa_region_lookup$epa_region_label)) {

# Pre-compute stacked positions within each category
stacked_positioned <- stacked_bars |>
  dplyr::filter(epa_region_label == region) |>
  dplyr::arrange(state, state_fiscal_year, category, stack_var) |>
  dplyr::group_by(state_fiscal_year, state,  category) |>
  dplyr::mutate(
    ymax = cumsum(value),
    ymin = ymax - value,
    stack_var = dplyr::case_when(
      stack_var == "requested_amount" ~ "Requested Amount",
      stack_var == "project_cost" ~ "Project Cost",
      stack_var == "funding_amount" ~ "Funding Amount", 
      stack_var == "requested_amount_ef" ~ "Requested Amount", 
      stack_var == "total_project_cost_ef" ~ "Project Cost",
      .default = "check"
    )
  ) |>
  dplyr::ungroup() |>
dplyr::mutate(
  plt_str = paste0(category, ": ", scales::label_currency()(value), " (", stack_var, ")")
) 

plot_tile_demand = "Comparing Demand for Funds to Available Funds and Funds for projects on Fundable List"

demand_p <- ggplot2::ggplot() +
  # Available Funds — left
  ggplot2::geom_rect(
    data = all_funds |>
      dplyr::ungroup() |>
      dplyr::filter(epa_region_label == region) |>
      dplyr::mutate(
        category = "Available Funds",
        plt_str = paste0(category, ": ", scales::label_currency()(total_funding_available))
      ),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) - 0.275 - 0.125,
      xmax = as.numeric(state_fiscal_year) - 0.275 + 0.125,
      ymin = 0,
      ymax = total_funding_available,
      fill = category,
      text = plt_str
    )
  )  +
  # On Fundable List — center
  ggplot2::geom_rect(
    data = dplyr::filter(stacked_positioned, category == "On Fundable List"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) - 0.125,
      xmax = as.numeric(state_fiscal_year) + 0.125,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = stack_var, text = plt_str
    )
  ) +
  # Demand for Funds — right
  ggplot2::geom_rect(
    data = dplyr::filter(stacked_positioned, category == "Demand for Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) + 0.275 - 0.125,
      xmax = as.numeric(state_fiscal_year) + 0.275 + 0.125,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = stack_var, text = plt_str
    )
  ) +
  ggplot2::scale_fill_manual(values = c(
    "Available Funds"              = "#4ea324",
    "Demand for Funds"             = "#172f60",
    "On Fundable List"             = "#791a7b",
    "Demand for Funds (Lead and EC)" = "#B15712",
    "On Fundable List (Lead and EC)" = "#ff6f00ff"
  )) +
  ggplot2::scale_alpha_manual(values = c(
    "Funding Amount" = 1,
    "Project Cost" = 0.9,
    "Requested Amount" = 0.8
  ), ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(levels(all_funds$state_fiscal_year)),
    labels = levels(all_funds$state_fiscal_year)
    ) +  
  ggplot2::scale_y_continuous(labels = label_currency()) + 

  ggplot2::facet_wrap(~state, scales = "free_y") +
  ggplot2::labs(
    x = "",
    y = "",
    fill = "",
    title = plot_tile_demand,
    subtitle = paste0("By ", region)
    ) +
    guides(alpha = "none") +
  epic_chart_theme


demand_gp <- ggplotly(demand_p, tooltip="text") |>
    layout(title = list(text = paste0(plot_tile_demand,
                                      '<br>',
                                      "By ", region
                                      )))
  

epa_region_slug <- region |> str_remove("^EPA ") |> str_to_lower() |> str_replace_all("[: -]", "_") |> str_replace_all("_+", "_")
  
  
ggsave(paste0("~/Desktop/demand_", epa_region_slug, ".jpeg"), demand_p)
  
htmlwidgets::saveWidget(partial_bundle(demand_gp) %>%
                            config(displayModeBar = FALSE) %>%
                            config(displaylogo = FALSE) %>%
                            layout(xaxis = list(fixedrange = TRUE),
                                   yaxis = list(fixedrange = TRUE))%>%
                            layout(plot_bgcolor='transparent') %>%
                            layout(paper_bgcolor='transparent'),
                          paste0("~/Desktop/demand_", epa_region_slug, ".html"))

}


# Demand Plots: Available vs Demand ----

avail_vs_demand_df <- combined_bars |>
  dplyr::filter(!category == "On Fundable List") |>
  dplyr::select(-epa_region_label) |>
  dplyr::mutate(
    alpha_var = dplyr::case_when(
      alpha_var == "requested_amount" ~ "Requested Amount",
      alpha_var == "project_cost" ~ "Project Cost",
      alpha_var == "funding_amount" ~ "Funding Amount",
      .default = alpha_var
    ),
    plt_str = paste0(category, ": ", scales::label_currency()(value), ifelse(!is.na(alpha_var), paste0(" (", alpha_var, ")"), "")) 
  )

avail_vs_demand_p <- ggplot(avail_vs_demand_df) +
  geom_col(aes(state_fiscal_year, value, group = category, fill=category, alpha = alpha_var, text = plt_str), position = "dodge") +
  facet_wrap(~state, nrow = 2) +  
  ggplot2::scale_y_continuous(labels = label_currency()) + 
  ggplot2::scale_fill_manual(values = c(
    "Available Funds"              = "#4ea324",
    "Demand for Funds"             = "#172f60",
    "On Fundable List"             = "#791a7b",
    "Demand for Funds (Lead and EC)" = "#B15712",
    "On Fundable List (Lead and EC)" = "#ff6f00ff"
  )) +
  ggplot2::scale_alpha_manual(values = c(
    "Funding Amount" = 1,
    "Project Cost" = 0.9,
    "Requested Amount" = 0.8
  ), ) +
  ggplot2::labs(
    x = "",
    y = "",
    fill = "",
    title = "Comparing Demand for Funds to Available Funds"
    ) +
  guides(alpha = "none") +
  epic_chart_theme +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

avail_vs_demand_gp <- ggplotly(avail_vs_demand, tooltip="text") |>
  layout(title = list(text = "Comparing Demand for Funds to Available Funds"))

ggsave("~/Desktop/available_v_demand.jpeg", avail_vs_demand) 
  
htmlwidgets::saveWidget(partial_bundle(avail_vs_demand_gp) %>%
                            config(displayModeBar = FALSE) %>%
                            config(displaylogo = FALSE) %>%
                            layout(xaxis = list(fixedrange = TRUE),
                                   yaxis = list(fixedrange = TRUE))%>%
                            layout(plot_bgcolor='transparent') %>%
                            layout(paper_bgcolor='transparent'),
                          paste0("~/Desktop/available_v_demand.html"))


# Demand Plots: Available (including state funds) vs Demand ----
# adds on top of available funds, a lighter green that shows any state funds available for projects 

all_avail_vs_demand_df <- combined_bars |>
  dplyr::filter(!category == "On Fundable List") |>
  dplyr::select(-epa_region_label) |>
  dplyr::mutate(
    alpha_var = dplyr::case_when(
      category == "Available Funds" ~ "DWSRF Available Funds",
      alpha_var == "requested_amount" ~ "Requested Amount",
      alpha_var == "project_cost" ~ "Project Cost",
      alpha_var == "funding_amount" ~ "Funding Amount",
      .default = alpha_var
    )
  ) |>
  dplyr::bind_rows(
    all_funds_state |> dplyr::select(-c("geo_cluster", "epa_region", "epa_region_label")) |> dplyr::rename(value = total_funding_available_state_funds) |> dplyr::mutate(category = "Available Funds", alpha_var = "Other Funds channeled through SRF")
  ) |>
  dplyr::mutate(
    plt_str = paste0(category, ": ", scales::label_currency()(value), paste0(" (", alpha_var, ")")) 
  ) |>
  dplyr::arrange(state, state_fiscal_year, category, alpha_var) |>
  dplyr::mutate(
    alpha_var = forcats::fct(alpha_var, levels = c("DWSRF Available Funds", "Other Funds channeled through SRF", "Funding Amount", "Project Cost","Requested Amount"))
  )  |>
  dplyr::mutate(state_fiscal_year = factor(state_fiscal_year, levels = all_fy_levels))

# Pre-compute stacked positions within each category
stacked_positioned_all_avail_vs_demand_df <- all_avail_vs_demand_df |>
  dplyr::group_by(state_fiscal_year, state,  category) |>
  dplyr::mutate(
    ymax = cumsum(value),
    ymin = ymax - value
  ) |>
  dplyr::ungroup() 

offset <- 0.2   
half_width <- 0.2

all_avail_vs_demand_p <- ggplot2::ggplot() +
  # Available Funds — left
  ggplot2::geom_rect(
    data = dplyr::filter(stacked_positioned_all_avail_vs_demand_df, category == "Available Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) - offset - half_width,
      xmax = as.numeric(state_fiscal_year) - offset + half_width,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = alpha_var, text = plt_str
    )
  )  +
  # Demand for Funds — right
  ggplot2::geom_rect(
    data = dplyr::filter(stacked_positioned_all_avail_vs_demand_df, category == "Demand for Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) + offset - half_width,
      xmax = as.numeric(state_fiscal_year) + offset + half_width,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = alpha_var, text = plt_str
    )
  ) +
  ggplot2::scale_fill_manual(values = c(
    "Available Funds"              = "#4ea324",
    "Demand for Funds"             = "#172f60",
    "On Fundable List"             = "#791a7b",
    "Demand for Funds (Lead and EC)" = "#B15712",
    "On Fundable List (Lead and EC)" = "#ff6f00ff"
  )) +
  ggplot2::scale_alpha_manual(values = c(
    "DWSRF Available Funds" = 1,
    "Other Funds channeled through SRF" = 0.5,
    "Funding Amount" = 1,
    "Project Cost" = 0.9,
    "Requested Amount" = 0.8
  ), ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(levels(all_funds$state_fiscal_year)),
    labels = levels(all_funds$state_fiscal_year)
    )  +
  ggplot2::scale_y_continuous(labels = label_currency()) +
  ggplot2::facet_wrap(~state, nrow = 2) +
  ggplot2::labs(
    x = "",
    y = "",
    fill = "",
    title = "Comparing Demand for Funds to Available Funds"
    ) +
    guides(alpha = "none") +
  epic_chart_theme +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) 


all_avail_vs_demand_gp <- ggplotly(all_avail_vs_demand_p, tooltip="text") |>
  layout(title = list(text = "Comparing Demand for Funds to Available Funds"))

ggsave("~/Desktop/all_available_v_demand.jpeg", all_avail_vs_demand_p) 
  
htmlwidgets::saveWidget(partial_bundle(all_avail_vs_demand_gp) %>%
                            config(displayModeBar = FALSE) %>%
                            config(displaylogo = FALSE) %>%
                            layout(xaxis = list(fixedrange = TRUE),
                                   yaxis = list(fixedrange = TRUE))%>%
                            layout(plot_bgcolor='transparent') %>%
                            layout(paper_bgcolor='transparent'),
                          paste0("~/Desktop/all_available_v_demand.html"))


# Unmet demand Plot (the difference between demand and available funds) ----

all_unmet_demand <- all_avail_vs_demand_df |>
  dplyr::group_by(state, state_fiscal_year, category) |>
  dplyr::summarise(total = sum(value, na.rm = TRUE)) |>
  tidyr::pivot_wider(names_from = "category", values_from = "total") |>
  dplyr::ungroup() |>
  dplyr::mutate(`Unmet Demand` = `Demand for Funds` -`Available Funds`) |>
  dplyr::filter(!`Unmet Demand` < 0 | is.na(`Unmet Demand`)) |>
  tidyr::pivot_longer(cols = c(`Available Funds`, `Demand for Funds`, `Unmet Demand`)) |>
  dplyr::filter(!name == "Demand for Funds") |>
  dplyr::mutate(
    plt_str = paste0(name, ": ", scales::label_currency()(value))
  )


unmet_demand_p <- ggplot(all_unmet_demand) +
  geom_col(aes(state_fiscal_year, value, fill = name, group= name, text = plt_str), position = "dodge") +
  facet_wrap(~state, nrow = 2) +
  ggplot2::scale_y_continuous(labels = label_currency()) +
  ggplot2::scale_fill_manual(values = comp_colors) +
  ggplot2::facet_wrap(~state, nrow = 2) +
  ggplot2::labs(
    x = "",
    y = "",
    fill = "",
    title = "Comparing Unmet Demand to Available Funds"
    ) +
    guides(alpha = "none") +
  epic_chart_theme +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) 

unmet_demand_gp <- ggplotly(unmet_demand_p, tooltip="text") |>
  layout(title = list(text = "Comparing Unmet Demand to Available Funds"))

ggsave("~/Desktop/unmet_demand.jpeg", unmet_demand_p) 
  
htmlwidgets::saveWidget(partial_bundle(unmet_demand_gp) %>%
                            config(displayModeBar = FALSE) %>%
                            config(displaylogo = FALSE) %>%
                            layout(xaxis = list(fixedrange = TRUE),
                                   yaxis = list(fixedrange = TRUE))%>%
                            layout(plot_bgcolor='transparent') %>%
                            layout(paper_bgcolor='transparent'),
                          paste0("~/Desktop/unmet_demand.html"))


# Demand Plots: Available (including state funds) vs Demand (version 2)----
# adds on top of available funds, a lighter green that shows any state funds available for projects 
custom_labeller <- function(labels) {
  labels["row_group"] <- NULL
  labels
}


all_avail_vs_demand_df_v2 <- combined_bars |>
  dplyr::ungroup() |>
  dplyr::filter(!category == "On Fundable List") |>
  dplyr::select(-epa_region_label) |>
  dplyr::mutate(
    alpha_var = dplyr::case_when(
      category == "Available Funds" ~ "DWSRF Available Funds",
      alpha_var == "requested_amount" ~ "Requested Amount",
      alpha_var == "project_cost" ~ "Project Cost",
      alpha_var == "funding_amount" ~ "Funding Amount",
      .default = alpha_var
    )
  ) |>
  dplyr::bind_rows(
    all_funds_state |> dplyr::select(-c("geo_cluster", "epa_region", "epa_region_label")) |> dplyr::rename(value = total_funding_available_state_funds) |> dplyr::mutate(category = "Available Funds", alpha_var = "Other Funds channeled through SRF")
  ) |>
  dplyr::mutate(
    plt_str = paste0(category, ": ", scales::label_currency()(value), paste0(" (", alpha_var, ")")) 
  ) |>
  dplyr::arrange(state, state_fiscal_year, category, alpha_var) |>
  dplyr::mutate(
    alpha_var = forcats::fct(alpha_var, levels = c("DWSRF Available Funds", "Other Funds channeled through SRF", "Funding Amount", "Project Cost","Requested Amount"))
  )  |>
  dplyr::mutate(state_fiscal_year = factor(state_fiscal_year, levels = all_fy_levels)) |>
  dplyr::filter(!state %in% c("Alabama", "Pennsylvania", "Ohio"))  |>
  dplyr::filter(!state_fiscal_year %in% c("2021", "2027"))  |>
  droplevels() |>
  dplyr::mutate(
    row_group = dplyr::case_when(
      state %in% c("Illinois", "New York", "Texas", "New Jersey") ~ "g1",
      state %in% c("Tennessee", "Mississippi", "Maryland", "Louisiana") ~ "g3",
      .default = "g2"
    )
  )

# readr::write_csv(all_avail_vs_demand_df_v2, "demand_raw_data.csv")
# googlesheets4::write_sheet(
#   data = all_avail_vs_demand_df_v2,
#   ss = "https://docs.google.com/spreadsheets/d/1VFgXRef9ByadRve_mRYYBUcRVuxxjsBHdzS1YMkxVys",
#   sheet = "raw_all_avail_vs_demand"
# )

all_avail_vs_demand_df_v2_wide <- all_avail_vs_demand_df_v2 |>
  tidyr::pivot_wider(names_from = "category", values_from = "value") |>
  dplyr::group_by(state, state_fiscal_year) |>
  dplyr::summarise(
    `Total Available Funds` = sum(`Available Funds`, na.rm = TRUE),
    `Total Demand for Funds` = sum( `Demand for Funds`, na.rm = TRUE)
  ) |>
  dplyr::mutate(`Demand to Available Funds Ratio` =  `Total Demand for Funds`/`Total Available Funds`)

# googlesheets4::write_sheet(
#   data = all_avail_vs_demand_df_v2_wide,
#   ss = "https://docs.google.com/spreadsheets/d/1VFgXRef9ByadRve_mRYYBUcRVuxxjsBHdzS1YMkxVys",
#   sheet = "all_avail_vs_demand_summary"
# )

# Pre-compute stacked positions within each category
stacked_positioned_all_avail_vs_demand_df_v2 <- all_avail_vs_demand_df_v2 |>
  dplyr::group_by(state_fiscal_year, state,  category) |>
  dplyr::mutate(
    ymax = cumsum(value),
    ymin = ymax - value
  ) |>
  dplyr::ungroup() 

offset <- 0.2   
half_width <- 0.2

all_avail_vs_demand_p_v2 <- ggplot2::ggplot() +
  # Available Funds — left
  ggplot2::geom_rect(
    data = dplyr::filter(stacked_positioned_all_avail_vs_demand_df_v2, category == "Available Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) - offset - half_width,
      xmax = as.numeric(state_fiscal_year) - offset + half_width,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = alpha_var, text = plt_str
    )
  )  +
  # Demand for Funds — right
  ggplot2::geom_rect(
    data = dplyr::filter(stacked_positioned_all_avail_vs_demand_df_v2, category == "Demand for Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) + offset - half_width,
      xmax = as.numeric(state_fiscal_year) + offset + half_width,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = alpha_var, text = plt_str
    )
  ) +
  ggplot2::scale_fill_manual(values = c(
    "Available Funds"              = "#4ea324",
    "Demand for Funds"             = "#172f60",
    "On Fundable List"             = "#791a7b",
    "Demand for Funds (Lead and EC)" = "#B15712",
    "On Fundable List (Lead and EC)" = "#ff6f00ff"
  )) +
  ggplot2::scale_alpha_manual(values = c(
    "DWSRF Available Funds" = 1,
    "Other Funds channeled through SRF" = 0.5,
    "Funding Amount" = 1,
    "Project Cost" = 0.9,
    "Requested Amount" = 0.8
  ), ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(levels(stacked_positioned_all_avail_vs_demand_df_v2$state_fiscal_year)),
    labels = c("'22", "'23", "'24", "'25", "'26")
    )  +
  ggplot2::scale_y_continuous(labels = label_currency()) +
  ggplot2::facet_wrap(
    row_group ~ state, 
    nrow = 3,
    labeller = custom_labeller
) +
  
  ggplot2::labs(
    x = "State Fiscal Year",
    y = "",
    fill = "",
    title = "Comparing Demand for Funds to Available Funds"
    ) +
    guides(alpha = "none") +
  epic_chart_theme 

all_avail_vs_demand_gp_v2 <- ggplotly(all_avail_vs_demand_p_v2, tooltip="text") |>
  layout(title = list(text = "Comparing Demand for Funds to Available Funds"))

all_avail_vs_demand_gp_v2$x$layout$annotations <- lapply(all_avail_vs_demand_gp_v2$x$layout$annotations, function(ann) {
  ann$text <- paste0("<b>", ann$text, "</b>")
  ann$font$size <- 14
  ann
})

ggsave("~/Desktop/all_available_v_demand_v2.jpeg", all_avail_vs_demand_p_v2) 
  
htmlwidgets::saveWidget(partial_bundle(all_avail_vs_demand_gp_v2) %>%
                            config(displayModeBar = FALSE) %>%
                            config(displaylogo = FALSE) %>%
                            layout(xaxis = list(fixedrange = TRUE),
                                   yaxis = list(fixedrange = TRUE))%>%
                            layout(plot_bgcolor='transparent') %>%
                            layout(paper_bgcolor='transparent'),
                          paste0("~/Desktop/all_available_v_demand_v2.html"))


# Demand Plots: Available (including state funds) vs Demand (by project type)----

funding_available_by_type <- get_financial() |>
  dplyr::select(
    state,
    state_fiscal_year,
    fed_cap_grant,
    total_funding_available,
    total_funding_available_state_funds
  ) |>
  dplyr::filter(
    !state %in% c("Arkansas", "Alabama", "Pennsylvania", "Ohio") & !state_fiscal_year %in% c("2021", "2027")
  ) |>
  #states with challenging  interpretation due to combined funding
  dplyr::filter(
    !state %in% c("Louisiana",  "Maryland", "New Jersey")
  ) |>
  dplyr::mutate(
    project_type = dplyr::case_when(
      fed_cap_grant %in% c("Base", "IIJA Gen Supp", "Base & IIJA Gen Supp", "Base and IIJA Gen Supp") ~ "General",
      fed_cap_grant %in% c("LSLR", "LSLR_FFY22", "LSLR_FFY23") ~ "Lead",
      fed_cap_grant %in% c("EC", "EC_FFY22", "EC_FFY23") ~ "Emerging Contaminants",
      .default =  paste0("Combined: ", fed_cap_grant)
    )
  )

funding_available_by_type_df <- funding_available_by_type |>
  tidyr::pivot_longer(cols = c("total_funding_available", "total_funding_available_state_funds")) |>
  dplyr::group_by(state, state_fiscal_year, project_type, name) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE)) |>
  dplyr::mutate(
    category = "Available Funds",
    stack_var = dplyr::case_when(
      name == "total_funding_available" ~ "DWSRF Available Funds",
      name == "total_funding_available_state_funds" ~ "Other Funds channeled through SRF",
    )
  )

  
demand_by_type <-  all_projects |>
   dplyr::filter(
    !state %in% c("Alabama", "Pennsylvania", "Ohio") & !state_fiscal_year %in% c("2021", "2027")
  ) |>
  dplyr::filter(
    !state %in% c("Louisiana",  "Maryland", "New Jersey")
  ) |>
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
    selected_demand = dplyr::coalesce(requested_amount, project_cost, funding_amount), 
    source_demand = dplyr::case_when(
      requested_amount > 0 ~ "requested_amount", 
      project_cost > 0 ~ "project_cost", 
      funding_amount > 0 ~ "funding_amount", 
      .default = "check"
    )
  ) 



demand_by_type_df <- demand_by_type |>
  dplyr::group_by(state, state_fiscal_year, project_type, source_demand) |>
  dplyr::summarise(value = sum(selected_demand, na.rm = TRUE)) |>
  dplyr::mutate(
    category = "Demand for Funds",
    stack_var = dplyr::case_when(
      source_demand == "requested_amount" ~ "Requested Amount",
      source_demand == "project_cost" ~ "Project Cost",
      source_demand == "funding_amount" ~ "Funding Amount",
    )
  )

combined_by_type <- funding_available_by_type_df |> dplyr::select(-name) |>
  bind_rows(demand_by_type_df |> dplyr::select(-source_demand)) |>
  dplyr::filter(value > 0) |>
  dplyr::mutate(
    plt_str = paste0(category, ": ", scales::label_currency()(value), " (", stack_var, ")" )
  )|>
  dplyr::filter(
    !(state == "Minnesota" & project_type %in% c("General", "Emerging Contaminants"))
  ) |>
  dplyr::mutate(
    row_group = dplyr::case_when(
      state %in% c("Illinois", "New York", "Texas") ~ "g1",
      state %in% c("Tennessee", "Mississippi") ~ "g3",
      .default = "g2"
    )
  )

# googlesheets4::write_sheet(
#   data = combined_by_type,
#   ss = "https://docs.google.com/spreadsheets/d/1VFgXRef9ByadRve_mRYYBUcRVuxxjsBHdzS1YMkxVys",
#   sheet = "combined_by_type"
# )


combined_by_type_wide <- combined_by_type |>
  dplyr::group_by(state, state_fiscal_year, project_type, category) |>
  dplyr::summarise(
    total = sum(value, na.rm = TRUE)
  ) |>
  tidyr::pivot_wider(names_from = c("category", "project_type"), values_from = "total")

# googlesheets4::write_sheet(
#   data = combined_by_type_wide,
#   ss = "https://docs.google.com/spreadsheets/d/1VFgXRef9ByadRve_mRYYBUcRVuxxjsBHdzS1YMkxVys",
#   sheet = "combined_by_type_summary"
# )

# General plot -----
## Pre-compute stacked positions within each category
combined_general <- combined_by_type |>
  dplyr::filter(project_type == "General") |>
  dplyr::group_by(state_fiscal_year, state,  category) |>
  dplyr::mutate(
    ymax = cumsum(value),
    ymin = ymax - value
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(state, state_fiscal_year, category) |>
  dplyr::mutate(
    state_fiscal_year = forcats::fct(state_fiscal_year)
  )


demand_general_p <- ggplot2::ggplot() +
  # Available Funds — left
  ggplot2::geom_rect(
    data = dplyr::filter(combined_general, category == "Available Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) - offset - half_width,
      xmax = as.numeric(state_fiscal_year) - offset + half_width,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = stack_var, text = plt_str
    )
  )  +
  # Demand for Funds — right
  ggplot2::geom_rect(
    data = dplyr::filter(combined_general, category == "Demand for Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) + offset - half_width,
      xmax = as.numeric(state_fiscal_year) + offset + half_width,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = stack_var, text = plt_str
    )
  ) +
  ggplot2::scale_fill_manual(values = c(
    "Available Funds"              = "#4ea324",
    "Demand for Funds"             = "#172f60",
    "On Fundable List"             = "#791a7b",
    "Demand for Funds (Lead and EC)" = "#B15712",
    "On Fundable List (Lead and EC)" = "#ff6f00ff"
  )) +
  ggplot2::scale_alpha_manual(values = c(
    "DWSRF Available Funds" = 1,
    "Other Funds channeled through SRF" = 0.5,
    "Funding Amount" = 1,
    "Project Cost" = 0.9,
    "Requested Amount" = 0.8
  ), ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(levels(combined_general$state_fiscal_year)),
    labels = c("'22", "'23", "'24", "'25", "'26")
    )  +
  ggplot2::scale_y_continuous(labels = label_currency()) +
  ggplot2::facet_wrap(
    row_group ~ state, 
    nrow = 3,
    labeller = custom_labeller
) +
  ggplot2::labs(
    x = "State Fiscal Year",
    y = "",
    fill = "",
    title = "Comparing Demand for Funds to Available Funds for General Projects"
    ) +
    guides(alpha = "none") +
  epic_chart_theme 

demand_general_gp <- ggplotly(demand_general_p, tooltip="text") |>
  layout(title = list(text = "Comparing Demand for Funds to Available Funds for General Projects"))

demand_general_gp$x$layout$annotations <- lapply(demand_general_gp$x$layout$annotations, function(ann) {
  ann$text <- paste0("<b>", ann$text, "</b>")
  ann$font$size <- 14
  ann
})

ggsave("~/Desktop/demand_general.jpeg", demand_general_p) 
  
htmlwidgets::saveWidget(partial_bundle(demand_general_gp) %>%
                            config(displayModeBar = FALSE) %>%
                            config(displaylogo = FALSE) %>%
                            layout(xaxis = list(fixedrange = TRUE),
                                   yaxis = list(fixedrange = TRUE))%>%
                            layout(plot_bgcolor='transparent') %>%
                            layout(paper_bgcolor='transparent'),
                          paste0("~/Desktop/demand_general.html"))

# Lead plot -----
## Pre-compute stacked positions within each category
combined_lead <- combined_by_type |>
  dplyr::filter(project_type == "Lead") |>
  dplyr::group_by(state_fiscal_year, state,  category) |>
  dplyr::mutate(
    ymax = cumsum(value),
    ymin = ymax - value
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(state, state_fiscal_year, category) |>
  dplyr::mutate(
    state_fiscal_year = forcats::fct(state_fiscal_year)
  )


demand_lead_p <- ggplot2::ggplot() +
  # Available Funds — left
  ggplot2::geom_rect(
    data = dplyr::filter(combined_lead, category == "Available Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) - offset - half_width,
      xmax = as.numeric(state_fiscal_year) - offset + half_width,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = stack_var, text = plt_str
    )
  )  +
  # Demand for Funds — right
  ggplot2::geom_rect(
    data = dplyr::filter(combined_lead, category == "Demand for Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) + offset - half_width,
      xmax = as.numeric(state_fiscal_year) + offset + half_width,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = stack_var, text = plt_str
    )
  ) +
  ggplot2::scale_fill_manual(values = c(
    "Available Funds"              = "#4ea324",
    "Demand for Funds"             = "#172f60",
    "On Fundable List"             = "#791a7b",
    "Demand for Funds (Lead and EC)" = "#B15712",
    "On Fundable List (Lead and EC)" = "#ff6f00ff"
  )) +
  ggplot2::scale_alpha_manual(values = c(
    "DWSRF Available Funds" = 1,
    "Other Funds channeled through SRF" = 0.5,
    "Funding Amount" = 1,
    "Project Cost" = 0.9,
    "Requested Amount" = 0.8
  ), ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(levels(combined_lead$state_fiscal_year)),
    labels = c("'22", "'23", "'24", "'25", "'26")
    )  +
  ggplot2::scale_y_continuous(labels = label_currency()) +
  ggplot2::facet_wrap(
    row_group ~ state, 
    nrow = 3,
    labeller = custom_labeller
) +
  ggplot2::labs(
    x = "State Fiscal Year",
    y = "",
    fill = "",
    title = "Comparing Demand for Funds to Available Funds for Lead Projects"
    ) +
    guides(alpha = "none") +
  epic_chart_theme 

demand_lead_gp <- ggplotly(demand_lead_p, tooltip="text") |>
  layout(title = list(text = "Comparing Demand for Funds to Available Funds for Lead Projects"))

demand_lead_gp$x$layout$annotations <- lapply(demand_lead_gp$x$layout$annotations, function(ann) {
  ann$text <- paste0("<b>", ann$text, "</b>")
  ann$font$size <- 14
  ann
})

ggsave("~/Desktop/demand_lead.jpeg", demand_lead_p) 
  
htmlwidgets::saveWidget(partial_bundle(demand_lead_gp) %>%
                            config(displayModeBar = FALSE) %>%
                            config(displaylogo = FALSE) %>%
                            layout(xaxis = list(fixedrange = TRUE),
                                   yaxis = list(fixedrange = TRUE))%>%
                            layout(plot_bgcolor='transparent') %>%
                            layout(paper_bgcolor='transparent'),
                          paste0("~/Desktop/demand_lead.html"))


# EC plot -----
## Pre-compute stacked positions within each category
combined_ec <- combined_by_type |>
  dplyr::filter(project_type == "Emerging Contaminants") |>
  dplyr::group_by(state_fiscal_year, state,  category) |>
  dplyr::mutate(
    ymax = cumsum(value),
    ymin = ymax - value
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(state, state_fiscal_year, category) |>
  dplyr::mutate(
    state_fiscal_year = forcats::fct(state_fiscal_year)
  )


demand_ec_p <- ggplot2::ggplot() +
  # Available Funds — left
  ggplot2::geom_rect(
    data = dplyr::filter(combined_ec, category == "Available Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) - offset - half_width,
      xmax = as.numeric(state_fiscal_year) - offset + half_width,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = stack_var, text = plt_str
    )
  )  +
  # Demand for Funds — right
  ggplot2::geom_rect(
    data = dplyr::filter(combined_ec, category == "Demand for Funds"),
    ggplot2::aes(
      xmin = as.numeric(state_fiscal_year) + offset - half_width,
      xmax = as.numeric(state_fiscal_year) + offset + half_width,
      ymin = ymin, ymax = ymax,
      fill = category, alpha = stack_var, text = plt_str
    )
  ) +
  ggplot2::scale_fill_manual(values = c(
    "Available Funds"              = "#4ea324",
    "Demand for Funds"             = "#172f60",
    "On Fundable List"             = "#791a7b",
    "Demand for Funds (Lead and EC)" = "#B15712",
    "On Fundable List (Lead and EC)" = "#ff6f00ff"
  )) +
  ggplot2::scale_alpha_manual(values = c(
    "DWSRF Available Funds" = 1,
    "Other Funds channeled through SRF" = 0.5,
    "Funding Amount" = 1,
    "Project Cost" = 0.9,
    "Requested Amount" = 0.8
  ), ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(levels(combined_general$state_fiscal_year)),
    labels = c("'22", "'23", "'24", "'25", "'26")
    )  +
  ggplot2::scale_y_continuous(labels = label_currency()) +
  ggplot2::facet_wrap(
    row_group ~ state, 
    nrow = 3,
    labeller = custom_labeller
) +
  ggplot2::labs(
    x = "State Fiscal Year",
    y = "",
    fill = "",
    title = "Comparing Demand for Funds to Available Funds for Emerging Contaminants Projects"
    ) +
    guides(alpha = "none") +
  epic_chart_theme +
  theme(strip.text = element_text(face = "bold"))

demand_ec_gp <- ggplotly(demand_ec_p, tooltip="text") |>
  layout(title = list(text = "Comparing Demand for Funds to Available Funds for Emerging Contaminants Projects")) 

# |>
#   layout(hovermode = "y unified")

demand_ec_gp$x$layout$annotations <- lapply(demand_ec_gp$x$layout$annotations, function(ann) {
  ann$text <- paste0("<b>", ann$text, "</b>")
  ann$font$size <- 14
  ann
})

ggsave("~/Desktop/demand_ec.jpeg", demand_ec_p) 
  
htmlwidgets::saveWidget(partial_bundle(demand_ec_gp) %>%
                            config(displayModeBar = FALSE) %>%
                            config(displaylogo = FALSE) %>%
                            layout(xaxis = list(fixedrange = TRUE),
                                   yaxis = list(fixedrange = TRUE))%>%
                            layout(plot_bgcolor='transparent') %>%
                            layout(paper_bgcolor='transparent'),
                          paste0("~/Desktop/demand_ec.html"))




## Summaries ----
overall_ratio_summaries <- all_avail_vs_demand_df_v2 |>
  dplyr::group_by(state, ) |>
  dplyr::summarise(
    `SUM of Total Available Funds` = sum(value * (category == "Available Funds")),
    `SUM of DWSRF Available Funds` = sum(value * (category == "Available Funds" & alpha_var == "DWSRF Available Funds")), 
    `SUM of Demand` = sum(value * (category == "Demand for Funds"))
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    `Demand to Available Funds Ratio (DWSRF + Other State and Federal Funds)` = `SUM of Demand`/`SUM of Total Available Funds`,	
    `Demand to Available Funds Ratio (Only DWSRF Funds)` = `SUM of Demand`/`SUM of DWSRF Available Funds`,
    `% Reduction in Demand-to-Available Funds Ratio when Other State and Federal funds are Utilized` = 100*(`Demand to Available Funds Ratio (Only DWSRF Funds)` - `Demand to Available Funds Ratio (DWSRF + Other State and Federal Funds)`)/`Demand to Available Funds Ratio (Only DWSRF Funds)`
  ) 


# googlesheets4::write_sheet(
#   data = overall_ratio_summaries,
#   ss = "https://docs.google.com/spreadsheets/d/1VFgXRef9ByadRve_mRYYBUcRVuxxjsBHdzS1YMkxVys",
#   sheet = "overall_ratio_summaries"
# )

ratio_summaries <- combined_by_type |>
  dplyr::ungroup() |>
  dplyr::group_by(state, project_type) |>
  dplyr::summarise(
    `SUM of Total Available Funds` = sum(value * (category == "Available Funds")),
    `SUM of DWSRF Available Funds` = sum(value * (category == "Available Funds" & stack_var == "DWSRF Available Funds")), 
    `SUM of Demand` = sum(value * (category == "Demand for Funds"))
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    `Demand to Available Funds Ratio (DWSRF + Other State and Federal Funds)` = `SUM of Demand`/`SUM of Total Available Funds`,	
    `Demand to Available Funds Ratio (Only DWSRF Funds)` = `SUM of Demand`/`SUM of DWSRF Available Funds`,
    `% Reduction in Demand-to-Available Funds Ratio when Other State and Federal funds are Utilized` = 100*(`Demand to Available Funds Ratio (Only DWSRF Funds)` - `Demand to Available Funds Ratio (DWSRF + Other State and Federal Funds)`)/`Demand to Available Funds Ratio (Only DWSRF Funds)`
  ) |>
  dplyr::filter(
    project_type %in% c("General", "Lead", "Emerging Contaminants")
  ) |>
  dplyr::arrange(project_type) 
  


# googlesheets4::write_sheet(
#   data = ratio_summaries,
#   ss = "https://docs.google.com/spreadsheets/d/1VFgXRef9ByadRve_mRYYBUcRVuxxjsBHdzS1YMkxVys",
#   sheet = "ratio_summaries"
# )

