state_name <- "Tennessee"
state_abbr <- "TN"
save_plots <- TRUE

# Load all projects
all_projects <- aws.s3::s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") |>
  janitor::clean_names() |>
  dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year))

# Filter projects on fundable list and produce summaries
sc_ef_project_type_summary <- all_projects |>
  dplyr::filter(
    state == state_name,
    expecting_funding == "Yes"
  ) |>
  dplyr::mutate(
    small_community = case_when(
    population == "No Information" ~ "No Information",
    is.na(population) ~ "No Information", 
    convert_to_numeric(population, TRUE) > 0 & convert_to_numeric(population, TRUE) <= 10000 ~ "Yes",
    TRUE ~ "No"
  )
) |>
  dplyr::group_by(state_fiscal_year, small_community, project_type) |>
  dplyr::summarise(
    counts = n()
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    project_type = ifelse(project_type == "Emerging Contaminants", "Emerging Contaminants (EC)", project_type),
    project_type = factor(project_type, levels=c("Both Lead and EC","Emerging Contaminants (EC)", "Lead", "General"))
  ) |>
  dplyr::group_by(state_fiscal_year, project_type) |>
  dplyr::mutate(
    total = sum(counts)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    perc = 100*(round(counts/total, 2)),
    plot_str = paste0(small_community, ": ", counts, " projects (", perc, "%)")
  )

# Loop over project_type for plot per level
for (type_cat in levels(sc_ef_project_type_summary$project_type)) {

  cat("Processing ", type_cat, "...\n")

  plot_title <- paste0("How many ", type_cat, " projects on the fundable list serve small communities?")
  

  # filter data frame per project type
  type_cat_df <- sc_ef_project_type_summary |> dplyr::filter(project_type == type_cat) 

  if(nrow(type_cat_df) == 0) {
    cat("Skipping ", type_cat, "...\n")
    cat("Empty dataframe \n")
    
    next
  }
  
  cat("Building plot...\n")
  
  sc_ef_p <- type_cat_df |>
    ggplot() +
    geom_col(
      aes(state_fiscal_year, counts, fill= small_community, text = plot_str), 
      position = "stack"
    ) +
    labs(
      x = "State Fiscal Year",
      y = "",
      fill = "Small Community",
      title = plot_title,
      subtitle = get_subtitle_str(sc_ef_project_type_summary$state_fiscal_year, state_name)
    ) +
    scale_fill_manual(values = ef_dac_sc_colors) + 
    epic_chart_theme


sc_ef_gp <- ggplotly(sc_ef_p, tooltip="text") %>%
    layout(title = list(text = paste0(plot_title,
                                      '<br>',
                                      get_subtitle_str(sc_ef_project_type_summary$state_fiscal_year, state_name)
                                      )))
  
  if (save_plots) {  
  cat("Saving ", type_cat, "plot ...\n")  
    run_save_plots(
      gg_plot = sc_ef_p,
      gp_object = sc_ef_gp, 
      name = paste0("expecting-funding-serving-small-communities-", stringr::str_replace_all(stringr::str_to_lower(type_cat), " ", "-"), "-yoy") 
    )
  }

  }

