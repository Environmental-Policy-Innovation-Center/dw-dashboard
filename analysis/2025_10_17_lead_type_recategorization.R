# Source styling functions/palettes
library(here)
source(here("resources", "inputs.R"))
source(here("resources", "cleaning.R"))
source(here("resources", "styling.R"))


# import project-level data
all_projects <- aws.s3::s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") |>
  janitor::clean_names()

#search strings
lsli_str
lslr_str

#devtools::install_github("davidsjoberg/ggsankey")

# Create both OLD and NEW categorizations
df_comparison <- all_projects |>
  dplyr::filter(project_type == "Lead") |>
  dplyr::mutate(
    # OLD categorization (without "both")
    lead_type_old = dplyr::case_when(
      stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
      stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
      stringr::str_detect(tolower(project_description), "replacement") & 
        stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
      stringr::str_detect(tolower(project_description), "inventory") & 
        stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
      TRUE ~ "unknown"
    ),
    
    # NEW categorization (with "both")
    lead_type_new = dplyr::case_when(
      stringr::str_detect(tolower(project_description), lsli_str) & 
        stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
      stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
      stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
      stringr::str_detect(tolower(project_description), "replacement") & 
        stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
      stringr::str_detect(tolower(project_description), "inventory") & 
        stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
      TRUE ~ "unknown"
    )
  )

#readr::write_csv(df_comparison, "~/Desktop/lead_type_comparison.csv")

# Create summary for Sankey diagram
sankey_data <- df_comparison |>
  dplyr::count(lead_type_old, lead_type_new) |>
  dplyr::rename(source = lead_type_old, 
         target = lead_type_new, 
         value = n)

# Print summary statistics
cat("=== Categorization Change Summary ===\n")
print(sankey_data)

cat("\n=== New 'both' category breakdown ===\n")
both_breakdown <- df_comparison |>
  dplyr::filter(lead_type_new == "both") |>
  dplyr::count(lead_type_old) |>
  dplyr::mutate(percentage = n / sum(n) * 100)
print(both_breakdown)
#All the new "Both" categories came from LSLI projects

# Using ggsankey (ggplot2-based)
sankey_long <- df_comparison |>
  ggsankey::make_long(lead_type_old, lead_type_new)

ggplot2::ggplot(sankey_long, aes(x = x, 
                        next_x = next_x, 
                        node = node, 
                        next_node = next_node,
                        fill = factor(node),
                        label = node)) +
  ggsankey::geom_sankey(flow.alpha = 0.5, node.color = "gray30") +
  ggsankey::geom_sankey_label(size = 3.5, color = "white", fill = "gray40") +
  ggplot2::scale_fill_viridis_d(option = "plasma") +
  ggsankey::theme_sankey(base_size = 16) +
  labs(title = "Lead Type Categorization: Old vs New",
       x = NULL) +
  epic_chart_theme +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank())

# Additional quantification metrics
cat("\n=== Impact Metrics ===\n")
cat("Total observations:", nrow(df_comparison), "\n")
cat("Observations now categorized as 'both':", 
    sum(df_comparison$lead_type_new == "both"), "\n")
cat("Percentage of total:", 
    round(sum(df_comparison$lead_type_new == "both") / nrow(df_comparison) * 100, 2), "%\n")

# Show which old categories contributed to 'both'
cat("\nOld category -> 'both' transitions:\n")
df_comparison |>
  filter(lead_type_new == "both") |>
  count(lead_type_old, sort = TRUE) |>
  print()


# now lead type factor variable with levels: both, lslr, lsli, and unknown
# manually inspect "both" designations and capture exceptions in the code