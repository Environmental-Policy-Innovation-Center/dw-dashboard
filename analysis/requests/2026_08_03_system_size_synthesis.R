# Small Systems Synthesis Report Analysis

# Author: Maria Sevillano
# Date: 2026-08-01

# Scope ===========================================
# SFY: 2022-2025

# Focus States: "AL", "IL", "IN", "LA", "MD", "MI", "MN", "MS", "NJ", "NY", "OH", "PA", "TN", "TX", "WI"
# Michigan does not have PWSID or population info, cannot perform this analysis
# Complete cases for system size (either from FT data or imputed from PWSID)

# System size distribution
# Very Small Systems (VSS) = 0 - 3,300
# Small Systems (SS) = 3,301 - 10,000
# Larger Systems (LS) (non small) = >10,000


# Load libraries ===========================================
library(tidyverse)
library(ggplot2)
library(gt)

# Helper functions ===========================================
# Convert string columns to numeric ----
convert_to_numeric <- function(column, fill_na_0 = FALSE) {
  
  column_numeric <- as.numeric(stringr::str_replace_all(column, "[^0-9.-]", ""))
  
  if (fill_na_0 == TRUE) {
    column_numeric <- tidyr::replace_na(column_numeric, 0)
  }
  
  return(column_numeric)
}


# Save CSV locally and to AWS ----
save_csv_to_s3 <- function(df, filename, bucket) {
  readr::write_csv(df, file.path(paste0("output/", filename, ".csv")))
  
  aws.s3::put_object(
    file   = file.path(paste0("output/", filename, ".csv")),
    acl    = "public-read",
    object = paste0("clean_data/funding-tracker/system_size_synthesis_report/", filename, ".csv"),
    bucket = aws_bucket
  )
}

# Save HTML table (gt table) locally and to AWS ----
save_html_table_to_s3 <- function(gt_table, filename, bucket) {
  gt::gtsave(gt_table, filename = paste0(filename, ".html"), path = "output/" )

  aws.s3::put_object(
    file   = file.path(paste0("output/", filename, ".html")),
    acl    = "public-read",
    object = paste0("clean_data/funding-tracker/system_size_synthesis_report/", filename, ".html"),
    bucket = aws_bucket,
    headers = list(`Content-Type` = "text/html")
  )
}

# Save plotly locally and to AWS ----
save_plotly_to_aws <- function(gp_object, filename) {
  htmlwidgets::saveWidget(
    plotly::partial_bundle(gp_object) |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::config(displaylogo = FALSE) |>
    plotly::layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE))|>
    plotly::layout(plot_bgcolor='transparent') |>
    plotly::layout(paper_bgcolor='transparent'), paste0("output/", filename, ".html")
  )

  aws.s3::put_object(
    file = file.path(paste0("output/", filename, ".html")),
    acl="public-read",
    object = paste0("clean_data/funding-tracker/system_size_synthesis_report/", filename, ".html"),
    bucket = aws_bucket
  )
  
}

# function to build one state's mini-table in gt
build_state_table <- function(one_state_data) {
  state_name <- unique(one_state_data$state)
  
  one_state_data |>
    dplyr::select(-state) |>   # drop state col since it's now the table title
    gt::gt() |>
    gt::tab_header(title = state_name) |>
    gt::cols_label(state_fiscal_year = "SFY") |>
    gt::data_color(
      columns = dplyr::all_of(target_cols),
      palette = blue_palette(4),
      direction = "row"
    ) |>
    gt::cols_move(
      columns = c(`SS (3,301-10,000)`, `LS (>10,000)`),
      after = `VSS (<= 3,300)`
    ) |>
    gt::text_transform(
      locations = gt::cells_body(columns = `VSS (<= 3,300)`),
      fn = function(x) sprintf("%s (%.1f%%)", x, one_state_data$`VSS (<= 3,300)_pct` * 100)
    ) |>
    gt::text_transform(
      locations = gt::cells_body(columns = `SS (3,301-10,000)`),
      fn = function(x) sprintf("%s (%.1f%%)", x, one_state_data$`SS (3,301-10,000)_pct` * 100)
    ) |>
    gt::text_transform(
      locations = gt::cells_body(columns = `LS (>10,000)`),
      fn = function(x) sprintf("%s (%.1f%%)", x, one_state_data$`LS (>10,000)_pct` * 100)
    ) |>
    # gt::text_transform(
    #   locations = gt::cells_body(columns = `No Information`),
    #   fn = function(x) sprintf("%s (%.1f%%)", x, one_state_data$`No Information_pct` * 100)
    # ) |>
    gt::cols_hide(columns = dplyr::all_of(pct_cols)) |>
    gt::opt_table_font(
      font = gt::google_font("Lato"),
      size = gt::px(12)
    ) |>
    gt::tab_options(
      heading.title.font.size = gt::px(14), # coresponds to state name for all tables instead of the first
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = gt::px(14),
      heading.subtitle.font.weight = "bold",
      row_group.font.size = gt::px(14),
      row_group.font.weight = "bold"
    )
}


# Styling ===========================================
blue_palette <- colorRampPalette(c("#b0bcdd", "#8598c8", "#1054a8", 
                                   "#0d4393", "#09327e"))

cont_palette <- colorRampPalette(c("#172f60","#4ea324"))

cat_palette <- colorRampPalette(c("#172f60","#1054a8",
                                  "#791a7b","#de9d29", 
                                  "#b15712","#4ea324"))

epic_chart_theme <- theme_minimal() + 
  theme(legend.position = "right", 
        text = element_text(size = 11), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), 
        axis.text.x = element_text(margin = margin(t = 10, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, 
                                                    b = 0, l = 0)), 
        axis.text.y = element_text(margin = margin(t = 0, r = 10, 
                                                   b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0))) 

# Read data ===========================================
## Funding Tracker Data
## Date: Latest update
all_projects <- aws.s3::s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") |>
  janitor::clean_names() |>
  dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year))

## SRF Portal Data (Project Dataset)
## Date: 2026-05-11 download
# SRF projects that began or were amended (that is, money added or taken away/deobligated) since 2021.
# https://sdwis.epa.gov/ords/sfdw_pub/r/sfdw/owsrf_public/srf-map
data_file_path <- "~/Downloads/DW_AA_PubPortal_2022_Merge_All_Centroid_-7487949132089372111.csv"

raw_dwsrf_map <- readr::read_csv(data_file_path) |>
  janitor::clean_names() 

## National Drinking Water Tool Data
## Community Water Systems
## Date: Data list was assembled on 2/20/2026. 
#EmmaLi's comments:
# It contains SDWIS, EPA SABs, and the DWSRF funding data (& some other summaries I made). 
# In this summary list, the SDWIS dataset I believe was updated in February 2026, but I also have a more recent update from July 14th that isn't reflected in that RDS dataset yet: s3://tech-team-data/national-dw-tool/clean/national/sdwis_viols.csv (edited)
national_water_system <- aws.s3::s3read_using(
  readRDS,
  object = "s3://tech-team-data/national-dw-tool/clean/national/national_water_system.RData"
)

# Constants ===========================================
# focus_states <- c("AL", "IL", "IN", "LA", "MD", "MI", "MN", "MS", "NJ", "NY", "OH", "PA", "TN", "TX", "WI")
focus_states <- c("AL", "IL", "IN", "LA", "MD", "MN", "MS", "NJ", "NY", "OH", "PA", "TN", "TX", "WI")

# target_cols <- c("VSS (<= 3,300)", "SS (3,301-10,000)", "LS (>10,000)", "No Information") #[keeping only complete cases]
target_cols <- c("VSS (<= 3,300)", "SS (3,301-10,000)", "LS (>10,000)")

aws_bucket <- "water-team-data"

# Clean data ===========================================
ft_data_clean <- all_projects |>
  dplyr::filter(!state_fiscal_year == "2027") |>
  dplyr::mutate(system_size = dplyr::case_when(
    population == "No Information" ~ "No Information",
    is.na(population) ~ "No Information", 
    # convert_to_numeric(population, TRUE) == 0 ~ "No Information",
    convert_to_numeric(population, TRUE) >= 0 & convert_to_numeric(population, TRUE) <= 3300 ~ "VSS (<= 3,300)" ,
    convert_to_numeric(population, TRUE) > 3300 & convert_to_numeric(population, TRUE) <= 10000 ~ "SS (3,301-10,000)",
    convert_to_numeric(population, TRUE) > 10000 ~ "LS (>10,000)",
    .default = "check"
  ))

portal_data_clean <- raw_dwsrf_map |>
  dplyr::mutate(
    initial_agreement_date_fmt = lubridate::mdy(stringr::word(initial_agreement_date, end = 1L)), #extract date portion of POSIXct column 
    calendar_year = lubridate::year(initial_agreement_date_fmt),
    system_size =  dplyr::case_when(
      # total_population == 0 ~ "No Information",
      total_population  >= 0 & total_population <= 3300 ~ "VSS (<= 3,300)",
      total_population > 3300 & total_population <= 10000 ~ "SS (3,301-10,000)",
      total_population > 10000 ~ "LS (>10,000)",
      .default = "check")
    )

ndwt_data_clean <- national_water_system$epa_sab |>
  dplyr::select(pwsid, pws_name, primacy_agency, pop_cat_5) |>
  dplyr::mutate(state = state.name[match(primacy_agency, state.abb)]) |>
  # keep only focus states
  dplyr::filter(primacy_agency %in% focus_states) |>
  dplyr::mutate(
    system_size =  dplyr::case_when(
      pop_cat_5 == "<=500" | pop_cat_5  == "501-3,300" ~ "VSS (<= 3,300)",
      pop_cat_5 == "3,301-10,000" ~ "SS (3,301-10,000)",
      pop_cat_5 == "10,001-100,000" | pop_cat_5 == ">100,000" ~ "LS (>10,000)",
      .default = "check")
  ) |> 
  dplyr::mutate(state = state.name[match(primacy_agency, state.abb)]) |>
  # keep only focus states
  dplyr::filter(primacy_agency %in% focus_states)


# Cleaning process order:
# (1) checked PWSIDs number of characters, inspected those not equal to length 9 and manually fixed by comparing to SABs data.
# (2) checked group of project_ids with inconsistent PWSID and reconciled by correcting PWSID or disambiguating projects (adding a suffix to denote different projects)
# (3) checked group of project_ids with inconsistent system size categories and reconciled by correcting size categories based on latest designation in FT dataset or disambiguating projects (adding a suffix to denote different projects)
# (4) checked observations with population values equal to 0 and reconciled 
# (5) Complete cases where PWSID data available using SABS (data imputation)
# (6) Retained only complete cases system_size --> ft_data_clean_ss_complete_cases

ft_data_clean_ss_complete_cases <- readr::read_csv("~/Downloads/2026_08_11_ft_data_clean_ss_complete_cases.csv", col_types = readr::cols(.default = "c"))
# Summaries ===========================================

# Universe of DWS

# (1) Proportion of projects on comprehensive  list and fundable list 
# for all 15 focus states we have data for 
# (a) 6 columns  (one table for each of the three size categories)
# (1) total systems statewide; 
# (2) #/proportion of systems statewide that are in VS, SS, LS,  
# (5) Number of small systems on comprehensive list; 
# (6) Total Number of projects on comprehensive list; 
# (7) proportion of small systems on comprehensive list; 
# (8) Number of small systems on fundable list; 
# (9) Total Number of projects on fundable list; 
# (10) proportion of projects on fundable list that are small systems
# Highlight the states where there’s big disparities between representation on the comprehensive list and fundable list  
# (b) funnelling data viz: do for one or two of the states above and explain why we can’t do it for all the states


##### Project counts by state and fiscal year ----
#RAW
ft_project_counts <- ft_data_clean |> 
  dplyr::group_by(state, state_fiscal_year) |>
  dplyr::tally() |>
  dplyr::ungroup()

#COMPLETE
ft_project_counts_complete <- ft_data_clean_ss_complete_cases |> 
  dplyr::group_by(state, state_fiscal_year) |>
  dplyr::tally() |>
  dplyr::ungroup()

#COMPARE
compare_ft_datasets <- ft_project_counts |>
  dplyr::left_join(ft_project_counts_complete, by = c("state", "state_fiscal_year")) |>
  dplyr::mutate(
    perc_change = 100*(n.x - n.y)/n.x
  ) |>
  dplyr::group_by(state) |>
  gt::gt() |>
  gt::cols_label(
    state_fiscal_year = "SFY",
    n.x = "RAW",
    n.y = "COMPLETE",
    perc_change = "PERC CHANGE"
  ) |>
  gt::data_color(
    columns = perc_change,
    palette = blue_palette(4)
  ) |>
  gt::tab_header(
    title = "Project count reduction with complete system size information"
  )

# save_html_table_to_s3(gt_table = compare_ft_datasets, filename = "compare_ft_datasets", bucket = aws_bucket)


# Summary by unique pwsid 2026_10_02 -----
# When pwsid information was NA or No Info, we relied on borrower information and counted as unique systems

## Summary of missingness ----
ft_data_clean_ss_complete_cases |>
  dplyr::group_by(state, state_fiscal_year) |>
  dplyr::tally() |>
  dplyr::rename(
    project_counts_complete_ss = n
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    #Missingness info for pwsid
    ft_data_clean_ss_complete_cases |>
      dplyr::mutate(
        pwsid = dplyr::case_when(
          pwsid == "MD006002" ~ "MD0060002",
          pwsid %in% c("TBD", "Pending", "0", "OH") ~ "No Information", 
          pwsid == "OH661721" ~ "OH0600712",
          .default = pwsid
        )          
      ) |>
      dplyr::group_by(state, state_fiscal_year, pwsid) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      dplyr::filter(grepl("No Info", pwsid) | is.na(pwsid)) |>
      tidyr::pivot_wider(
        names_from = pwsid,
        values_from = n,
        values_fill = 0
      ) |>
      dplyr::rename(
        pwsid_NA = `NA`,
        pwsid_no_info = `No Information`
      ), by = c("state", "state_fiscal_year")
  ) |>
  dplyr::left_join(
    #Missingness info for borrower
    ft_data_clean_ss_complete_cases |>
      dplyr::group_by(state, state_fiscal_year, borrower) |>
      dplyr::tally() |>
      dplyr::filter(grepl("No Info", borrower) | is.na(borrower)) |>
      tidyr::pivot_wider(
        names_from = borrower,
        values_from = n
      ) |>
      dplyr::rename(
        borrower_NA = `NA`
      ), by = c("state", "state_fiscal_year")
  ) |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(c("pwsid_NA", "pwsid_no_info", "borrower_NA")), ~ tidyr::replace_na(.x, 0))
  ) |>
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1IB11F2GBpiUzwPNf8TYXLVvQTJlLvH3JjAifkvWy2wg",
    sheet = "pwsid_borrower_missingness_summary"
  )


## Summary for comprehensive -----
ft_data_clean_ss_complete_cases |>
  dplyr::mutate(
    pwsid = dplyr::case_when(
      pwsid == "MD006002" ~ "MD0060002",
      pwsid %in% c("TBD", "Pending", "0", "OH") ~ "No Information", 
      pwsid == "OH661721" ~ "OH0600712",
      .default = pwsid
    )          
  ) |>
  dplyr::mutate(
    pwsid = ifelse(pwsid == "No Information", NA_character_, pwsid),
    pwsid = dplyr::coalesce(pwsid, borrower)
  ) |>
  dplyr::filter(!is.na(pwsid) | pwsid == "No Information") |>
  dplyr::group_by(state, pwsid, state_fiscal_year, system_size) |>
  dplyr::tally() |>
  dplyr::ungroup() |>
  dplyr::select(-pwsid, -n) |>
  dplyr::group_by(state, state_fiscal_year, system_size) |>
  dplyr::tally() |>
  dplyr::mutate(
    system_size = forcats::fct(system_size, levels = target_cols)
  ) |>
  tidyr::pivot_wider(
    names_from = system_size,
    values_from = n
  ) |>
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1IB11F2GBpiUzwPNf8TYXLVvQTJlLvH3JjAifkvWy2wg",
    sheet = "comprehensive_sfy_state_unique_pwsid_counts"
  )

## Summary for on fundable list -----
ft_data_clean_ss_complete_cases |>
  dplyr::mutate(
    pwsid = dplyr::case_when(
      pwsid == "MD006002" ~ "MD0060002",
      pwsid %in% c("TBD", "Pending", "0", "OH") ~ "No Information", 
      pwsid == "OH661721" ~ "OH0600712",
      .default = pwsid
    )          
  ) |>
  dplyr::filter(
    expecting_funding == "Yes"
  ) |>
  dplyr::mutate(
    pwsid = ifelse(pwsid == "No Information", NA_character_, pwsid),
    pwsid = dplyr::coalesce(pwsid, borrower)
  ) |>
  dplyr::filter(!is.na(pwsid) | pwsid == "No Information") |>
  dplyr::group_by(state, pwsid, state_fiscal_year, system_size) |>
  dplyr::tally() |>
  dplyr::ungroup() |>
  dplyr::select(-pwsid, -n) |>
  dplyr::group_by(state, state_fiscal_year, system_size) |>
  dplyr::tally() |>
  dplyr::mutate(
    system_size = forcats::fct(system_size, levels = target_cols)
  ) |>
  tidyr::pivot_wider(
    names_from = system_size,
    values_from = n
  ) |>
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1IB11F2GBpiUzwPNf8TYXLVvQTJlLvH3JjAifkvWy2wg",
    sheet = "fundable_sfy_state_unique_pwsid_counts"
  )



##### Question 1: Summaries with complete cases data -----

# Project counts by state, fiscal year, and system size
pct_cols <- paste0(target_cols, "_pct")

# Project counts by state, fiscal year, and system size
ft_project_counts_ss <- ft_data_clean_ss_complete_cases |> 
  dplyr::group_by(state, state_fiscal_year, system_size) |>
  dplyr::tally() |>
  dplyr::mutate(
    system_size = forcats::fct(system_size, levels = target_cols)
  ) |>
  tidyr::pivot_wider(
    names_from = system_size,
    values_from = n
  ) |>  
  dplyr::ungroup() |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(target_cols), ~ tidyr::replace_na(.x, 0))
  ) |>
  dplyr::mutate(
    total = rowSums(dplyr::pick(dplyr::all_of(target_cols)))
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(target_cols),
      ~ .x / total,
      .names = "{.col}_pct"
    )
  )

# split data by state, build a mini gt table per state
focus_state_names <- ft_project_counts_ss |>
  dplyr::group_split(state) |>
  purrr::map_chr(~ unique(.x$state))

prelim_ft_project_counts_ss_gt <- ft_project_counts_ss |>
  dplyr::group_split(state) |>
  purrr::map(build_state_table)

# overwrite first table: overall title as title, state name demoted to subtitle
prelim_ft_project_counts_ss_gt[[1]] <- prelim_ft_project_counts_ss_gt[[1]] |>
  gt::tab_header(
    title = "System size distribution of projects in focus states",
    subtitle = focus_state_names[1]  # state name stays as title, consistent with every other table
  ) |>
    gt::tab_options(
      table.font.size = gt::px(12),
      heading.title.font.size = gt::px(16),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = gt::px(14), # coresponds to state name for the first table
      heading.subtitle.font.weight = "bold",
      row_group.font.size = gt::px(14),
      row_group.font.weight = "bold"
    )


n_tables <- length(prelim_ft_project_counts_ss_gt)
prelim_ft_project_counts_ss_gt[[n_tables]] <- prelim_ft_project_counts_ss_gt[[n_tables]] |>
  gt::tab_footnote(footnote = gt::md("Data source: EPIC Funding Tracker Complete Cases"))

#!!! (the "bang-bang-bang" / splice operator)
# rlang/tidyverse's unquoting via splicing. 
# Instead of passing the whole list as a single argument (gt_group(prelim_ft_project_counts_ss_gt), 
# !!! unpacks ("splices") the list into separate arguments, gt_group() expects each table passed as its own argument

ft_project_counts_ss_gt <- gt::gt_group(!!!prelim_ft_project_counts_ss_gt) 

# save_html_table_to_s3(gt_table = ft_project_counts_ss_gt, filename = "ft_system_size_distribution_complete_ss", bucket = aws_bucket)

# To note:
# Cannot perform this analysis with Michigan since we dont have population information
# We can use SABs data to fill in No Information in other states (i.e., Illinois) with pwsid

summary_ef_size <- ft_data_clean_ss_complete_cases |> 
  dplyr::group_by(state, state_fiscal_year, system_size, expecting_funding) |>
  dplyr::tally() |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = system_size, values_from = n) 

# expecting funding  by system size
summary_fundable_size <- summary_ef_size |>
  dplyr::filter(expecting_funding == "Yes") |>
  dplyr::select(-expecting_funding) |>
  dplyr::relocate(
    target_cols
  ) |>
  dplyr::relocate(state_fiscal_year, state)

# save_csv_to_s3(summary_fundable_size, "ft_system_size_complete_ss_fun", aws_bucket)

# comprehensive by system size
summary_comprehensive_size <- summary_ef_size |>
  dplyr::select(-expecting_funding) |>
  dplyr::group_by(state_fiscal_year, state) |>
  dplyr::summarise(
    dplyr::across(
        dplyr::all_of(target_cols),
        ~ sum(.x, na.rm = TRUE),
        .names = "{.col}"
      )
  ) |>
  dplyr::arrange(state, state_fiscal_year)

# save_csv_to_s3(summary_comprehensive_size, "ft_system_size_complete_ss_comp", aws_bucket)

# system size and funding list category
ft_project_counts_ss_cat <- summary_comprehensive_size |>
  dplyr::mutate(
    category = "Comprehensive"
  ) |>
  dplyr::bind_rows(
    summary_fundable_size |> dplyr::mutate(category = "Fundable")
  ) |> 
  dplyr::mutate(
    total = rowSums(dplyr::pick(dplyr::all_of(target_cols)))
  ) |>
   dplyr::mutate(
    dplyr::across(
      dplyr::all_of(target_cols),
      ~ .x / total,
      .names = "{.col}_pct"
    )
  ) |>
  dplyr::arrange(state, state_fiscal_year)

# save_csv_to_s3(ft_project_counts_ss_cat, "ft_system_size_complete_ss_comp_fund", aws_bucket)

## Comprehensive GT =========

prelim_ft_project_counts_ss_cat_comp_gt <- ft_project_counts_ss_cat |>
  dplyr::ungroup() |>
  dplyr::filter(category == "Comprehensive") |>
  dplyr::select(-category) |>
  dplyr::group_split(state) |>
  purrr::map(build_state_table)

prelim_ft_project_counts_ss_cat_comp_gt[[1]] <- prelim_ft_project_counts_ss_cat_comp_gt[[1]] |>
  gt::tab_header(
    title = gt::md("System size distribution of projects in  \n**Comprehensive** List"),
    subtitle = focus_state_names[1]  # state name stays as title, consistent with every other table
  ) |>
    gt::tab_options(
      table.font.size = gt::px(12),
      heading.title.font.size = gt::px(16),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = gt::px(14), # coresponds to state name for the first table
      heading.subtitle.font.weight = "bold",
      row_group.font.size = gt::px(14),
      row_group.font.weight = "bold"
    )


n_tables <- length(prelim_ft_project_counts_ss_cat_comp_gt)
prelim_ft_project_counts_ss_cat_comp_gt[[n_tables]] <- prelim_ft_project_counts_ss_cat_comp_gt[[n_tables]] |>
  gt::tab_footnote(footnote = gt::md("Data source: EPIC Funding Tracker Complete Cases"))

prelim_ft_project_counts_ss_cat_comp_gt <- gt::gt_group(!!!prelim_ft_project_counts_ss_cat_comp_gt) 

## Fundable GT =========
prelim_ft_project_counts_ss_cat_fun_gt <- ft_project_counts_ss_cat |>
  dplyr::ungroup() |>
  dplyr::filter(category == "Fundable") |>
  dplyr::select(-category) |>
  dplyr::group_split(state) |>
  purrr::map(build_state_table)

prelim_ft_project_counts_ss_cat_fun_gt[[1]] <- prelim_ft_project_counts_ss_cat_fun_gt[[1]] |>
  gt::tab_header(
    title = gt::md("System size distribution of projects in  \n**Fundable** List"),
    subtitle = focus_state_names[1]  # state name stays as title, consistent with every other table
  ) |>
    gt::tab_options(
      table.font.size = gt::px(12),
      heading.title.font.size = gt::px(16),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = gt::px(14), # coresponds to state name for the first table
      heading.subtitle.font.weight = "bold",
      row_group.font.size = gt::px(14),
      row_group.font.weight = "bold"
    )

n_tables <- length(prelim_ft_project_counts_ss_cat_fun_gt)
prelim_ft_project_counts_ss_cat_fun_gt[[n_tables]] <- prelim_ft_project_counts_ss_cat_fun_gt[[n_tables]] |>
  gt::tab_footnote(footnote = gt::md("Data source: EPIC Funding Tracker Complete Cases"))

prelim_ft_project_counts_ss_cat_fun_gt <- gt::gt_group(!!!prelim_ft_project_counts_ss_cat_fun_gt) 

# gtExtras::gt_two_column_layout(list(prelim_ft_project_counts_ss_cat_comp_gt, prelim_ft_project_counts_ss_cat_fun_gt))

# save_html_table_to_s3(gt_table = prelim_ft_project_counts_ss_cat_comp_gt, filename = "ft_system_size_distribution_complete_ss_comp", bucket = aws_bucket)
# save_html_table_to_s3(gt_table = prelim_ft_project_counts_ss_cat_fun_gt, filename = "ft_system_size_distribution_complete_ss_fun", bucket = aws_bucket)


## Universe =========
ndwt_high_level_summary_table <- ndwt_data_clean |>
  dplyr::filter(state %in% focus_state_names) |>
  dplyr::select(pwsid, state, system_size) |>
  dplyr::distinct() |>
  dplyr::group_by(state, system_size) |>
  dplyr::tally() |>
  dplyr::ungroup() |>
  dplyr::mutate(
    system_size = forcats::fct(system_size, levels = c("VSS (<= 3,300)", "SS (3,301-10,000)","LS (>10,000)"))
  ) |>
  tidyr::pivot_wider(
    names_from = system_size,
    values_from = n
  ) |>
  dplyr::mutate(category = "Universe")

ndwt_high_level_summary_table_gt <- ndwt_high_level_summary_table |>
  gt::gt() |>
  gt::tab_header(
    title = "System size distribution of community drinking water systems in focus states",    
  ) |>
  gt::cols_label(state = "State") |>
  gt::data_color(
    columns = c(`VSS (<= 3,300)`, `SS (3,301-10,000)`,`LS (>10,000)`),  # the columns you're comparing within each row
    palette = blue_palette(4),
    direction = "row"   # <- this is the key argument
  ) |>
  gt::cols_hide(
    category
  ) |>
  gt::cols_move(
    columns = c(`SS (3,301-10,000)`,`LS (>10,000)`),
    after = `VSS (<= 3,300)`
  ) |>
  gt::tab_footnote(
    footnote = gt::md("Data source: National Drinking Water Tool")
  )

# save_html_table_to_s3(gt_table = ndwt_high_level_summary_table_gt, filename = "ndwt_system_size_distribution", bucket = aws_bucket)

## Crosswalks -----
# 2026-08-11
# Keep IUP data intact, and look at crosswalk exercise as a separate exercise 
# where we see a one to any, and accept that relationship, and justify it (e.g., admin decision to phase out etc.) 
# and articulate it in our methodology.
# The general process for assessing this: where we see violations of the 1:1 rule, we scrutinize those and make a judgement, 
# we think there was an error or duplication, these aren't related or are related, these are from same borrower, 
# issued in a phase way etc.

# Produce Aggregate Heatmap, one plot per system size

### Portal Data ========================
portal_agreement_crosswalk_prep <- portal_data_clean |>
  dplyr::filter(state %in% focus_state_names) |>
  dplyr::select(state, state_tracking_number, calendar_year, system_size, current_agreement_amount, project_description, project_name) |>
  dplyr::rename(base_project_id = state_tracking_number) |>
  dplyr::mutate(
    calendar_year = as.numeric(as.character(calendar_year)),  # factor -> numeric, safely
    project_id = dplyr::case_when(
      state == "Illinois" ~ stringr::str_sub(base_project_id, 4),
      state == "Indiana" ~ sub("[a-z]+$", "", base_project_id),           # strip trailing letter suffix, if any,
      state == "New York" ~ stringr::str_remove(base_project_id, "-.*"),
      .default = base_project_id
    )
  )

# Inspect project ids duplicates
portal_duplicates <- portal_agreement_crosswalk_prep |>
  #these variables are used for joins
  dplyr::group_by(state, project_id) |>
  dplyr::tally() |>
  dplyr::filter(n>1)

#there are 19 non-unique project_id in portal data (we crosswalk 4 in future steps)

### Funding Tracker Data ========================

# Pay close attention to project ids that were changed in the qc process,
# there's 12 unique ids that were modified --> 16 combinations of sta, base_project_id, and project_id
ft_data_clean_ss_complete_cases |>
  dplyr::mutate(id_same = project_id == base_project_id) |> 
  dplyr::filter(
    !id_same
  ) |>
  dplyr::select(state, base_project_id, project_id) |>
  dplyr::distinct() |>
  dplyr::left_join(
    portal_agreement_crosswalk_prep,
    by = c("state", "base_project_id")
  )|>
  dplyr::filter(
    !is.na(calendar_year)
  )

# only 2 project ids from Wisconsin would be affected by id changes (i.e, should receive funding) coresponding to:
# 4892-03
# 5649-02 

ft_duplicates <- ft_data_clean_ss_complete_cases |>
  dplyr::filter(!is.na(project_id)) |>
  dplyr::filter(!project_id == "No Information") |>
  dplyr::group_by(state, state_fiscal_year, project_id) |>
  dplyr::tally() |>
  dplyr::filter(n>1)

ft_agreement_crosswalk_prep <- ft_data_clean_ss_complete_cases |>
  dplyr::select(project_id, base_project_id, state, state_fiscal_year, system_size, project_description, project_name, project_cost, requested_amount, funding_amount, expecting_funding) |>
  dplyr::mutate(
    project_id = stringr::str_replace_all(project_id, "\\s+", ""),
    project_id = dplyr::case_when(
      state == "Louisiana" ~ stringr::str_remove(project_id, "-GS|-EC|-LSLI|-LSLR|-LSL"),
      state == "Indiana"   ~ stringr::str_remove(project_id, " "),
      state == "New Jersey" ~  stringr::str_replace(project_id, "-\\s*(.{0,3}).*", "-\\1"),
      state == "Michigan" ~ stringr::str_sub(project_id, start = 1, end = 7),
      .default = project_id),
    project_id = stringi::stri_trim(project_id)
    )
### Joins ========================
ft_agreement_crosswalk_prep_join <- ft_agreement_crosswalk_prep |>
  dplyr::group_by(state) |>
  dplyr::mutate(
    epic_id = paste0(state, "_", dplyr::row_number())
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    portal_agreement_crosswalk_prep |> 
      dplyr::select(state, project_id, base_project_id, project_description, project_name, current_agreement_amount) |> 
      dplyr::distinct() |> 
      dplyr::mutate(has_agreement = "Yes"),
    by = c("state", "project_id")
  ) |>
  dplyr::mutate(
    has_agreement = ifelse(is.na(has_agreement), "No", has_agreement)
  )

#manually correct many to many relationship based to multiple project ids in srf map and inspect Wisconsin id changes
nrow(ft_agreement_crosswalk_prep_join) - nrow(ft_data_clean_ss_complete_cases)

epic_id_inspect <- ft_agreement_crosswalk_prep_join |>
  dplyr::group_by(epic_id) |>
  dplyr::tally() |>
  dplyr::filter(n >1) |>
  dplyr::pull(epic_id)

### Inspect and review ========================
# cases where there's ft duplciates or WI specific cases that had a project id change in the qc excercise
ft_agreement_crosswalk_prep_join |>
  dplyr::mutate(epic_drop_id = dplyr::row_number()) |>
  dplyr::filter(epic_id %in% epic_id_inspect) |> 
  dplyr::filter(has_agreement == "Yes") |>
  View()

ft_agreement_crosswalk_prep_join |>
  dplyr::mutate(epic_drop_id = dplyr::row_number()) |>
  dplyr::filter(project_id %in% ft_duplicates$project_id) |> 
  dplyr::filter(has_agreement == "Yes") |>
  View()

ft_agreement_crosswalk_prep_join |>
  dplyr::filter(state == "Wisconsin") |>
  dplyr::filter(base_project_id.x %in% c("4892-03", "5649-02")) |>
  View()

### Final crosswalk ========================
ft_agreement_crosswalk_ungrouped <- ft_agreement_crosswalk_prep_join |> 
  dplyr::mutate(
    has_agreement = dplyr::case_when(
      project_id == "4892-03-2" ~ "Yes",
      .default = has_agreement
    )
  )

ft_agreement_crosswalk <- ft_agreement_crosswalk_prep_join |> 
  dplyr::mutate(
    has_agreement = dplyr::case_when(
      project_id == "4892-03-2" ~ "Yes",
      .default = has_agreement
    )
  ) |>
  dplyr::group_by(state, state_fiscal_year, system_size, has_agreement) |> 
  dplyr::tally() |> 
  dplyr::ungroup() |>
  dplyr::filter(has_agreement =="Yes") |>
  dplyr::select(-has_agreement) |>
  tidyr::pivot_wider(names_from = "system_size", values_from = "n") |>
  dplyr::mutate(
    category = "Agreement"
  )
  
### Heatmap plotting ======================
#### ALL COMPLETE CASE DATA ------
years_in_scope <- c("2021", "2022", "2023", "2024", "2025", "2026")

grouped_data <- summary_comprehensive_size |>
  dplyr::mutate(
    category = "Comprehensive List"
  ) |>
  dplyr::bind_rows(summary_fundable_size |> dplyr::mutate(category = "Fundable List")) |>
  dplyr::bind_rows(ndwt_high_level_summary_table |> tidyr::crossing(state_fiscal_year = years_in_scope)) |>
  dplyr::bind_rows(ft_agreement_crosswalk) |>
  dplyr::ungroup() |>
  tidyr::complete(state_fiscal_year, category, state) |>
  dplyr::mutate(
    category = forcats::fct(category, levels = c("Universe", "Comprehensive List", "Fundable List", "Agreement")),
    state = forcats::fct(state, levels = rev(focus_state_names))
  )

label_map <- c(
  "Universe" = "U",
  "Comprehensive List" = "C",
  "Fundable List" = "F",
  "Agreement" = "A"
)
 
stages_plots <- purrr::map(target_cols, function(col) {
  ggplot2::ggplot() +
    #solid tile Universe tile layer
    geom_tile(
      data = grouped_data |> dplyr::filter(category == "Universe"),
      aes(category, state), 
      fill = "#4EA324",
      width = 1,
      height = 1
    ) +
    # #label layer for system size
    # geom_text(data = grouped_data , aes(category, state, fill = .data[[col]], label = .data[[col]])) +
    #gradient tile Comp, Fun, and Agreement layers 
    geom_tile(
      data = grouped_data |> dplyr::filter(!category == "Universe"),
      aes(category, state, fill = .data[[col]]),
      width = 1,
      height = 1
    ) +
    #label Universe layer in black
    geom_text(data = grouped_data |> dplyr::filter(category == "Universe"),
              aes(category, state, label = .data[[col]]), color = "black") +
    #label Comp, Fun, and Agreement layers in white
    geom_text(data = grouped_data |> dplyr::filter(!category == "Universe"),
              aes(category, state, label = .data[[col]]), color = "white") +
    facet_grid(~state_fiscal_year ) +
    labs(
      title = paste0("Project counts across funding stages and years for: ", col),
      x ="",
      y = "",
      caption = "Data Sources: \nUniverse (U): EPA SABs, note that the data reflects community water systems only\nComprehensive (C) and Fundable (F) Lists: Funding Tracker Standardization of DWSRF IUP PPLs; note that projects can persist across SFYs\nAgreements (A): EPA DWSRF Portal; note that agreements are usually not reached in the SFY, and they are depicted for comparison purposes"
    ) +
    # guides(
    #   fill = "none"
    # ) +
    scale_x_discrete(labels = label_map) +
    scale_fill_gradientn(colors = blue_palette(3)) +
    epic_chart_theme
})

stages_plotly_caption <- "Data Sources: \nUniverse (U): EPA SABs, note that the data reflects community water systems only\nComprehensive (C) and Fundable (F) Lists: Funding Tracker Standardization of DWSRF IUP PPLs; note that projects can persist across SFYs\nAgreements (A): EPA DWSRF Portal; note that agreements are usually not reached in the SFY, and they are depicted for comparison purposes"


# save_plotly_to_aws(
#   plotly::ggplotly(stages_plots[[1]]) |>
#       plotly::layout(
#         margin = list(b = 140),
#         annotations = list(list(
#           text = gsub("\n", "<br>", stages_plotly_caption),
#           xref = "paper", yref = "paper",
#           x = 0, y = -0.05,
#           xanchor = "left", yanchor = "top",
#           showarrow = FALSE,
#           font = list(size = 10, color = "grey40"),
#           align = "left"
#         ))
#       ), 
#       "heatmap_funding_stages_vss")

# save_plotly_to_aws(
#   plotly::ggplotly(stages_plots[[2]]) |>
#       plotly::layout(
#         margin = list(b = 140),
#         annotations = list(list(
#           text = gsub("\n", "<br>", stages_plotly_caption),
#           xref = "paper", yref = "paper",
#           x = 0, y = -0.05,
#           xanchor = "left", yanchor = "top",
#           showarrow = FALSE,
#           font = list(size = 10, color = "grey40"),
#           align = "left"
#         ))
#       ),
#       "heatmap_funding_stages_ss")

# save_plotly_to_aws(plotly::ggplotly(stages_plots[[3]]) |>
#       plotly::layout(
#         margin = list(b = 140),
#         annotations = list(list(
#           text = gsub("\n", "<br>", stages_plotly_caption),
#           xref = "paper", yref = "paper",
#           x = 0, y = -0.05,
#           xanchor = "left", yanchor = "top",
#           showarrow = FALSE,
#           font = list(size = 10, color = "grey40"),
#           align = "left"
#         ))
#       ),
#       "heatmap_funding_stages_ls")

#### SUBSETS COMPLETE CASE DATA ------

#### ALL COMPLETE CASE DATA SPECIFIC YEARS------
years_in_scope_complete <- c("2022", "2023", "2024", "2025")

grouped_data_complete <- grouped_data |>
  dplyr::filter(state_fiscal_year %in% years_in_scope_complete) 

# years_in_scope_complete <- c("2022", "2023", "2024", "2025")
# target_cols <- c("VSS (<= 3,300)", "SS (3,301-10,000)", "LS (>10,000)")

## Per-target-column (system_size) completeness ----

# review
gd_long <- grouped_data_complete |>
  tidyr::pivot_longer(cols = all_of(target_cols),
               names_to = "size_category",
               values_to = "value") |>
  dplyr::mutate(has_data = !is.na(value))

gd_col_check <- gd_long |>
  dplyr::select(state_fiscal_year, category, state, size_category, has_data) |>
  tidyr::complete(state_fiscal_year = years_in_scope_complete,
           tidyr::nesting(category, state, size_category),
           fill = list(has_data = FALSE))

gd_col_summary <- gd_col_check |>
  dplyr::group_by(state, category, size_category) |>
  dplyr::summarise(
    years_missing = list(setdiff(years_in_scope_complete, state_fiscal_year[has_data])),
    n_years_present = sum(has_data),
    is_complete_years = n_years_present == length(years_in_scope_complete),
    .groups = "drop"
  )

gd_complete_by_col <- gd_col_summary |> filter(is_complete_years)

gd_complete_states_summary <- gd_complete_by_col |>
  dplyr::group_by(state, size_category) |>
  dplyr::mutate(
    n_funding_category_present = dplyr::n(),
    is_complete_funding_category = n_funding_category_present == 4
  ) |>
  dplyr::select(-years_missing)

# save_csv_to_s3(gd_complete_states_summary, "complete_states_summary_scope", aws_bucket)


gd_complete_states <-   gd_complete_states_summary |>
  dplyr::filter(is_complete_years, is_complete_funding_category) |>
  dplyr::ungroup() |>
  dplyr::select(state) |>
  dplyr::distinct() |>
  dplyr::pull(state) |> 
  as.character()


grouped_data_complete <- grouped_data_complete |>
  dplyr::filter(state %in% gd_complete_states)

stages_plots_complete <- purrr::map(target_cols, function(col) {
  ggplot2::ggplot() +
    #solid tile Universe tile layer
    geom_tile(
      data = grouped_data_complete |> dplyr::filter(category == "Universe"),
      aes(category, state), 
      fill = "#4EA324",
      width = 1,
      height = 1
    ) +
    geom_tile(
      data = grouped_data_complete |> dplyr::filter(!category == "Universe"),
      aes(category, state, fill = .data[[col]]),
      width = 1,
      height = 1
    ) +
    #label Universe layer in black
    geom_text(data = grouped_data_complete |> dplyr::filter(category == "Universe"),
              aes(category, state, label = .data[[col]]), color = "black") +
    #label Comp, Fun, and Agreement layers in white
    geom_text(data = grouped_data_complete |> dplyr::filter(!category == "Universe"),
              aes(category, state, label = .data[[col]]), color = "white") +
    facet_grid(~state_fiscal_year ) +
    labs(
      title = paste0("Project counts across funding stages and years for: ", col),
      x ="",
      y = "",
      caption = "Data Sources: \nUniverse (U): EPA SABs, note that the data reflects community water systems only\nComprehensive (C) and Fundable (F) Lists: Funding Tracker Standardization of DWSRF IUP PPLs; note that projects can persist across SFYs\nAgreements (A): EPA DWSRF Portal; note that agreements are usually not reached in the SFY, and they are depicted for comparison purposes"
    ) +
    scale_x_discrete(labels = label_map) +
    scale_fill_gradientn(colors = blue_palette(3)) +
    epic_chart_theme
})

# save_plotly_to_aws(
#   plotly::ggplotly(stages_plots_complete[[1]]) |>
#       plotly::layout(
#         margin = list(b = 140),
#         annotations = list(list(
#           text = gsub("\n", "<br>", stages_plotly_caption),
#           xref = "paper", yref = "paper",
#           x = 0, y = -0.05,
#           xanchor = "left", yanchor = "top",
#           showarrow = FALSE,
#           font = list(size = 10, color = "grey40"),
#           align = "left"
#         ))
#       ), 
#       "heatmap_funding_stages_complete_vss")

# save_plotly_to_aws(
#   plotly::ggplotly(stages_plots_complete[[2]]) |>
#       plotly::layout(
#         margin = list(b = 140),
#         annotations = list(list(
#           text = gsub("\n", "<br>", stages_plotly_caption),
#           xref = "paper", yref = "paper",
#           x = 0, y = -0.05,
#           xanchor = "left", yanchor = "top",
#           showarrow = FALSE,
#           font = list(size = 10, color = "grey40"),
#           align = "left"
#         ))
#       ),
#       "heatmap_funding_stages_complete_ss")

# save_plotly_to_aws(plotly::ggplotly(stages_plots_complete[[3]]) |>
#       plotly::layout(
#         margin = list(b = 140),
#         annotations = list(list(
#           text = gsub("\n", "<br>", stages_plotly_caption),
#           xref = "paper", yref = "paper",
#           x = 0, y = -0.05,
#           xanchor = "left", yanchor = "top",
#           showarrow = FALSE,
#           font = list(size = 10, color = "grey40"),
#           align = "left"
#         ))
#       ),
#       "heatmap_funding_stages_complete_ls")


# Question #2 ===========================================
# Of the small systems / VSS on the fundable list how many get any PF and of those that are expected to receive PF, what is the average (or median) project cost covered by PF;

pf_ft_data_clean_ss_complete_cases <- ft_data_clean_ss_complete_cases |>
  dplyr::filter(expecting_funding == "Yes") |>
  dplyr::mutate(
    pf_numeric = as.numeric(principal_forgiveness)
  ) |>
  dplyr::filter(!is.na(pf_numeric)) |>
  dplyr::filter(pf_numeric > 0) 

pf_ft_data_clean_ss_complete_cases_summary <- pf_ft_data_clean_ss_complete_cases |>
  dplyr::group_by(state, state_fiscal_year, system_size) |>
  dplyr::summarise(
    counts = n(),
    mean_pf = mean(pf_numeric),
    median_pf = median(pf_numeric),
    min_pf = min(pf_numeric),
    max_pf = max(pf_numeric)
  )
  
# save_csv_to_s3(pf_ft_data_clean_ss_complete_cases_summary, "ft_data_clean_ss_complete_cases_pf_summary", aws_bucket)


# Question #3 ===========================================
# How persistent are projects? 
# Dependent on Project ID and will be a smaller subset 
# – complete for 1 or 2 states that we can do all the way through EPA portal data (edited)

ft_agreement_crosswalk_ungrouped |>
  dplyr::filter(!project_id %in% c("No Information", "NoInformation", "No Info")) |>
  dplyr::filter(!is.na(project_id)) |>
  dplyr::filter(state %in% gd_complete_states) |>
  dplyr::group_by(state, project_id) |>
  dplyr::tally() |>
  dplyr::arrange(desc(n))

ft_agreement_crosswalk_ungrouped|>
  dplyr::filter(state %in% gd_complete_states) |>
  dplyr::filter(system_size == "VSS (<= 3,300)") |>
  dplyr::filter(state == "Illinois") |>
  dplyr::filter(!project_id %in% c("No Information", "NoInformation", "No Info")) |>
  dplyr::filter(!is.na(project_id)) |>
  dplyr::filter(has_agreement == "Yes") |>
  ggplot2::ggplot(aes(state_fiscal_year, project_id)) +
  geom_line(aes(group = project_id)) +
  geom_point(shape = 1, size = 3) +
  geom_point(aes(color = expecting_funding), shape = 16, size = 1) +
  epic_chart_theme
 

years_seen_info <-  ft_agreement_crosswalk_ungrouped|>
  dplyr::filter(state %in% gd_complete_states) |>
  dplyr::filter(state_fiscal_year %in% years_in_scope_complete) |>
  dplyr::filter(!project_id %in% c("No Information", "NoInformation", "No Info")) |>
  group_by(state, project_id, system_size) |>
  summarise(
    cohort_year = min(state_fiscal_year),
    years_seen  = paste(sort(unique(state_fiscal_year)), collapse = ", "),
    n_years     = n_distinct(state_fiscal_year),
    .groups = "drop"
  ) |>
  dplyr::arrange(desc(n_years))

save_csv_to_s3(years_seen_info |> dplyr::rename(base_year = cohort_year), "summary_persistence_complete", aws_bucket)


df_state <- ft_agreement_crosswalk_ungrouped|>
  dplyr::filter(state %in% gd_complete_states) |>
  dplyr::filter(state_fiscal_year %in% years_in_scope_complete) |>
  dplyr::filter(!project_id %in% c("No Information", "NoInformation", "No Info")) |>
  dplyr::left_join(years_seen_info) |>
  dplyr::arrange(match(project_id, years_seen_info$project_id), has_agreement) |>
  dplyr::mutate(
    system_size = forcats::fct(system_size, levels = target_cols)
  )

ggplot2::ggplot(df_state, aes(state_fiscal_year, project_id)) +
    geom_line(aes(group = project_id)) +
    geom_point(aes(size = n_years), shape = 1) +
    geom_point(aes(color = system_size), shape = 16, size = 1) +
    facet_wrap(~system_size, scales = "free_y") +
    scale_color_manual(values = system_size_palette) +
    epic_chart_theme

jitter_pos <- ggplot2::position_jitter(
  width = 0.3,
  height = 0.3,
  seed = 123
)

  # ggplot2::ggplot(df_state |> dplyr::filter(state == "New York"), aes(cohort_year, n_years)) +
  #   # geom_line(aes(group = project_id)) +
  #   geom_point(aes(size = has_agreement), shape = 1, color = "red",  position = jitter_pos) +
  #   geom_jitter(aes(color = system_size), shape = 16, size = 1,  position = jitter_pos) +
  #   facet_wrap(~system_size + has_agreement, scales = "free_y", ncol = 2) +
  #   scale_color_manual(values = system_size_palette) +
  #   scale_size_manual(values = c("Yes" = 2, "No" = 1)) +
  #   epic_chart_theme +
  #   labs(
  #     title = "NY Project persistence",  
  #     x = "First Year Seen",
  #     y = "Number of Years in a PPL"
  #   )

persistence_p <- ggplot2::ggplot(
    df_state |> dplyr::filter(has_agreement =="Yes"), aes(n_years)) +
    geom_histogram(
      aes(
        fill = system_size,
      text = paste0(
        "Years in PPL: ", after_stat(x),
        "<br>Number of Projects: ", after_stat(count)
      )
    ), bins = 4) +
    facet_grid(state~system_size) + 
    scale_fill_manual(values = system_size_palette) +
    epic_chart_theme +
    labs(
      title = "Project persistence",  
      x = "Number of Years in a PPL",
      y = "",
      fill = ""
    )

p_ly <- plotly::ggplotly(persistence_p,  tooltip = "text")

strip_annotations <- lapply(p_ly$x$layout$annotations, function(ann) {
  
  is_top_strip  <- !is.null(ann$y) && ann$y == 1 && is.null(ann$annotationType)
  is_side_strip <- !is.null(ann$x) && ann$x == 1 && !is.null(ann$textangle) && ann$textangle == 90
  
  if (is_top_strip) {
    ann$font <- list(size = 16, color = "grey20")
  }
  if (is_side_strip) {
    ann$font <- list(size = 16, color = "grey40")
  }
  ann
})

p_ly$x$layout$annotations <- strip_annotations

persistence_gp <- plotly::layout(
  p_ly,
  title = list(
    text = "Project persistence before reaching an agreement",
    font = list(size = 18, color = "grey40")
  ),
  margin = list(t = 70, b = 150, l = 30, r = 30),
  annotations = list(
      list(
        text = "Persistence assessed for SFY 2022-2025",
        xref = "paper", yref = "paper",
        x = 0, y = -0.15,
        xanchor = "left", yanchor = "top",
        showarrow = FALSE,
        font = list(size = 12, color = "grey40"),
        align = "left"
      )
    )
  )


persistence_gp

save_plotly_to_aws(persistence_gp, "project_persistence_has_agreement")


ggplot2::ggplot(df_state |> dplyr::filter(has_agreement =="Yes")|> dplyr::filter(expecting_funding=="Yes"), aes(n_years)) +
    geom_histogram(aes(fill = system_size), bins = 4) +
    facet_grid(state~system_size) + 
    scale_fill_manual(values = system_size_palette) +
    epic_chart_theme +
    labs(
      title = "Project Persistance in Fundable List",  
      x = "Number of Years in a PPL",
      y = ""
    ) 




# ft_agreement_crosswalk_ungrouped |>
#   dplyr::filter(!is.na(project_id)) |>
#   dplyr::filter(!project_id %in% c("No Information", "NoInformation", "No Info")) |>
#   dplyr::filter(state == "Illinois") |>
#   ggplot2::ggplot() +
#   geom_tile(aes(state_fiscal_year, project_id, fill = expecting_funding))



# id_state_sfy_ss_agreement <- ft_agreement_crosswalk_prep |>
#   dplyr::filter(has_agreement == "Yes") |>
#   dplyr::left_join(portal_agreement_crosswalk_prep |> dplyr::select(state, calendar_year, project_id) |> dplyr::distinct(), by = c("state", "project_id")) |>
#   dplyr::group_by(project_id, state) |>
#   dplyr::group_split()



# # list_of_dfs <- id_state_sfy_ss_agreement |> 
# #   dplyr::group_split() |> 
# #   purrr::set_names(
# #     id_state_sfy_ss_agreement |> dplyr::group_keys() |> dplyr::pull(state, )
# #   )
