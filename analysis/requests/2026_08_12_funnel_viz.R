# Load libraries ===========
library(plotly)
library(crosstalk)
# Helper functions ===========

# Build trapezoid / triangle polygons for each stage x fiscal_year
#    Geometry rule: stage i is drawn from y = (4 - i) to y = (4 - i + 1),
#    i.e. Universe is the top band, Agreement is the bottom band.
#    - top width of the shape  = scaled total of stage i
#    - bottom width of the shape = scaled total of stage i+1
#      (so the taper visually shows the drop-off into the next stage)
#    - the LAST stage (Agreement) has bottom width = 0 -> inverted triangle
# ---------------------------------------------------------------------------

build_funnel_geometry <- function(df, max_half_width = 4, normalize = c("per_year", "global")) {
  normalize <- match.arg(normalize)

  stage_totals <- df %>%
    mutate(funnel_stage = factor(funnel_stage, levels = STAGE_LEVELS)) %>%
    group_by(fiscal_year, funnel_stage) %>%
    summarise(total = sum(n), .groups = "drop") %>%
    arrange(fiscal_year, funnel_stage)

  # scaling reference: either each year scaled to its own Universe total,
  # or all years scaled to the single largest Universe total (comparable widths)
  ref <- stage_totals %>%
    group_by(fiscal_year) %>%
    summarise(ref_total = max(total), .groups = "drop")

  if (normalize == "global") {
    ref$ref_total <- max(ref$ref_total)
  }

  stage_totals <- stage_totals %>%
    left_join(ref, by = "fiscal_year") %>%
    mutate(half_width = max_half_width * sqrt(total / ref_total)) %>%
    arrange(fiscal_year, funnel_stage) %>%
    group_by(fiscal_year) %>%
    mutate(
      stage_idx  = as.integer(funnel_stage),
      y_top      = length(STAGE_LEVELS) - stage_idx + 1,
      y_bottom   = y_top - 1,
      hw_top     = half_width,
      hw_bottom  = lead(half_width, default = 0)  # last stage tapers to a point
    ) %>%
    ungroup()

  # explode each stage into 4 polygon vertices (trapezoid; triangle when hw_bottom = 0)
  polygons <- stage_totals %>%
    rowwise() %>%
    do({
      d <- .
      data.frame(
        fiscal_year  = d$fiscal_year,
        funnel_stage = d$funnel_stage,
        x = c(-d$hw_top, d$hw_top, d$hw_bottom, -d$hw_bottom),
        y = c(d$y_top, d$y_top, d$y_bottom, d$y_bottom),
        vertex_order = 1:4
      )
    }) %>%
    ungroup() %>%
    mutate(funnel_stage = factor(funnel_stage, levels = STAGE_LEVELS))

  list(polygons = polygons, stage_totals = stage_totals)
}

# Given the stage geometry, scatter dots inside each stage's shape,
# Grouped into 3 left-to-right zones by size_category, colored by size.
# `dot_scale` = how many records each dot represents (e.g. 10 -> 1 dot/10 records)
# ---------------------------------------------------------------------------
build_funnel_dots <- function(df, stage_totals, dot_scale = 1, min_dots = 1, seed = 1) {
  set.seed(seed)

  df <- df %>%
    mutate(
      funnel_stage  = factor(funnel_stage, levels = STAGE_LEVELS),
      size_category = factor(size_category, levels = SIZE_LEVELS)
    ) %>%
    left_join(
      stage_totals %>% select(fiscal_year, funnel_stage, y_top, y_bottom, hw_top, hw_bottom),
      by = c("fiscal_year", "funnel_stage")
    ) %>%
    mutate(n_dots = pmax(min_dots, round(n / dot_scale)))

  dot_rows <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    d <- df[i, ]
    n_dots <- d$n_dots
    if (n_dots <= 0) next

    zone_idx  <- match(as.character(d$size_category), SIZE_LEVELS)  # 1=left,2=mid,3=right
    zone_lo   <- (zone_idx - 1) / 3
    zone_hi   <- zone_idx / 3

    margin_y <- 0.06 * (d$y_top - d$y_bottom)
    y_vals <- runif(n_dots, d$y_bottom + margin_y, d$y_top - margin_y)

    # local shape half-width at a given y (linear interpolation top->bottom)
    frac <- (y_vals - d$y_bottom) / (d$y_top - d$y_bottom)
    local_hw <- d$hw_bottom + frac * (d$hw_top - d$hw_bottom)

    # margin so dots don't sit flush against the funnel edge
    inner_margin <- 0.08 * pmax(local_hw, 0.3)
    left_edge  <- -local_hw + inner_margin
    right_edge <-  local_hw - inner_margin
    width      <- pmax(right_edge - left_edge, 0.01)

    x_lo <- left_edge + zone_lo * width
    x_hi <- left_edge + zone_hi * width
    x_vals <- runif(n_dots, x_lo, x_hi)

    dot_rows[[i]] <- data.frame(
      fiscal_year   = d$fiscal_year,
      funnel_stage  = d$funnel_stage,
      size_category = d$size_category,
      x = x_vals,
      y = y_vals
    )
  }

  bind_rows(dot_rows)
}

# ---------------------------------------------------------------------------
# Plot in native plot_ly
#    grouped into 3 left-to-right zones by size_category, colored by size.
#    `dot_scale` = how many records each dot represents (e.g. 10 -> 1 dot/10 records)
# ---------------------------------------------------------------------------
make_funnel <- function(fy, show_legend = FALSE) {

  poly_fy <- geo$polygons |>
    dplyr::filter(fiscal_year == fy)

  dots_fy <- dots |>
    dplyr::filter(fiscal_year == fy)

  fig <- plotly::plot_ly()

  fig <- fig |>
    plotly::add_polygons(
      data = poly_fy,
      x = ~x,
      y = ~y,
      line = list(color = "gray", width = 2),
      fillcolor = "transparent",
      showlegend = FALSE,
      inherit = FALSE,
      hoverinfo = "none" 
    ) |>
    plotly::add_polygons(
      data = poly_fy,
      x = ~-x,
      y = ~y,
      line = list(color = "gray", width = 2),
      fillcolor = "transparent",
      showlegend = FALSE,
      inherit = FALSE,
      hoverinfo = "none" 
    )

  for (cat in names(system_size_palette)) {

    d <- dots_fy |>
      dplyr::filter(size_category == cat)

    shared_data <- SharedData$new(d, key = ~slot_id, group = "shared_category")

    fig <- fig |>
      plotly::add_markers(
        data = shared_data,
        x = ~x,
        y = ~y,
        name = cat,
        legendgroup = cat,
        showlegend = show_legend,
        marker = list(
          color = system_size_palette[[cat]]
        ),
        inherit = FALSE,
        hovertext = ~plt_str,
        hoverinfo = "text"
      )
  }

  fig |>
    plotly::layout(
      xaxis = list(
        title = "",
        showticklabels = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "",
        showticklabels = FALSE,
        zeroline = FALSE
      )
    )
}

generate_dfs <- function(processing_state) {

  state_count <- grouped_data_complete |> 
    tidyr::pivot_longer(cols = target_cols, names_to = "size_category", values_to = "n") |> 
    dplyr::filter(state == processing_state) |>
    dplyr::mutate(
      fiscal_year = paste0("SFY", str_sub(state_fiscal_year, start = -2)),
      funnel_stage = category
    )

  fund_count <- ft_agreement_crosswalk_ungrouped |>
    dplyr::filter(state == processing_state) |>
    dplyr::filter(expecting_funding == "Yes") |>
    dplyr::select(project_id, state_fiscal_year, system_size) |>
    dplyr::mutate(
      funnel_stage = "Fundable List",
      fiscal_year = paste0("SFY", stringi::stri_sub(state_fiscal_year, -2))
    )

  comp_count <- ft_agreement_crosswalk_ungrouped |>
    dplyr::filter(state == processing_state) |>
    dplyr::select(project_id, state_fiscal_year, system_size) |>
    dplyr::mutate(
      funnel_stage = "Comprehensive List",
      fiscal_year = paste0("SFY", stringi::stri_sub(state_fiscal_year, -2))
    )

  agre_count <- ft_agreement_crosswalk_ungrouped |>
    dplyr::filter(state == processing_state) |>
    dplyr::filter(has_agreement == "Yes") |>
    dplyr::select(project_id, state_fiscal_year, system_size) |>
    dplyr::mutate(
      funnel_stage = "Agreement",
      fiscal_year = paste0("SFY", stringi::stri_sub(state_fiscal_year, -2))
    )

  complete_project_id_pre <- rbind(fund_count, comp_count, agre_count) |> 
    dplyr::filter(state_fiscal_year %in% years_in_scope_complete) |>
    dplyr::rename(size_category = system_size)

  geo <- build_funnel_geometry(state_count)
  dots <- build_funnel_dots(state_count, geo$stage_totals) 

  dots_indexed <- dots |>
    dplyr::group_by(fiscal_year, funnel_stage, size_category) |>
    dplyr::mutate(slot_id = dplyr::row_number()) |>
    dplyr::ungroup()

  projects_indexed <- complete_project_id_pre |>
    dplyr::group_by(fiscal_year, funnel_stage, size_category) |>
    dplyr::mutate(slot_id = dplyr::row_number()) |>
    dplyr::ungroup()

  result <- dots_indexed |>
    dplyr::left_join(
      projects_indexed,
      by = c("fiscal_year", "funnel_stage", "size_category", "slot_id")
    )

  summary_by_stage_year <- result |>
    dplyr::group_by(funnel_stage, fiscal_year, size_category) |>
    dplyr::tally(name = "count") |>
    dplyr::ungroup()

  summary_by_year <- result |> 
    dplyr::group_by(funnel_stage, fiscal_year) |> 
    dplyr::tally(name = "total")|>
    dplyr::ungroup()
    
  full_summary <- summary_by_stage_year |>
    dplyr::left_join(summary_by_year) |>
    dplyr::mutate(perc = round(100*count/total, 1)) |>
    dplyr::mutate(
      text_summary = paste0(count, " [",size_category , "] of ", total, " (", perc, "%)")
    )

  dots <- result |>
    dplyr::left_join(full_summary) |>
    dplyr::mutate(
      plt_str = ifelse(
        funnel_stage == "Universe", 
        paste0(funnel_stage, "\n", text_summary),
        paste0(funnel_stage, " (project_id): ", project_id, "\n", text_summary)
    ))
  
  return(list(geo = geo, dots = dots))
}

# Constants ===========
STAGE_LEVELS <- c("Universe", "Comprehensive List", "Fundable List", "Agreement")
SIZE_LEVELS  <- c("VSS (<= 3,300)", "SS (3,301-10,000)", "LS (>10,000)")

system_size_palette <- c(
  "VSS (<= 3,300)"    = "#4C0D4A",
  "SS (3,301-10,000)" = "#1054A8",
  "LS (>10,000)"      = "#DE9D29"
)

stages_plotly_caption <- "Data Sources: \nUniverse (U): EPA SABs, note that the data reflects community water systems only\nComprehensive (C) and Fundable (F) Lists: Funding Tracker Standardization of DWSRF IUP PPLs; note that projects can persist across SFYs\nAgreements (A): EPA DWSRF Portal; note that agreements are usually not reached in the SFY, and they are depicted for comparison purposes"

## Build PLOTS -----

for (i in seq_along(gd_complete_states)) {

processing_state <- gd_complete_states[i]
df_list_1 <- generate_dfs(processing_state)

geo <- df_list_1$geo
dots <- df_list_1$dots

fiscal_years <- sort(unique(dots$fiscal_year))

funnels <- purrr::map2(
  fiscal_years,
  seq_along(fiscal_years),
  \(fy, i) make_funnel(
    fy,
    show_legend = i == 1
  )
)

fig_all <- plotly::subplot(
  funnels,
  nrows = 1,
  shareY = TRUE,
  margin = 0.02
) |>
  plotly::layout(
    margin = list(
      t = 70,   # room for SFY labels
      b = 150,  # room for multi-line caption
      l = 30,
      r = 30
    ),
    annotations = list(
      list(
        text = "SFY22",
        x = 0, y = 4.2,
        xref = "x1", yref = "y1",
        showarrow = FALSE,
        font = list(
          size = 14
        )
      ),
      list(
        text = "SFY23",
        x = 0, y = 4.2,
        xref = "x2", yref = "y",
        showarrow = FALSE,
        font = list(
          size = 14
        )
      ),
      list(
        text = "SFY24",
        x = 0, y = 4.2,
        xref = "x3", yref = "y",
        showarrow = FALSE,
        font = list(
          size = 14
        )
      ),
      list(
        text = "SFY25",
        x = 0, y = 4.2,
        xref = "x4", yref = "y",
        showarrow = FALSE,
        font = list(
          size = 14
        )
      ),
      list(
          text = "U",
          x = -4, y = 3.5,
          xref = "x", yref = "y",
          showarrow = FALSE,
          font = list(
            size = 14,
            color = "#2c3e50",
            family = "Arial Black"   # bold-looking font
          )
        ),
        list(
          text = "C",
          x = -4, y = 2.5,
          xref = "x", yref = "y",
          showarrow = FALSE,
          font = list(
            size = 14,
            color = "#2c3e50",
            family = "Arial Black"   # bold-looking font
          )
        ),
        list(
          text = "F",
          x = -4, y = 1.5,
          xref = "x", yref = "y",
          showarrow = FALSE,
          font = list(
            size = 14,
            color = "#2c3e50",
            family = "Arial Black"   # bold-looking font
          )
        ),
        list(
          text = "A",
          x = -4, y = 0.5,
          xref = "x", yref = "y",
          showarrow = FALSE,
          font = list(
            size = 14,
            color = "#2c3e50",
            family = "Arial Black"   # bold-looking font
          )
        ),
        list(
          text = paste0("Funding Funnel for: ", processing_state),
          xref = "paper", yref = "paper",
          x = 0, y = 1.1,
          xanchor = "left", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 18, color = "grey40"),
          align = "left"
        ),

        list(
          text = gsub("\n", "<br>", stages_plotly_caption),
          xref = "paper", yref = "paper",
          x = 0, y = -0.05,
          xanchor = "left", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 12, color = "grey40"),
          align = "left"
        )
    )
  ) |>
  plotly::highlight(
    on = "plotly_click",
    # off = "plotly_doubleclick",
    color = "red"
    # ,
    # selected = attrs_selected(opacity = 1),
    # opacityDim = 0.2   # dims everything else that's not selected
  )

fig_all

save_plotly_to_aws(fig_all, paste0("funding_funnel_", str_remove(processing_state, "\\s")))
}
