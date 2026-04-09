# =============================================================================
# review_helpers.R
# Functions for Claude API-based data dictionary / data / script review
#
# Dependencies:
#   - pandoc (system, for docx text extraction)
#   - R packages: httr2, jsonlite, readr, readxl, dplyr, purrr, tibble, stringr, glue
#     Install once: install.packages(c("httr2","jsonlite","readr","readxl",
#                                      "dplyr","purrr","tibble","stringr","glue"))
# =============================================================================

library(httr2)
library(jsonlite)
library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(glue)


# =============================================================================
# SECTION 1: extract_docx_text() -----
# =============================================================================
#
# Extracts and structures content from a data dictionary .docx file using pandoc.
# Returns a named list with:
#   $raw_text    - full plain-text string (for caching as system context)
#   $tables      - list of data frames, one per table found in the document
#   $sections    - character vector of heading-level sections
#   $token_safe  - a compact, token-efficient string representation
#
# @param path       Path to .docx file
# @param max_chars  Soft cap on $token_safe string (default 12000 chars ~ 3k tokens)
# @param pandoc_bin Path to pandoc binary; NULL = auto-detect from PATH
# =============================================================================

extract_docx_text <- function(path,
                               max_chars  = 12000,
                               pandoc_bin = NULL) {

  stopifnot(file.exists(path))
  stopifnot(grepl("\\.docx?$", path, ignore.case = TRUE))

  # ── locate pandoc ────────────────────────────────────────────────────────────
  if (is.null(pandoc_bin)) {
    pandoc_bin <- Sys.which("pandoc")
    if (nchar(pandoc_bin) == 0) {
      stop(paste(
        "pandoc not found on PATH.",
        "Install from https://pandoc.org/installing.html",
        "or provide pandoc_bin= argument."
      ))
    }
  }

  # ── 1. Extract raw plain text via pandoc ─────────────────────────────────────
  tmp_plain <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp_plain), add = TRUE)

  ret_plain <- system2(
    pandoc_bin,
    args   = c(shQuote(path), "-t", "plain", "-o", shQuote(tmp_plain)),
    stdout = TRUE, stderr = TRUE
  )
  if (!file.exists(tmp_plain)) {
    stop(glue("pandoc failed to produce plain text output.\npandoc stderr:\n{paste(ret_plain, collapse='\n')}"))
  }
  raw_text <- paste(readLines(tmp_plain, warn = FALSE), collapse = "\n")

  # ── 2. Extract markdown (preserves table structure as pipe tables) ────────────
  tmp_md <- tempfile(fileext = ".md")
  on.exit(unlink(tmp_md), add = TRUE)

  system2(
    pandoc_bin,
    args   = c(shQuote(path), "-t", "markdown", "--wrap=none",
               "-o", shQuote(tmp_md)),
    stdout = FALSE, stderr = FALSE
  )
  md_lines <- if (file.exists(tmp_md)) readLines(tmp_md, warn = FALSE) else character(0)

  # ── 3. Parse sections from plain text ────────────────────────────────────────
  # pandoc plain-text output underlines headings with = or -
  sections <- .parse_plain_sections(raw_text)

  # ── 4. Parse tables from markdown ────────────────────────────────────────────
  tables <- .parse_md_tables(md_lines)

  # ── 5. Build token-safe compact representation ───────────────────────────────
  # Strategy: sections as headers, tables as compact CSV-ish blocks
  # Truncate to max_chars to stay within token budget
  token_safe <- .build_token_safe(sections, tables, raw_text, max_chars)

  list(
    raw_text   = raw_text,
    tables     = tables,
    sections   = sections,
    token_safe = token_safe
  )
}


# ── Internal helpers for extract_docx_text() ──────────────────────────────────

.parse_plain_sections <- function(raw_text) {
  lines <- strsplit(raw_text, "\n")[[1]]
  sections <- character(0)
  current  <- NULL

  for (i in seq_along(lines)) {
    line <- lines[[i]]
    # pandoc plain: heading text followed by line of === or ---
    if (i < length(lines)) {
      next_line <- lines[[i + 1]]
      if (grepl("^=+$", trimws(next_line)) && nchar(trimws(line)) > 0) {
        current <- trimws(line)
        sections <- c(sections, current)
        next
      }
      if (grepl("^-+$", trimws(next_line)) && nchar(trimws(line)) > 0) {
        current <- trimws(line)
        sections <- c(sections, current)
        next
      }
    }
    # Also catch ATX-style headings that pandoc sometimes outputs
    if (grepl("^#{1,6}\\s+", line)) {
      heading_text <- sub("^#{1,6}\\s+", "", line)
      sections <- c(sections, trimws(heading_text))
    }
  }
  unique(sections)
}


.parse_md_tables <- function(md_lines) {
  if (length(md_lines) == 0) return(list())

  tables     <- list()
  in_table   <- FALSE
  table_buf  <- character(0)
  table_idx  <- 0

  flush_table <- function() {
    if (length(table_buf) < 2) return()
    table_idx <<- table_idx + 1
    df <- tryCatch({
      # pipe-table rows: | col | col |
      rows <- table_buf[grepl("^\\s*\\|", table_buf)]
      # remove separator rows (|---|---|)
      rows <- rows[!grepl("^\\s*\\|[-:| ]+\\|\\s*$", rows)]
      if (length(rows) < 2) return()

      parse_row <- function(r) {
        # split on |, trim, drop empty first/last
        cells <- str_trim(str_split(r, "\\|")[[1]])
        cells[nchar(cells) > 0]
      }
      header <- parse_row(rows[[1]])
      data_rows <- lapply(rows[-1], parse_row)
      # keep only rows with same number of cells as header
      data_rows <- Filter(function(r) length(r) == length(header), data_rows)
      if (length(data_rows) == 0) return()

      df <- as.data.frame(
        do.call(rbind, data_rows),
        stringsAsFactors = FALSE
      )
      names(df) <- header
      df
    }, error = function(e) NULL)

    if (!is.null(df)) {
      tables[[table_idx]] <<- df
    }
  }

  for (line in md_lines) {
    is_table_line <- grepl("^\\s*\\|", line)
    if (is_table_line) {
      in_table  <- TRUE
      table_buf <- c(table_buf, line)
    } else if (in_table) {
      flush_table()
      in_table  <- FALSE
      table_buf <- character(0)
    }
  }
  if (in_table) flush_table()

  tables
}


.build_token_safe <- function(sections, tables, raw_text, max_chars) {
  parts <- character(0)

  # Sections block
  if (length(sections) > 0) {
    parts <- c(parts,
      "=== DOCUMENT SECTIONS ===",
      paste("-", sections),
      ""
    )
  }

  # Tables block — compact: field name + key columns only
  if (length(tables) > 0) {
    parts <- c(parts, "=== DATA DICTIONARY TABLES ===")
    for (i in seq_along(tables)) {
      df <- tables[[i]]
      parts <- c(parts, glue("--- Table {i} ({nrow(df)} rows x {ncol(df)} cols) ---"))
      # Header row
      parts <- c(parts, paste(names(df), collapse = " | "))
      # All data rows (token budget enforced at end)
      for (j in seq_len(nrow(df))) {
        parts <- c(parts, paste(as.character(df[j, ]), collapse = " | "))
      }
      parts <- c(parts, "")
    }
  }

  # If no tables parsed, fall back to raw text
  if (length(tables) == 0) {
    parts <- c(parts,
      "=== RAW DOCUMENT TEXT ===",
      raw_text
    )
  }

  result <- paste(parts, collapse = "\n")

  # Hard truncate with notice
  if (nchar(result) > max_chars) {
    result <- paste0(
      substr(result, 1, max_chars),
      "\n\n[... TRUNCATED — full document exceeds token_safe limit of ",
      max_chars, " chars. Increase max_chars= if needed. ...]"
    )
  }
  result
}


# =============================================================================
# SECTION 2: read_and_summarize() — data input summariser -----
# (used by build_review_calls, but also useful standalone)
# =============================================================================
#
# Reads a CSV or XLS/XLSX file and returns a compact summary:
#   $file_name, $n_rows, $n_cols, $col_summary (tibble), $head_rows (tibble)
#
# @param path      Path to CSV, XLS, or XLSX file
# @param n_head    Number of head rows to include (default 5)
# @param n_sample  Number of sample values per column (default 5)
# @param sheet     Sheet name/index for Excel files (default 1)
# =============================================================================

read_and_summarize <- function(path,
                                n_head   = 5,
                                n_sample = 5,
                                sheet    = 1) {
  stopifnot(file.exists(path))

  ext <- tolower(tools::file_ext(path))

  df <- tryCatch({
    if (ext == "csv") {
      readr::read_csv(path, show_col_types = FALSE, n_max = 10000)
    } else if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(path, sheet = sheet, n_max = 10000)
    } else {
      stop(glue("Unsupported file type: .{ext}. Supported: csv, xls, xlsx"))
    }
  }, error = function(e) {
    stop(glue("Failed to read {basename(path)}: {conditionMessage(e)}"))
  })

  # Column-level summary
  col_summary <- purrr::map_dfr(names(df), function(col) {
    vals <- df[[col]]
    non_na <- na.omit(vals)
    tibble::tibble(
      column      = col,
      r_type      = class(vals)[1],
      n_distinct  = dplyr::n_distinct(vals, na.rm = TRUE),
      pct_na      = round(mean(is.na(vals)) * 100, 1),
      n_rows_total = nrow(df),
      sample_vals = paste(
        head(unique(as.character(non_na)), n_sample),
        collapse = "; "
      ),
      min_val     = if (is.numeric(vals)) as.character(min(vals, na.rm = TRUE)) else NA_character_,
      max_val     = if (is.numeric(vals)) as.character(max(vals, na.rm = TRUE)) else NA_character_
    )
  })

  list(
    file_name   = basename(path),
    n_rows      = nrow(df),
    n_cols      = ncol(df),
    col_summary = col_summary,
    head_rows   = head(df, n_head)
  )
}


# Helper: format a data summary as a compact token-safe string
.format_data_summary <- function(summary_obj) {
  s <- summary_obj
  header <- glue(
    "FILE: {s$file_name}  |  {s$n_rows} rows x {s$n_cols} cols\n\n"
  )

  col_lines <- apply(s$col_summary, 1, function(r) {
    range_str <- if (!is.na(r["min_val"]))
      glue(" range=[{r['min_val']}, {r['max_val']}]") else ""
    glue("  {r['column']} | {r['r_type']} | {r['n_distinct']} distinct | ",
         "{r['pct_na']}% NA{range_str} | eg: {r['sample_vals']}")
  })

  head_str <- paste(
    capture.output(print(as.data.frame(s$head_rows), row.names = FALSE)),
    collapse = "\n"
  )

  paste0(
    header,
    "COLUMN SUMMARY:\n",
    paste(col_lines, collapse = "\n"),
    "\n\nHEAD ROWS:\n",
    head_str
  )
}


# =============================================================================
# SECTION 3: build_review_calls() -----
# =============================================================================
#
# Constructs a list of API call bodies for the four review passes.
# Each element: list(label = "...", body = list(...ready for httr2...))
#
# Call types produced:
#   1. col_names   — column name matching between dict and data
#   2. types_nulls — type/constraint vs. data type + NA rate
#   3. script_logic — cleaning script vs. dict expected transformations
#   4. script_data  — cleaning script vs. actual data sample (spot-check)
#
# @param dict       Output of extract_docx_text()
# @param script_text Character string of the R cleaning script
# @param data_list  List of outputs from read_and_summarize(), one per file
# @param model      Claude model string (default: claude-haiku for calls 1-2,
#                   claude-sonnet for calls 3-4)
# @param max_tokens Max output tokens per call ()
# =============================================================================

build_review_calls <- function(dict,
                                script_text,
                                data_list,
                                model_fast   = "claude-haiku-4-5-20251001",
                                model_reason = "claude-sonnet-4-6",
                                max_tokens   = 5000) {

  stopifnot(is.list(dict), !is.null(dict$token_safe))
  stopifnot(is.character(script_text), length(script_text) == 1)
  stopifnot(is.list(data_list), length(data_list) >= 1)

  # ── Shared system block: data dictionary (cacheable) ─────────────────────────
  dict_system_block <- list(
    list(
      type  = "text",
      text  = paste0(
        "You are a data quality reviewer. The data dictionary below defines ",
        "the expected columns, types, and rules for a dataset.\n\n",
        "DATA DICTIONARY:\n",
        dict$token_safe
      ),
      cache_control = list(type = "ephemeral")  # cache this across all calls
    )
  )

  # ── JSON schema instruction (appended to every user prompt) ──────────────────
  json_schema_instruction <- paste0(
    "\n\nRespond ONLY with a JSON array. No explanation, no markdown fences. Schema:\n",
    '[{"severity":"ERROR|WARNING|INFO",',
    '"field":"<column name or script line ref or null>",',
    '"issue":"<description>",',
    '"suggestion":"<fix or null>"}]',
    "\nIf no issues found, return an empty array: []"
  )

  # ── Helper: combine all data summaries when multiple files ───────────────────
  all_data_text <- paste(
    purrr::map_chr(data_list, .format_data_summary),
    collapse = "\n\n---\n\n"
  )

  # Container for all calls
  calls <- list()

  # ─────────────────────────────────────────────────────────────────────────────
  # CALL 1: Column name matching — dict fields vs. data column names
  # Model: fast (haiku) — pure name comparison, no reasoning needed
  # ─────────────────────────────────────────────────────────────────────────────
  data_col_names_text <- paste(
    purrr::map_chr(data_list, function(s) {
      glue("File: {s$file_name}\nColumns: {paste(s$col_summary$column, collapse=', ')}")
    }),
    collapse = "\n\n"
  )

  calls[["col_names"]] <- list(
    label = "1_col_names",
    model = model_fast,
    body  = list(
      model      = model_fast,
      max_tokens = max_tokens,
      system     = dict_system_block,
      messages   = list(list(
        role    = "user",
        content = paste0(
          "Compare the column names defined in the data dictionary against ",
          "the actual column names found in the data files below.\n",
          "Flag: missing columns, extra/unexpected columns, name mismatches ",
          "(case, spacing, abbreviation differences).\n\n",
          "DATA FILE COLUMNS:\n",
          data_col_names_text,
          json_schema_instruction
        )
      ))
    )
  )

  # ─────────────────────────────────────────────────────────────────────────────
  # CALL 2: Types, constraints, null rates
  # Model: fast (haiku) — structured comparison
  # ─────────────────────────────────────────────────────────────────────────────
  calls[["types_nulls"]] <- list(
    label = "2_types_nulls",
    model = model_fast,
    body  = list(
      model      = model_fast,
      max_tokens = max_tokens,
      system     = dict_system_block,
      messages   = list(list(
        role    = "user",
        content = paste0(
          "Review data types, null rates, and value constraints.\n",
          "For each column in the data file(s), check:\n",
          "  - Does the R type (character/numeric/date) match the dictionary's expected type?\n",
          "  - Does the null/NA rate violate any NOT NULL or required-field rules in the dict?\n",
          "  - Do sample values fall within expected ranges, categories, or formats?\n\n",
          "DATA FILE COLUMN SUMMARIES:\n",
          all_data_text,
          json_schema_instruction
        )
      ))
    )
  )

  # ─────────────────────────────────────────────────────────────────────────────
  # CALL 3: Cleaning script logic vs. data dictionary rules
  # Model: reasoning (sonnet) — requires code comprehension
  # Script is NOT cached (changes run-to-run); dict IS cached via system
  # ─────────────────────────────────────────────────────────────────────────────

  # Trim script: remove blank lines and pure-comment blocks to reduce tokens
  script_trimmed <- .trim_script(script_text)
 ##TODO step by step reasoning; ask about not reiterating warnings on #[keep] author notes
  calls[["script_logic"]] <- list(
    label = "3_script_logic",
    model = model_reason,
    body  = list(
      model      = model_reason,
      max_tokens = max_tokens,
      system     = dict_system_block,
      messages   = list(list(
        role    = "user",
        content = paste0(
          "Review the R cleaning script below against the data dictionary.\n",
          "Check for:\n",
          "  - Columns renamed incorrectly (does not match dict field names)\n",
          "  - Type coercions that conflict with dict types\n",
          "  - Filtering or dropping rows that may remove valid records\n",
          "  - Missing transformations required by the dictionary\n",
          "  - Hardcoded values (thresholds, categories) inconsistent with dict\n",
          "  - Columns in the dict that are never touched by the script\n\n",
          "R CLEANING SCRIPT:\n```r\n",
          script_trimmed,
          "\n```",
          json_schema_instruction
        )
      ))
    )
  )

  # ─────────────────────────────────────────────────────────────────────────────
  # CALL 4: Script behaviour on actual data values (spot-check)
  # Model: reasoning (sonnet) — needs to trace code over real samples
  # ─────────────────────────────────────────────────────────────────────────────
  calls[["script_data"]] <- list(
    label = "4_script_data",
    model = model_reason,
    body  = list(
      model      = model_reason,
      max_tokens = max_tokens,
      system     = dict_system_block,
      messages   = list(list(
        role    = "user",
        content = paste0(
          "Spot-check the cleaning script against the actual data values below.\n",
          "Focus on:\n",
          "  - String patterns in the data that the script's regex/parsing may break on\n",
          "  - Numeric values near thresholds that could be misclassified\n",
          "  - Date formats in the sample vs. what the script assumes\n",
          "  - Any column where sample values look inconsistent with how the script handles them\n\n",
          "R CLEANING SCRIPT:\n```r\n",
          script_trimmed,
          "\n```\n\n",
          "DATA SAMPLES:\n",
          all_data_text,
          json_schema_instruction
        )
      ))
    )
  )

  calls
}


# ── Internal: strip blank lines and comment blocks from R script ──────────────
# Comments marked with # [keep] are retained and wrapped with a framing note
# so the LLM understands they are intentional author context, not code.
#
# Example usage in a cleaning script:
#   # [keep] project_details column absent in CSV; DISADV. flag used instead
#   # [keep] DG confirmed: use first requested_funding value only (2024-01-15)
# ─────────────────────────────────────────────────────────────────────────────
.trim_script <- function(script_text) {
  lines <- strsplit(script_text, "\n")[[1]]

  is_blank       <- grepl("^\\s*$", lines)
  is_kept_comment <- grepl("^\\s*#\\s*\\[keep\\]", lines, ignore.case = TRUE)
  is_pure_comment <- grepl("^\\s*#", lines) & !is_kept_comment

  # Rewrite [keep] lines into a form the LLM will interpret as author context
  lines[is_kept_comment] <- stringr::str_replace(
    lines[is_kept_comment],
    "^(\\s*)#\\s*\\[keep\\]\\s*",
    "\\1# [AUTHOR NOTE] "
  )

  kept <- lines[!(is_pure_comment | is_blank)]
  paste(kept, collapse = "\n")
}


# =============================================================================
# SECTION 4: Quick-use wrapper -----
# =============================================================================
#
# run_review() ties everything together.
# Returns a list with $findings (tibble) and $api_log (tibble).
# Also writes logs/review_findings.csv and logs/api_calls.csv.
#
# @param dict_path     Path to .docx data dictionary
# @param data_paths    Character vector of CSV/XLS/XLSX paths
# @param script_path   Path to R cleaning script
# @param log_dir       Directory for log files (created if absent)
# @param call_delay    Seconds to wait between API calls (default random 2-4s)
# =============================================================================

run_review <- function(dict_path,
                        data_paths,
                        script_path,
                        log_dir    = "logs",
                        call_delay = NULL) {

  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

  message("Extracting data dictionary...")
  dict <- extract_docx_text(dict_path)
  message(glue("  -> {length(dict$tables)} table(s), {length(dict$sections)} section(s) found"))

  message("Reading and summarising data files...")
  data_list <- purrr::map(data_paths, function(p) {
    message(glue("  -> {basename(p)}"))
    read_and_summarize(p)
  })

  message("Reading cleaning script...")
  script_text <- readr::read_file(script_path)

  message("Building review calls...")
  calls <- build_review_calls(dict, script_text, data_list)

  # Storage
  all_findings <- list()
  api_rows     <- list()

  message(glue("Running {length(calls)} review call(s)...\n"))

  for (call in calls) {
    message(glue("[{call$label}] Calling Claude ({call$model})..."))

    resp <- call_claude_with_retry(call$body)

    # ── Token logging ──
    usage <- resp$usage
    api_row <- tibble::tibble(
      timestamp            = Sys.time(),
      call_label           = call$label,
      model                = call$model,
      input_tokens         = usage$input_tokens %||% NA_integer_,
      output_tokens        = usage$output_tokens %||% NA_integer_,
      cache_creation_tokens = usage$cache_creation_input_tokens %||% 0L,
      cache_read_tokens    = usage$cache_read_input_tokens %||% 0L,
      stop_reason          = resp$stop_reason %||% NA_character_
    )
    api_rows[[call$label]] <- api_row
    message(glue(
      "  in={api_row$input_tokens} out={api_row$output_tokens} ",
      "cache_hit={api_row$cache_read_tokens} stop={api_row$stop_reason}"
    ))

    # ── Parse findings ──
    raw_text <- paste(
      purrr::map_chr(resp$content, function(b) if (b$type == "text") b$text else ""),
      collapse = ""
    )

    findings_df <- tryCatch({
      clean_json <- stringr::str_trim(raw_text)
      clean_json <- stringr::str_remove(clean_json, "^```json\\s*")
      clean_json <- stringr::str_remove(clean_json, "\\s*```$")
      parsed <- jsonlite::fromJSON(clean_json, simplifyDataFrame = TRUE)
      if (length(parsed) == 0) {
        tibble::tibble(
          call_label = call$label,
          severity   = "INFO",
          field      = NA_character_,
          issue      = "No issues found.",
          suggestion = NA_character_
        )
      } else {
        tibble::as_tibble(parsed) |>
          dplyr::mutate(call_label = call$label, .before = 1)
      }
    }, error = function(e) {
      message(glue("  WARNING: Could not parse JSON response for {call$label}"))
      tibble::tibble(
        call_label = call$label,
        severity   = "PARSE_ERROR",
        field      = NA_character_,
        issue      = raw_text,
        suggestion = NA_character_
      )
    })

    all_findings[[call$label]] <- findings_df

    # ── Delay between calls ──
    delay <- if (!is.null(call_delay)) call_delay else runif(1, 2, 4)
    Sys.sleep(delay)
  }

  # ── Combine and write logs ──
  findings_combined <- dplyr::bind_rows(all_findings)
  api_log_combined  <- dplyr::bind_rows(api_rows)

  findings_path <- file.path(log_dir, "review_findings.csv")
  api_log_path  <- file.path(log_dir, "api_calls.csv")

  readr::write_csv(findings_combined, findings_path)
  readr::write_csv(api_log_combined,  api_log_path)

  message(glue("\nDone. {nrow(findings_combined)} finding(s) logged to {findings_path}"))
  message(glue("API usage log: {api_log_path}"))

  invisible(list(
    findings = findings_combined,
    api_log  = api_log_combined
  ))
}


# =============================================================================
# SECTION 5: call_claude_with_retry() -----
# (reproduced here for completeness — matches the pattern from the strategy doc)
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

call_claude_with_retry <- function(body,
                                    max_attempts = 5,
                                    api_key      = Sys.getenv("ANTHROPIC_API_KEY")) {

  if (nchar(api_key) == 0) {
    stop("ANTHROPIC_API_KEY not set. Use Sys.setenv(ANTHROPIC_API_KEY='sk-...')")
  }

  attempt <- 1L

  while (attempt <= max_attempts) {
    resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
      httr2::req_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json",
        "anthropic-beta"    = "prompt-caching-2024-07-31"
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(resp)

    if (status == 200L) {
      return(httr2::resp_body_json(resp))
    }

    if (status == 429L) {
      retry_after <- suppressWarnings(
        as.numeric(httr2::resp_header(resp, "retry-after"))
      )
      retry_after <- if (is.na(retry_after)) 2^attempt else retry_after
      wait <- retry_after + runif(1, 0, 1)
      message(glue(
        "Rate limited (attempt {attempt}/{max_attempts}). ",
        "Waiting {round(wait, 1)}s..."
      ))
      Sys.sleep(wait)
      attempt <- attempt + 1L

    } else if (status == 529L) {
      wait <- 30 * 2^(attempt - 1L) + runif(1, 0, 5)
      message(glue(
        "API overloaded (attempt {attempt}/{max_attempts}). ",
        "Waiting {round(wait, 1)}s..."
      ))
      Sys.sleep(wait)
      attempt <- attempt + 1L

    } else {
      err_body <- tryCatch(
        httr2::resp_body_string(resp),
        error = function(e) "<unreadable>"
      )
      stop(glue("Claude API error {status}:\n{err_body}"))
    }
  }

  stop(glue("Exceeded max retry attempts ({max_attempts}) due to rate limiting or overload."))
}