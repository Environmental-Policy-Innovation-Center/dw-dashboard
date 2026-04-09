# view_code_review.R
# ─────────────────────────────────────────────────────────────────────────────
# Renders a self-contained code review HTML viewer and opens it in the browser.
#
# Usage (from a data frame):
#   my_data |>
#     dplyr::filter(call_label == "3_script_logic") |>
#     view_code_review()
#
# Usage (from a saved JSON file):
#   view_code_review("path/to/findings.json")
#
# Optional arguments:
#   title   – string shown in the browser tab / page heading
#   out     – path to write the HTML file (default: temp file, deleted on exit)
#   open    – whether to open the browser automatically (default: TRUE)
# ─────────────────────────────────────────────────────────────────────────────

view_code_review <- function(x,
                             title = "Code Review Findings",
                             out   = NULL,
                             open  = TRUE) {

  # ── 1. Resolve input to a JSON string ──────────────────────────────────────
  json <- if (is.data.frame(x)) {
    jsonlite::toJSON(x, auto_unbox = TRUE)

  } else if (is.character(x) && length(x) == 1 && file.exists(x)) {
    readLines(x, warn = FALSE) |> paste(collapse = "\n")

  } else if (is.character(x) && length(x) == 1) {
    # assume raw JSON string passed directly
    x

  } else {
    stop("`x` must be a data frame, a path to a JSON file, or a JSON string.")
  }

  # quick validation
  parsed <- tryCatch(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    error = function(e) stop("Could not parse JSON: ", e$message)
  )
  if (!is.list(parsed)) stop("JSON must be an array of objects.")

  # ── 2. Resolve output path ─────────────────────────────────────────────────
  if (is.null(out)) {
    out <- tempfile(pattern = "code_review_", fileext = ".html")
    if (open) on.exit(unlink(out), add = TRUE)   # clean up temp file after browser opens
  }

  # ── 3. Build self-contained HTML ───────────────────────────────────────────
  # Dynamic values are injected via simple gsub() substitution using unique
  # placeholders. This avoids glue/sprintf entirely, so CSS/JS curly braces
  # are never misinterpreted.
  escaped_title <- gsub("<", "&lt;", gsub("&", "&amp;", title))
  timestamp     <- format(Sys.time(), "%Y-%m-%d %H:%M")
  n_findings    <- as.character(length(parsed))

  # Raw string (R 4.0+): r"---(...)---" means R never processes any characters
  # inside, so JS regex syntax like /[.*+?^${}()|[\]\\]/g passes through untouched.
  # The "---" padding ensures the delimiter won't appear anywhere in HTML/CSS/JS.
  html <- r"---(<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>__TITLE__</title>
<style>
  * { box-sizing: border-box; }
  body { font-family: "Segoe UI", sans-serif; font-size: 13px; background: #f7f7f7;
         margin: 0; padding: 20px; color: #1a1a1a; }
  h2   { font-size: 15px; margin: 0 0 4px; font-weight: 600; }
  .subtitle { font-size: 12px; color: #888; margin-bottom: 16px; }
  .toolbar  { display: flex; align-items: center; gap: 12px; margin-bottom: 12px;
              flex-wrap: wrap; }
  .filters  { display: flex; gap: 6px; flex-wrap: wrap; }
  .filter-btn { padding: 4px 12px; border-radius: 20px; border: 1.5px solid #ccc;
                background: white; cursor: pointer; font-size: 12px; font-weight: 500; }
  .filter-btn.active { color: white; border-color: transparent; }
  .filter-btn[data-sev="ALL"].active     { background: #555; }
  .filter-btn[data-sev="ERROR"].active   { background: #c0392b; }
  .filter-btn[data-sev="WARNING"].active { background: #d68910; }
  .filter-btn[data-sev="INFO"].active    { background: #2980b9; }
  .search-wrap { margin-left: auto; }
  #search { padding: 4px 10px; border: 1.5px solid #ccc; border-radius: 20px;
            font-size: 12px; width: 200px; outline: none; }
  #search:focus { border-color: #2980b9; }
  .summary-chips { display: flex; gap: 8px; margin-bottom: 10px; flex-wrap: wrap; }
  .chip { display: flex; align-items: center; gap: 5px; padding: 3px 10px;
          border-radius: 12px; font-size: 11px; font-weight: 600;
          background: #f0f0f0; color: #555; }
  .chip .dot { width: 8px; height: 8px; border-radius: 50%; display: inline-block; }
  .dot-error   { background: #c0392b; }
  .dot-warning { background: #d68910; }
  .dot-info    { background: #2980b9; }
  .count { font-size: 12px; color: #777; margin-bottom: 8px; }
  table { width: 100%; border-collapse: collapse; font-size: 12px; background: #fff;
          border-radius: 8px; overflow: hidden; box-shadow: 0 1px 4px rgba(0,0,0,0.07); }
  th { background: #f4f4f4; text-align: left; padding: 8px 10px;
       border-bottom: 2px solid #ddd; font-weight: 600; }
  td { padding: 8px 10px; border-bottom: 1px solid #eee; vertical-align: top; }
  tr:last-child td { border-bottom: none; }
  tr:hover td { background: #fafafa; }
  .badge { display: inline-block; padding: 2px 8px; border-radius: 4px;
           font-size: 11px; font-weight: 700; color: white; white-space: nowrap; }
  .badge-ERROR   { background: #c0392b; }
  .badge-WARNING { background: #d68910; }
  .badge-INFO    { background: #2980b9; }
  .field-tag { font-family: monospace; font-size: 11px; background: #f0f0f0;
               padding: 1px 6px; border-radius: 3px; white-space: nowrap; }
  .suggestion { color: #555; font-style: italic; }
  mark { background: #fff176; border-radius: 2px; padding: 0 1px; }
  .empty { text-align: center; padding: 30px; color: #aaa; font-style: italic; }
  col.col-check { width: 28px; }
  col.col-num   { width: 32px; }
  col.col-sev   { width: 90px; }
  col.col-field { width: 130px; }
  col.col-issue { width: 42%; }
  .check-cell { text-align: center; vertical-align: middle; }
  .check-cell input[type=checkbox] { width: 15px; height: 15px; cursor: pointer;
                                     accent-color: #27ae60; margin: 0; }
  tr.reviewed td { opacity: 0.38; }
  tr.reviewed td.check-cell { opacity: 1; }
  .dot-reviewed { background: #27ae60; }
</style>
</head>
<body>

<h2>__TITLE__</h2>
<p class="subtitle">Generated __TIMESTAMP__ &nbsp;&middot;&nbsp; __NFINDINGS__ findings</p>

<div class="toolbar">
  <div class="filters" id="filters">
    <button class="filter-btn active" data-sev="ALL">All</button>
    <button class="filter-btn" data-sev="ERROR">ERROR</button>
    <button class="filter-btn" data-sev="WARNING">WARNING</button>
    <button class="filter-btn" data-sev="INFO">INFO</button>
  </div>
  <div class="search-wrap">
    <input id="search" type="text" placeholder="Search findings&#x2026;" oninput="render()">
  </div>
</div>

<div class="summary-chips" id="chips"></div>
<div class="count" id="count"></div>

<table>
  <colgroup>
    <col class="col-check"><col class="col-num"><col class="col-sev">
    <col class="col-field"><col class="col-issue"><col>
  </colgroup>
  <thead>
    <tr><th></th><th>#</th><th>Severity</th><th>Field</th><th>Issue</th><th>Suggestion</th></tr>
  </thead>
  <tbody id="tbody"></tbody>
</table>

<script>
const issues = __JSON__;
let activeSev = "ALL";

// ── Reviewed state (persisted to localStorage keyed by page title + index) ──
const STORE_KEY = "cr_reviewed___TITLE__";
let reviewed = new Set(JSON.parse(localStorage.getItem(STORE_KEY) || "[]"));

function saveReviewed() {
  localStorage.setItem(STORE_KEY, JSON.stringify([...reviewed]));
}

function buildChips() {
  const counts = { ERROR: 0, WARNING: 0, INFO: 0 };
  issues.forEach(i => { const s = (i.severity || "INFO").toUpperCase();
                         if (s in counts) counts[s]++; });
  const nReviewed = reviewed.size;
  document.getElementById("chips").innerHTML = [
    `<div class="chip"><span class="dot dot-error"></span>${counts.ERROR} error${counts.ERROR !== 1 ? "s" : ""}</div>`,
    `<div class="chip"><span class="dot dot-warning"></span>${counts.WARNING} warning${counts.WARNING !== 1 ? "s" : ""}</div>`,
    `<div class="chip"><span class="dot dot-info"></span>${counts.INFO} info</div>`,
    `<div class="chip"><span class="dot dot-reviewed"></span>${nReviewed} reviewed</div>`
  ].join("");
}

function esc(s) {
  return String(s).replace(/&/g,"&amp;").replace(/</g,"&lt;")
                  .replace(/>/g,"&gt;").replace(/"/g,"&quot;");
}
function highlight(text, q) {
  if (!q) return esc(text);
  const r = new RegExp(esc(q).replace(/[.*+?^${}()|[\]\\]/g,"\\$&"), "gi");
  return esc(text).replace(r, m => `<mark>${m}</mark>`);
}

function render() {
  const q = document.getElementById("search").value.trim().toLowerCase();
  const filtered = issues.map((item, idx) => ({ item, idx })).filter(({ item }) => {
    const sev = (item.severity || "INFO").toUpperCase();
    if (activeSev !== "ALL" && sev !== activeSev) return false;
    if (q) {
      const blob = [sev, item.field, item.issue, item.suggestion || ""]
                     .join(" ").toLowerCase();
      if (!blob.includes(q)) return false;
    }
    return true;
  });

  document.getElementById("count").textContent =
    `Showing ${filtered.length} of ${issues.length} findings`;

  const tbody = document.getElementById("tbody");
  if (filtered.length === 0) {
    tbody.innerHTML = `<tr><td colspan="6" class="empty">No findings match the current filters.</td></tr>`;
    return;
  }
  tbody.innerHTML = filtered.map(({ item, idx }) => {
    const sev   = (item.severity || "INFO").toUpperCase();
    const field = item.field || item.column || item.location || "&#x2014;";
    const sugg  = item.suggestion || item.fix || item.recommendation || null;
    const isReviewed = reviewed.has(idx);
    return `<tr class="${isReviewed ? "reviewed" : ""}" data-idx="${idx}">
      <td class="check-cell">
        <input type="checkbox" ${isReviewed ? "checked" : ""} data-idx="${idx}"
               onchange="toggleReviewed(${idx}, this.checked)">
      </td>
      <td>${idx + 1}</td>
      <td><span class="badge badge-${sev}">${sev}</span></td>
      <td><span class="field-tag">${highlight(field, q)}</span></td>
      <td>${highlight(item.issue || item.description || "", q)}</td>
      <td class="suggestion">${sugg ? highlight(sugg, q) : "<em style='color:#bbb'>&#x2014;</em>"}</td>
    </tr>`;
  }).join("");
}

function toggleReviewed(idx, checked) {
  if (checked) reviewed.add(idx); else reviewed.delete(idx);
  saveReviewed();
  // update just the row class rather than re-rendering the whole table
  const row = document.querySelector(`tr[data-idx="${idx}"]`);
  if (row) row.classList.toggle("reviewed", checked);
  buildChips();
}

document.getElementById("filters").addEventListener("click", e => {
  const btn = e.target.closest(".filter-btn");
  if (!btn) return;
  document.querySelectorAll(".filter-btn").forEach(b => b.classList.remove("active"));
  btn.classList.add("active");
  activeSev = btn.dataset.sev;
  render();
});

buildChips();
render();
</script>
</body>
</html>)---"

  # Inject dynamic values via unique placeholder substitution
  html <- gsub("__TITLE__",      escaped_title, html, fixed = TRUE)
  html <- gsub("__TIMESTAMP__",  timestamp,     html, fixed = TRUE)
  html <- gsub("__NFINDINGS__",  n_findings,    html, fixed = TRUE)
  html <- gsub("__JSON__",       json,          html, fixed = TRUE)

  writeLines(html, out)
  message("Code review viewer written to: ", out)

  if (open) utils::browseURL(paste0("file://", normalizePath(out)))

  invisible(out)
}
