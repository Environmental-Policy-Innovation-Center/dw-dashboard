clean_tx_y4 <- function() {
  base_path <- file.path("year4", "TX", "data")
  
  # this includes all projects (Comprehensive List)
  tx_ppl <- data.table::fread(file.path(base_path, "tx-y4-iup-appendix-j.csv"),
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names()
  
  # expecting funding projects (Fundable List)
  tx_invite <- data.table::fread(file.path(base_path, "tx-y4-iup-appendix-k.csv"),
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = clean_numeric_string(eligible_project_cost),
      expecting_funding = "Yes"
    ) |>
    dplyr::select(pif_number, funding_amount, expecting_funding)

  tx_lslr <- data.table::fread(file.path(base_path, "tx-y4-lslr-iup-appendix-j.csv"),
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      project_type = "Lead",
      funding_amount = "No Information",
      disadvantaged = "Yes"
  ) |>
    dplyr::rename(
      pif_number = pif_no,
      pws_id = pws_id_no,
      population = population_served
    ) 
 
  
  # tx_lslr$pif_number %in% tx_ppl$pif_number
  # tx_invite$pif_number %in% tx_ppl$pif_number

  combined_lists<- tx_ppl |>
    dplyr::left_join(tx_invite, by = "pif_number") |>
    dplyr::bind_rows(tx_lslr)

 
  # join invited by project id and then process for output
  tx_clean <-  combined_lists |>
    dplyr::mutate(
           community_served = as.character(NA),
           borrower = str_squish(entity),
           pwsid = str_squish(pws_id),
           pwsid = replace_na(pwsid, "No Information"),
           project_id = str_squish(pif_number),
           project_name = replace_na(project_name, "No Information"),
           project_cost = clean_numeric_string(total_project_cost),
           requested_amount = as.character(NA),
           funding_amount = replace_na(funding_amount, "No Information"),
           principal_forgiveness = as.character(NA),
           population = clean_numeric_string(population),
           project_description = str_squish(project_description),
           project_rank = str_squish(rank),
           project_score = str_squish(points),
           project_type = case_when(
            !is.na(project_type) ~ project_type,
             grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"), 
           disadvantaged = dplyr::case_when(
            !is.na(disadvantaged) ~ disadvantaged,
            is.na(disadv_percent) ~ "No",
            .default = "Yes"
           ),
           project_id = replace_na(project_id, "No Information"),
           expecting_funding = replace_na(expecting_funding, "No"),
           state = "Texas",
           state_fiscal_year = "2026"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
tx_clean |> dplyr::distinct() |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

####### Decision : No duplicates

# Check for disinfection byproduct in description
tx_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision : No disinfection byproduct string
  
# Check for lead subtypes: Both
  tx_clean |>
    dplyr::filter(project_type=="Lead") |>
    dplyr::mutate(
      lead_type = dplyr::case_when(
        stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
        stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
        stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
        # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
        stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
        stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::filter(lead_type == "both")

  ####### Decision: No lead projects classified as both
  
  # Check for lead subtypes: Unknown
  tx_clean |>
    dplyr::filter(project_type=="Lead") |>
    dplyr::mutate(
      lead_type = dplyr::case_when(
        stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
        stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
        stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
        # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
        stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
        stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::filter(lead_type == "unknown")

####### SANITY CHECKS END #######
  
  # Run validation tests
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}
