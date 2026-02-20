clean_tx_y1 <- function() {
  
  # (267,14)
  tx_ppl <- data.table::fread("year1/TX/data/tx-y1-appendix-j.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # drop & rename columns for easier merge
    dplyr::select(-owner_type, -green_type, -related_pif_number_s, -gpr, -requested_phase_s) %>%
    dplyr::rename(project_cost = total_project_cost) %>%
    # fix inconsistent formatting for merge
    dplyr::mutate(
      project_description = stringr::str_squish(project_description),
      project_description = stringr::str_replace(project_description, "12- mile", "12-mile"),
      rank_test = as.numeric(rank)
    ) 
  
  tx_invite <- data.table::fread("year1/TX/data/tx-y1-appendix-k.csv",
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes"
    ) |>
    dplyr::select(pif_number, expecting_funding)
  
  tx_ppl <- tx_ppl |>
    dplyr::left_join(tx_invite, by="pif_number")
  
  tx_lead <- data.table::fread("year1/TX/data/tx-y1-appendix-i-lsl.csv",
                   colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::mutate(
      project_type = "Lead",
      disadvantaged = "Yes"
      ) |>
    dplyr::rename(project_cost = total_project_cost) |>
    dplyr::select(-requested_phase_s)

  tx_lead_invite <- data.table::fread("year1/TX/data/tx-y1-appendix-j-lsl.csv",
                   colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::mutate(
      project_type = "Lead",
      expecting_funding = "Yes"
    ) |>
    dplyr::rename(project_cost = total_project_cost) |>
    dplyr::select(-requested_phase_s)
  
  tx_ppl_lead <- tx_lead |>
    dplyr::left_join(tx_lead_invite)
# there is no pif number Joining with `by = join_by(rank, points, entity, pws_id, population, project_description, project_cost, project_type)`

  tx_ec <- data.table::fread("year1/TX/data/tx-y1-appendix-j-ec.csv",
                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_type = "Emerging Contaminants",
      disadvantaged = "Yes",
      expecting_funding = "No Information") |>
    dplyr::rename(project_cost = total_project_cost) |>
    dplyr::select(-requested_phase_s)
  
  tx_clean <- dplyr::bind_rows(tx_ppl, tx_ppl_lead, tx_ec) |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = stringr::str_squish(entity),
      pwsid = stringr::str_squish(pws_id),
      pwsid = replace_na(pwsid, "No Information"),
      project_id = stringr::str_squish(pif_number),
      project_id = tidyr::replace_na(project_id, "No Information"),
      project_name = as.character(NA),
      project_description = stringr::str_squish(project_description),
      project_type = dplyr::case_when(
        # ec and led docs already defined
        !is.na(project_type) ~ project_type,
        # search for keywords from full PPL, otherwise General project
        grepl(lead_str, project_description, ignore.case = TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      project_cost = clean_numeric_string(project_cost),
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      population = clean_numeric_string(population),
      # ec and lead docs already defined - if still NA and disavd_percent from PPL is NA, Not DAC
      disadvantaged = ifelse(is.na(disadv_percent) & is.na(disadvantaged), "No", "Yes"),     
      project_rank = stringr::str_squish(rank),     
      project_score = stringr::str_squish(points),
      expecting_funding = tidyr::replace_na(expecting_funding, "No"),
      state = "Texas",
      state_fiscal_year = "2023"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  ####### SANITY CHECKS START #######
  # Hone in on project id duplication
  #tx_clean |> dplyr::distinct() |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

  ####### Decision : No duplicates, 277 No Information

  # Check for disinfection byproduct in description
  #tx_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision : No disinfection byproduct string
  
  # Check for lead subtypes: Both
  # tx_clean |>
  #   dplyr::filter(project_type=="Lead") |>
  #   dplyr::mutate(
  #     lead_type = dplyr::case_when(
  #       stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
  #       stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
  #       stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
  #       # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
  #       stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
  #       stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
  #       TRUE ~ "unknown"
  #     )
  #   ) |>
  #   dplyr::filter(lead_type == "both")

  ####### Decision: 110 lead projects classified as both
  
  # Check for lead subtypes: Unknown
  # tx_clean |>
  #   dplyr::filter(project_type=="Lead") |>
  #   dplyr::mutate(
  #     lead_type = dplyr::case_when(
  #       stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
  #       stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
  #       stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
  #       # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
  #       stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
  #       stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
  #       TRUE ~ "unknown"
  #     )
  #   ) |>
  #   dplyr::filter(lead_type == "unknown") |> View()

  ####### Decision: 18 lead projects classified as unknown
  ####### SANITY CHECKS END #######
  
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}