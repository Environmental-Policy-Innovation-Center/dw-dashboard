clean_tn_y4 <- function() {
  
  tn_ppl_base <- data.table::fread("year4/TN/data/TN_SFY26_base_PPL.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "base"
    )

  tn_ppl_base_unranked <- data.table::fread("year4/TN/data/TN_SFY26_base_PPL_unranked.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "base unranked"
    )

  tn_ppl_ec <- data.table::fread("year4/TN/data/TN_SFY26_EC_PPL.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_type = "Emerging Contaminants",
      list = "ec"
    )
  
  tn_ppl <- dplyr::bind_rows(tn_ppl_base, tn_ppl_base_unranked, tn_ppl_ec)

  tn_clean <- tn_ppl |>
    dplyr::mutate(community_served = str_squish(county),
           borrower = str_squish(local_government),
           pwsid = str_squish(pwsid_number),
           pwsid = str_replace(pwsid, "NA", "No Information"),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_description = str_squish(project_description), 
           project_type = case_when(
            !is.na(project_type) ~ project_type,
             grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"), 
           project_cost = clean_numeric_string(total_project_amount),
           requested_amount = as.character(NA),
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           population = clean_numeric_string(pop_served),
           disadvantaged = dplyr::case_when(
            list == "base" | list == "base unranked" ~ ifelse(grepl("\\*", local_government), "Yes", "No"),
            list == "ec" & as.numeric(atpi) <= 50 ~ "Yes",
            .default = "No"
           ),
           project_rank = str_squish(rank_order),
           project_rank = replace_na(project_rank, "No Information"),
           project_score = str_squish(priority_points),
           project_score = replace_na(project_score, "No Information"),
           expecting_funding = dplyr::case_when(
            list == "ec" ~ "Yes",
            list == "base" ~"Yes",
            .default = "No"
           ), 
           state = "Tennessee",
           state_fiscal_year = "2026") |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  tn_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
  
  # Check for lead subtypes: Both
  tn_clean |>
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
  tn_clean |>
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

  ####### Decision: No lead projects classified as unknown


  ####### SANITY CHECKS END #######
  
  
  run_tests(tn_clean)
  rm(list=setdiff(ls(), "tn_clean"))
  
  return(tn_clean)
}