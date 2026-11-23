clean_tx_y0 <- function() {

  tx_ppl <- read.csv("year0/TX/data/tx-dw-sfy22-ppl-invited.csv") %>%
    clean_names()
  
  # tx_ppl |> 
  #   dplyr::summarise(
  #     total_cost = sum(as.numeric(clean_numeric_string(project_cost)))
  #   )

  tx_ppl |>
    dplyr::filter(disadv == "70%") |>
    nrow()

  tx_clean <- tx_ppl %>%
    mutate(
      community_served = as.character(NA),
      borrower = str_squish(entity),
      pwsid = str_squish(pws_id),
      project_id = str_squish(pif),
      project_name = as.character(NA),
      project_type = case_when(
        # search for keywords from full PPL, otherwise General project
        grepl("lsl|lead", project_description, ignore.case = TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = clean_numeric_string(project_cost),
      requested_amount = as.character(NA),
      # funding_amount = ifelse(invited == "Yes", project_cost, "No Information"),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      project_description = str_squish(project_description),
      population = clean_numeric_string(population),
      disadvantaged = ifelse(disadv != "", "Yes", "No"),
      project_rank = str_squish(rank),
      project_score = str_squish(points),
      expecting_funding = ifelse(invited == "Yes", "Yes", "No"),
      state = "Texas",
      state_fiscal_year = "2022",
      list = "SFY22 General IUP"
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  # Check pwsid
  # tx_clean |> dplyr::filter(!pwsid == "No Information") |> dplyr::mutate(length_pwsid = stringr::str_length(pwsid)) |> dplyr::filter(!length_pwsid == 9)

  tx_clean <- tx_clean |>
    dplyr::mutate(pwsid = ifelse(pwsid %in% c("", "Pending"), "No Information", pwsid))

  ####### Decision : Empty and Pending --> No Information
  
  # Hone in on project id duplication
  # tx_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision : No project ids duplicates

  # Check for disinfection byproduct in description
  # tx_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
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

  ####### Decision: No lead projects classified as both
  
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
  #   dplyr::filter(lead_type == "unknown") 

  ### No lead unknown
  
  ####### SANITY CHECKS END #######  
  
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}
