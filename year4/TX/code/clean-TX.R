clean_tx_y4 <- function() {
  base_path <- file.path("year4", "TX", "data")
  
  # General -----
  ## this includes all projects (Comprehensive List) ----
  tx_ppl <- data.table::fread(file.path(base_path, "tx-y4-iup-appendix-j.csv"),
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY26 General Comprehensive List",
      project_cost = clean_numeric_string(total_project_cost),
      disadvantaged = ifelse(!is.na(disadv_percent), "Yes", "No")
    )
  
  ## appendix I ----
  tx_ineligible_dis <- data.table::fread(file.path(base_path, "tx-y4-iup-appendix-i.csv"),
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY26 General Not Disadvantaged List",
      disadvantaged = "No"
    ) |>
    dplyr::select(-project_cost)
  
  ## expecting funding projects (Fundable List) ----
  # there are 4 fundable projects that are general not disadvantaged
  tx_invite <- data.table::fread(file.path(base_path, "tx-y4-iup-appendix-k.csv"),
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = clean_numeric_string(eligible_project_cost),
      expecting_funding = "Yes",
      list = "SFY26 General Fundable List",
      project_cost = "No Information"
    ) |>
    dplyr::select(pif_number, funding_amount, expecting_funding, list, project_cost)

 # Lead -----
  tx_lslr <- data.table::fread(file.path(base_path, "tx-y4-lslr-iup-appendix-j.csv"),
                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      project_type = "Lead",
      funding_amount = "No Information",
      disadvantaged = "Yes",
      list = "SFY26 LSL Fundable List"
    ) |>
    dplyr::rename(
      pif_number = pif_no,
      pws_id = pws_id_no,
      population = population_served,
      project_cost = total_project_cost
    )  
  
  # tx_lslr$pif_number %in% tx_ppl$pif_number
  # tx_invite$pif_number %in% tx_ppl$pif_number
  
  # EC ----
  ## Appendix J -----  
  tx_ec_ineligible_dis <- tibble::tribble(
    ~pif , 
    ~list,
    "17924",
    "SFY26 EC Not Disadvantaged List"
  )

  ## comprehensive ----_
  tx_ec_comp <- data.table::fread(file.path(base_path, "tx-y4-ec-iup-appendix-k.csv"),
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      disadvantaged = ifelse(pif_number %in% tx_ec_ineligible_dis$pif, "No", "Yes"),
      list = "SFY26 EC Comprehensive List"
    ) |>
    dplyr::rename(
      project_name = name_of_project
    )
  
  ## fundable ----
  tx_ec_fundable <- data.table::fread(file.path(base_path, "tx-y4-ec-iup-appendix-l.csv"),
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding ="Yes",
      funding_amount = "No Information",
      list = "SFY26 EC Fundable List"
      ) |>
    dplyr::select(pif_number, expecting_funding, list)
    
  tx_ec <- tx_ec_comp |>
    dplyr::left_join(tx_ec_fundable, by = "pif_number") |>
    dplyr::mutate(
      list = dplyr::coalesce(list.y, list.x)
    ) |>
    dplyr::select(-c(list.y, list.x)) |>
    dplyr::mutate(
      list = ifelse(pif_number == "17924", "SFY26 EC Not Disadvantaged List", list),
      project_type = "Emerging Contaminants",
      project_cost = total_project_cost
    )
    
  combined_lists <- tx_ppl |>
    #comprehensive and ineligible
    dplyr::left_join(tx_ineligible_dis, by = "pif_number") |>
    dplyr::mutate(
      entity = dplyr::coalesce(entity.y, entity.x),
      disadvantaged = dplyr::coalesce(disadvantaged.y, disadvantaged.x), 
      list = dplyr::coalesce(list.y, list.x)
    ) |>
    dplyr::select(-c(list.y, list.x, disadvantaged.y, disadvantaged.x, entity.y, entity.x)) |>
    # include fundable
    dplyr::left_join(tx_invite, by = "pif_number") |>
    dplyr::mutate(
      project_cost = dplyr::coalesce(project_cost.y, project_cost.x),
      list = dplyr::coalesce(list.y, list.x)
    ) |>
    dplyr::select(-c(list.y, list.x, project_cost.y, project_cost.x)) |>
    dplyr::bind_rows(tx_lslr) |>
    dplyr::bind_rows(tx_ec)

  # join invited by project id and then process for output
  tx_clean <-  combined_lists |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = str_squish(entity),
      pwsid = str_squish(pws_id),
      pwsid = replace_na(pwsid, "No Information"),
      pwsid = ifelse(pwsid=="none", "No Information",pwsid ),
      project_id = str_squish(pif_number),
      project_id = replace_na(project_id, "No Information"),
      project_name = replace_na(project_name, "No Information"),
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        grepl("lsl|lead", project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"), 
      project_cost = project_cost,
      requested_amount = as.character(NA),
      funding_amount = replace_na(funding_amount, "No Information"),
      principal_forgiveness = as.character(NA),
      project_description = str_squish(project_description),
      population = clean_numeric_string(population),
      disadvantaged = disadvantaged,
      project_rank = str_squish(rank),
      project_rank = clean_numeric_string(project_rank),
      project_score = str_squish(points),
      project_score = clean_numeric_string(project_score),
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Texas",
      state_fiscal_year = "2026"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
#tx_clean  |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

####### Decision : No duplicates

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
  
  tx_clean <- tx_clean |>
    dplyr::mutate(
      project_type = ifelse(
        project_id == "16825",
        "General",
        project_type
      )
    )

 #Decision: 1 unknown --> General
  
  # Check pwsid lengths
  # tx_clean |>
  #   dplyr::mutate(
  #     length_pwsid = stringr::str_length(pwsid)
  #   ) |> 
  #   dplyr::filter(!length_pwsid == 9) |>
  #   dplyr::filter(!pwsid == "No Information")   

  tx_clean <- tx_clean |>
    dplyr::mutate(
      pwsid = dplyr::case_when(
        borrower == "Aqua Water Supply Corporation" & pwsid == "TX011013" ~ "TX0110013",
        .default = pwsid
      )
    )
  
####### SANITY CHECKS END #######
  
  tx_clean <- tx_clean |>
    dplyr::mutate(
      project_type = ifelse(borrower == "Mexia" & project_id == "16581", "General", project_type)
    )
  # Run validation tests
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}
