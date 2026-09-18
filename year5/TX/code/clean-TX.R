clean_tx_y5 <- function() {
  base_path <- file.path("year5", "TX", "data")
  
  # General -----
  ## this includes all projects (Comprehensive List) ----
  tx_ppl <- data.table::fread(file.path(base_path, "SFY27_General_Comprehensive_appendix_j.csv"),
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY27 General Comprehensive List",
      project_cost = clean_numeric_string(total_project_cost),
      disadvantaged = ifelse(!is.na(disadv_percent), "Yes", "No")
    )
  
  ## appendix I ----
  tx_ineligible_dis <- data.table::fread(file.path(base_path, "SFY27_General_Not_Disadvantaged_appendix_i.csv"),
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY27 General Not Disadvantaged List",
      # project_cost = clean_numeric_string(project_cost),
      disadvantaged = "No"
    ) |>
    dplyr::select(-project_cost)
  
  ## expecting funding projects (Fundable List) ----
  # there are 4 fundable projects that are general not disadvantaged
  tx_invite <- data.table::fread(file.path(base_path, "SFY27_General_Fundable_appendix_k.csv"),
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY27 General Fundable List",
      project_cost = clean_numeric_string(project_cost),
      expecting_funding = "Yes"
    ) |>
    dplyr::select(pif_number, expecting_funding, list, project_cost)
    
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
    dplyr::select(-c(list.y, list.x, project_cost.y, project_cost.x)) 


  # sum(tx_ineligible_dis$pif_number %in% tx_invite$pif_number)

  # join invited by project id and then process for output
  tx_clean <-  combined_lists |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = str_squish(entity),
      pwsid = str_squish(pws_id),
      pwsid = replace_na(pwsid, "No Information"),
      project_id = str_squish(pif_number),
      project_id = replace_na(project_id, "No Information"),
      project_name = "No Information",
      project_type = case_when(
        grepl("lsl|lead", project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"), 
      project_cost = project_cost,
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      project_description = str_squish(project_description),
      population = ifelse(is.na(population), "No Information", clean_numeric_string(population)),
      disadvantaged = disadvantaged,
      project_rank = str_squish(rank),
      project_rank = clean_numeric_string(project_rank),
      project_score = str_squish(points),
      project_score = clean_numeric_string(project_score),
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Texas",
      state_fiscal_year = "2027"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
#tx_clean  |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

####### Decision : No duplicates

# Check for disinfection byproduct in description
#tx_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision : 1 disinfection byproduct string; this project will be reclassified as General
  
tx_clean <- tx_clean  |>
  dplyr::mutate(project_type = ifelse(project_id == "17468" & project_type == "Emerging Contaminants", "General", project_type))
  
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


 #Decision: 1 unknown --> kept as lead unknown -- as we do not know how they are addressing L&C rule (2026_09_16 Danielle)
  
#tx_clean |> dplyr::filter(project_id %in% tx_ineligible_dis$pif_number) |> dplyr::select(disadvantaged) |> distinct()

  

####### SANITY CHECKS END #######
  
  # Run validation tests
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}
