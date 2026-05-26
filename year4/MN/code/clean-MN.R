clean_mn_y4 <- function() {
  
  mn_base_ec_fundable <- data.table::fread("year4/MN/data/mn-general-ec-fundable-list.csv",
                                           colClasses = "character", na.strings = "", header=TRUE) |> 
    janitor::clean_names() |>
    dplyr::mutate(expecting_funding = "Yes",
           list = "SFY26 General & EC Fundable List")
  
  mn_not_fundable <- data.table::fread("year4/MN/data/mn-not-fundable-list.csv",
                                       colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    # drop projects expecting funding in Gen/EC list
    filter(!(project_id %in% mn_base_ec_fundable$project_id)) |>
    dplyr::mutate(
      funding_amount = "No Information",
      list = "SFY26 Not Fundable List"
    )
  
  # read in version of the lead ppl manually edited to remove nested values.
  # see notes in data dictionary for how columns were combined.
  # raw version of the data also available for reference.
  mn_lead <- data.table::fread("year4/MN/data/mn-lead-table-decoupled.csv",
                               colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::mutate(project_type = "Lead",
           list = "SFY26 LSL Fundable List",
           expecting_funding = "Yes",
           # store project_ids (since there can be multiple in this table)
           project_id_lead = project_id,
           # then make the join variable only the first project
           project_id = substr(project_id, 1, 10)
           ) |>
    dplyr::rename(total_project_cost_lead = total_project_costs,
           iup_approved_amount_lead = iup_approved_amount,
           ppl_rank_lead = ppl_rank,
           ppl_points_lead = ppl_points,
           system_population_served_lead = system_population_served,
           project_description_lead = project_description
           )
  
  mn_combined <- bind_rows(mn_base_ec_fundable, mn_not_fundable, mn_lead) |>
    dplyr::mutate(project_type = 
             case_when(
               !is.na(project_type) ~ "Lead",
               grepl("lead|lsl", project_description, ignore.case=T) ~ "Lead",
               convert_to_numeric(ec_amount, TRUE) > 0 ~ "Emerging Contaminants",
               grepl("EC", iup_part) ~ "Emerging Contaminants"
             ),
           total_project_cost = clean_numeric_string((total_project_costs))
           ) |>
    dplyr::select(project_id, funding_amount, list, project_type, total_project_cost, expecting_funding,
           # keep all lead projects to merge back in after joining with comp ppl, since
           # project_id will only match to one of the merged rows and may not have composite data in comp table
           project_id_lead, total_project_cost_lead, iup_approved_amount_lead, ppl_rank_lead,
           ppl_points_lead, system_population_served_lead, project_description_lead)
  
  
  mn_comp_ppl <-  data.table::fread("year4/MN/data/mn-comprehensive-ppl.csv",
                                    colClasses = "character", na.strings = "") |> 
    janitor::clean_names()

  
  mn_clean <- mn_comp_ppl |>
    dplyr::left_join(mn_combined, by="project_id") |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = stringr::str_squish(word(rank_system, 2, -1)),
      pwsid = paste0("MN", stringr::str_remove(project_id, "-.*")),
      project_id = ifelse(!is.na(project_id_lead),
                          project_id_lead, stringr::str_squish(project_id)),
      project_name = as.character(NA),
      project_description = ifelse(!is.na(project_description_lead),
                                   project_description_lead, stringr::str_squish(project)),
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        grepl("lead|lsl", project_description, ignore.case = TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      # defer to total_project_cost from other lists, otherwise use project cost on comp ppl
      project_cost = ifelse(!is.na(total_project_cost), total_project_cost, clean_numeric_string(project_cost)),
      project_cost = ifelse(!is.na(total_project_cost_lead),
                            clean_numeric_string(total_project_cost_lead), project_cost),
      requested_amount = as.character(NA),
      funding_amount = clean_numeric_string(iup_approved_amount_lead),
      principal_forgiveness = as.character(NA),
      population = ifelse(!is.na(system_population_served_lead),
                          clean_numeric_string(system_population_served_lead), clean_numeric_string(population)),
      disadvantaged = as.character(NA),
      project_rank = ifelse(!is.na(ppl_rank_lead), str_squish(ppl_rank_lead), str_squish(word(rank_system, 1))),
      project_score = ifelse(!is.na(ppl_points_lead), str_squish(ppl_points_lead), str_squish(points)),
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Minnesota",
      state_fiscal_year = "2026",
      list = replace_na(list, "SFY26 Comprehensive List")
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  run_tests(mn_clean)
  
  rm(list=setdiff(ls(), "mn_clean"))
  
  return(mn_clean)
}

####### SANITY CHECKS START #######

# Hone in on project id duplication
# mn_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision: No duplicates

# Check for disinfection byproduct in description
# mn_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
####### Decision: No change, classified as expected

# Check for lead subtypes
# mn_clean |>
#   dplyr::filter(project_type=="Lead") |>
# dplyr::mutate(
#   lead_type = dplyr::case_when(
#   stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
#   stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
#   stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
#  # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
#  stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
#  stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
#   TRUE ~ "unknown"
# )) |>
#   dplyr::filter(lead_type == "both")
# ####### Decision: No lead projects classified as both

####### SANITY CHECKS END #######