clean_oh_y1 <- function() {
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table
  
  not_ef_list <- c("53", "164", "341", "249", "256")
  
  ## Base fundable list 
  oh_comp <- data.table::fread("year1/OH/data/oh-ppl-base-id.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(expecting_funding = case_when(
      epic_project_id %in% not_ef_list ~ "No",
      TRUE ~ "Yes"),) |>
    dplyr::select(-c(loan_type, estimated_award_date))

  ## DAC PF
  oh_dac_ppl <- data.table::fread("year1/OH/data/oh-ppl-pf.csv",
                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # these are all disadvantaged
    dplyr::mutate(disadvantaged = "Yes",
           list = "SFY23 DAC PPL") |>
    dplyr::select(epic_project_id, project_score, disadvantaged, rate, 
           estimated_principal_forgiveness, list)
  
  ## Regional PF
  oh_reg_ppl <- data.table::fread("year1/OH/data/oh-ppl-regional-pf.csv",
                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY23 Regionalization PPL") |>
    dplyr::select(epic_project_id, estimated_principal_forgiveness, project_score, rate, list)
  
  
  # merging dac and reg together: 
  oh_dac_reg <- merge(oh_dac_ppl, oh_reg_ppl, all = T) 
  
  # merging this back to base: 
  oh_base_dac_reg <- merge(oh_comp, oh_dac_reg, by = "epic_project_id", 
                           all = T) |>
    dplyr::mutate(rate = paste0(rate.x, rate.y), 
           project_type = case_when(grepl("LSL", rate, ignore.case = T) ~ "Lead", 
                                    grepl("lsl|lead", project, ignore.case=T) ~ "Lead",
                                    grepl("HAB|PFAS", rate, ignore.case = T) ~ "Emerging Contaminants", 
                                    grepl(ec_str, project, ignore.case=TRUE) ~ "Emerging Contaminants", 
                                    # this project gets incorrectly categorized in the string match 
                                    project == "Watermain Imps Bun. 1 - Grange Hall Booster Station Wtr Mns" ~ "General", 
                                    TRUE ~ "General"),
           # convert all PF to numerical or 0, then string for formatting
           principal_forgiveness = clean_numeric_string(convert_to_numeric(estimated_principal_forgiveness, T))) 
  
  
  # adding EC ppl:(9, 7)
  oh_pfas_ppl <- data.table::fread("year1/OH/data/oh-ppl-hab-pfas.csv",
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(project_type = "Emerging Contaminants",
           list = "SFY23 EC PPL") |>
    dplyr::select(epic_project_id, project_type, list)
  

  # list of No Info PF projects
  lead_ni_pf <- c("212", "213", "261", "267", "214", "215", "171", "262", "170")
  
  # adding lead ppl: (59, 8)
  oh_lead_ppl <- data.table::fread("year1/OH/data/oh-lslr.csv",
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |> 
    dplyr::mutate(project_type = "Lead",
           list = "SFY23 Lead PPL",
           # if missing the asterisk at the end of project, we don't know how much PF they'll receive, though they are eligible
           principal_forgiveness = ifelse(epic_project_id %in% lead_ni_pf, "No Information", as.character(NA))
           ) |>
    dplyr::select(epic_project_id, project_type, list, principal_forgiveness)
  
  # there is one project that does not appear on the fundable list - prepping 
  # it for a bind_rows
  oh_lead_extra <- data.table::fread("year1/OH/data/oh-lslr.csv",
                         colClasses = "character", na.strings = "") |>
    janitor::clean_names() |> 
    dplyr::filter(epic_project_id == "441") |>
    dplyr::mutate(project_type.y = "Lead", 
           expecting_funding = "No", 
           pwsid = "No Information", 
           population = "No Information",
           diadvantaged = "No",
           list = "SFY24 Lead PPL") |>
    dplyr::select(-c(estimated_lsl_eligible_costs:rate))
  
  
  # binding the lead and ec together: 
  oh_lead_ec <- bind_rows(oh_lead_ppl, oh_pfas_ppl)
  
  # merging with the final list 
  oh_full <- merge(oh_base_dac_reg, oh_lead_ec, by = "epic_project_id", 
                    all = T)
  oh_full <- bind_rows(oh_full, oh_lead_extra)

  # final cleaning 
  oh_clean <- oh_full |>
    dplyr::mutate(
           # bring over No Info from Lead, otherwise use PF from dis/reg PF tables
           principal_forgiveness = ifelse(!is.na(principal_forgiveness.y), principal_forgiveness.y, principal_forgiveness.x),
           # ensure no projects not funded have listed PF
           principal_forgiveness = ifelse(epic_project_id %in% not_ef_list, "0", principal_forgiveness),
           principal_forgiveness = replace_na(principal_forgiveness, "0"),
           project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    TRUE ~ project_type.x), 
           borrower = str_squish(entity),
           project_id = as.character(NA),
           project_name = as.character(NA),
           requested_amount = clean_numeric_string(estimated_loan_amount),
           funding_amount = as.character(NA),
           project_description = str_squish(project),
           community_served = str_squish(county),  
           project_cost = as.character(NA),
           population = clean_numeric_string(population),
           disadvantaged = case_when(is.na(disadvantaged) ~ "No", 
                                     TRUE ~ disadvantaged), 
           project_rank = as.character(NA), 
           project_score = clean_numeric_string(project_score),
           expecting_funding = case_when(is.na(expecting_funding) ~ "No", 
                                         TRUE ~ expecting_funding), 
           state = "Ohio", 
           state_fiscal_year = "2023",
           list = replace_na("SFY23 Fundable List and Comprehensive List")) |>
    # there was an extra column in there from the bind_rows
    dplyr::filter(!is.na(borrower)) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  oh_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string

  # Check for lead subtypes: Both
  oh_clean |>
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
  oh_clean |>
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
    dplyr::filter(lead_type == "unknown") |> View()

  ####### Decision: No lead projects classified as unknonw
  
  ####### SANITY CHECKS END #######
  
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}

