clean_oh_y3 <- function() {
  
  base_path <- "year3/OH/data"
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table

  # Comprehensive Project List
  oh_fundable <- fread(file.path(base_path, "oh-ppl-base.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(borrower = str_squish(entity),
           project_description = str_squish(project),
           requested_amount = clean_numeric_string(estimated_loan_amount),
           funding_amount = clean_numeric_string(estimated_loan_amount),
           population = clean_numeric_string(sdwis_population),
           pwsid = str_squish(pws_id),
           community_served = str_squish(county), 
           # these are all expecting funding 
           expecting_funding = "Yes") %>%
    select(epic_project_id, borrower, project_description, requested_amount,
           funding_amount, population, pwsid, community_served, expecting_funding, 
           rate)

  # DAC Principal Forgiveness List
  oh_dac <- fread(file.path(base_path, "oh-ppl-pf.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # removing numeric strings from estimated pf for the clean_numeric_string
    # function later
    mutate(dac_pf = ifelse(estimated_principal_forgiveness %in% 
                             c("Bypass 1", "Bypass 2"), 
                           "Bypass", estimated_principal_forgiveness), 
           # these are all disadvantaged 
           disadvantaged = "Yes") %>% 
    select(epic_project_id, disadvantaged, project_score, rate, 
           dac_pf)
  
  # Regionalization Principal Forgiveness List
  oh_reg <- fread(file.path(base_path, "oh-ppl-regional-pf.csv"),
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # NOTE - there was one project "Phillipsburg - Drinking Water PFAS Remediation"
    # that didn't have a project ID but ends up on the ec_extra list, so it's safe
    # to filter here
    filter(!is.na(epic_project_id)) %>%
    mutate(reg_pf = ifelse(estimated_principal_forgiveness %in% 
                             c("Bypass 1", "Bypass 2"), "
                           Bypass", estimated_principal_forgiveness)) %>%
    select(epic_project_id, reg_pf, project_score, rate)
  
  # discount list - NOTE - this table is not really mentioned in the DD (except
  # for project_type) and the regionalization list already has that information, 
  # so opting to skip adding this table. 
  # oh_dis <- fread(file.path(base_path, "oh-discount.csv"),
  #                 colClasses = "character", na.strings = "") %>%
  #   clean_names()
  
  # merging DAC & regionalization PPLs to handle pf cols: 
  oh_dac_reg <- merge(oh_dac, oh_reg, by = "epic_project_id", all = T) %>%
    # pasting rates together for string matching 
    mutate(rate = paste0(rate.x, rate.y), 
           # can confirm project scores are the same where the lists overlap
           project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                     TRUE ~ project_score.x), 
           # handling pf overlaps 
           principal_forgiveness = case_when(!is.na(reg_pf) & !(reg_pf %in% c("Bypass", "EC List")) ~ reg_pf, 
                                             !is.na(dac_pf) & !(dac_pf %in% c("Bypass", "EC List", "REG List")) ~ dac_pf)) %>%
    # trimming extra cols: 
    select(-c(rate.x, dac_pf, reg_pf, project_score.y, project_score.x, rate.y))
  
  
  # merging these together for project types:
  oh_main <- merge(oh_fundable, oh_dac_reg, by = "epic_project_id", all = T) %>%
    mutate(rate = paste0(rate.x, rate.y),
           project_type = case_when(grepl("LSL", rate, ignore.case = T) ~ "Lead", 
                                    grepl("HAB|EC|PFAS", rate, ignore.case = T) ~ "Emerging Contaminants", 
                                    grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
                                    TRUE ~ "General")) %>%
    # trim extra columns
    select(-c(rate.x, rate.y, rate))

  # Emerging Contaminants List
  oh_ec <- fread(file.path(base_path, "oh-ppl-ecr.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # these are all not on the fundable list but will be added later
    filter(grepl("$", estimated_ec_amount)) %>%
    mutate(project_type = "Emerging Contaminants",
           ec_pf = ifelse(est_ec_principal_forgiveness %in% 
                            c("Bypass 1", "Bypass 2"), "Bypass", est_ec_principal_forgiveness),
           project_score = str_squish(score_total_points)) %>%
    select(epic_project_id, ec_pf, project_score, project_type)

  # Get extra EC projects that don't match fundable list & prepping for 
  # a bind_rows later: 
  oh_ec_extra <- fread(file.path(base_path, "oh-ppl-ecr.csv"),
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(!grepl("$", estimated_ec_amount)) %>%
    mutate(borrower = str_squish(entity),
           project_description = str_squish(project),
           pwsid = str_squish(pws_id),
           community_served = str_to_sentence(county),
           project_score = str_squish(score_total_points),
           project_type = "Emerging Contaminants",
           requested_amount = clean_numeric_string(estimated_loan_amount),
           disadvantaged = "No",
           expecting_funding = "No",
           funding_amount = "No Information",
           # principal_forgiveness = clean_numeric_string(est_ec_principal_forgiveness),
           principal_forgiveness = "No Information",
           population = "No Information") %>%
    select(borrower, project_description, pwsid, community_served,
           project_score, project_type, disadvantaged,requested_amount,
           expecting_funding, funding_amount, principal_forgiveness, population)
  
  # merging ec list and our main list to handle pf cols: 
  oh_main_ec <- merge(oh_main, oh_ec, by = "epic_project_id", all = T) %>%
    # fixing project types
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                   TRUE ~ project_type.x), 
           # can confirm pf does not overlap
           principal_forgiveness = case_when(!is.na(ec_pf) ~ ec_pf, 
                                             TRUE ~ principal_forgiveness), 
           # note the is a project w/ the desscription "PFAS Well Mitigation" 
           # that has a different project score from the DAC PPL. Confirmed 
           # that it should receive the project score on the EC list
           project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                     TRUE ~ project_score.x)) %>%
    select(-c(project_type.x, project_type.y, project_score.x, project_score.y, 
              ec_pf))

  # Lead Service Line List  
  oh_lead <- fread(file.path(base_path, "oh-lslr.csv"),
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # these are all lead projects
    mutate(project_type = "Lead") %>%
    select(epic_project_id, project_type)

  # Combine fundable, dc, regionalization, and discount ppls: 
  oh_comp <- merge(oh_main_ec, oh_lead, by = "epic_project_id", all = T) %>%
    # default to project type for lead or ec lists: 
    mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y,
                                    TRUE ~ project_type.x))  %>%
    # remove extra columns
    select(-c("project_type.x", "project_type.y"))
  
  # Add extra EC projects and final cleanup
  oh_clean <- bind_rows(oh_comp, oh_ec_extra) %>%
    # fixing the expecting funding columns based on the presence of "Bypass" 
    # or "*" in the PF columns 
    # link to thread where we decided this: https://enviropolicyinno.slack.com/archives/C08LXGF02AE/p1762974431302769?thread_ts=1759336213.263239&cid=C08LXGF02AE
    mutate(expecting_funding = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "No", 
                                         TRUE ~ expecting_funding), 
           funding_amount = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "No Information", 
                                      TRUE ~ funding_amount),
           principal_forgiveness = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "No Information", 
                                             TRUE ~ principal_forgiveness)) %>%
    mutate(state = "Ohio",
           state_fiscal_year = "2025",
           # cleaning numeric strings: 
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           project_score = clean_numeric_string(project_score),
           # fill in extra cols: 
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           disadvantaged = ifelse(is.na(disadvantaged), "No", disadvantaged),
           project_rank = as.character(NA)) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
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
    dplyr::filter(lead_type == "unknown")

    
  ####### SANITY CHECKS END #######

  # Run validation tests
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}