clean_oh_y3 <- function() {
  base_path <- "year3/OH/data"

  # Comprehensive Project List
  oh_comp <- fread(file.path(base_path, "oh-ppl-base.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(borrower = str_squish(entity),
           project_description = str_squish(project),
           funding_amount = clean_numeric_string(estimated_loan_amount),
           population = clean_numeric_string(sdwis_population),
           pwsid = str_squish(pws_id),
           community_served = str_squish(county)) %>%
    select(epic_project_id, borrower, project_description, funding_amount, population, pwsid, community_served)

  # DAC Principal Forgiveness List
  oh_pf <- fread(file.path(base_path, "oh-ppl-pf.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(dac_pf = estimated_principal_forgiveness) %>%
    mutate(dac_pf = ifelse(dac_pf == "BYPASS 1", "BYPASS", dac_pf)) %>%
    select(epic_project_id, dac_pf)

  # Regional Principal Forgiveness List
  oh_reg_pf <- fread(file.path(base_path, "oh-ppl-regional-pf.csv"),
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(reg_pf = estimated_principal_forgiveness) %>%
    mutate(reg_pf = ifelse(reg_pf == "BYPASS 1", "BYPASS", reg_pf)) %>%
    select(epic_project_id, reg_pf)

  # Emerging Contaminants List
  oh_ec <- fread(file.path(base_path, "oh-ppl-ecr.csv"),
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           disadvantaged = "No Information",
           project_score = str_squish(score_total_points)) %>%
    select(epic_project_id, project_type, disadvantaged, estimated_ec_amount, est_ec_principal_forgiveness, project_score)

  # Get extra EC projects that don't match comprehensive list
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
           disadvantaged = "No Information",
           expecting_funding = "No",
           funding_amount = "No Information",
           principal_forgiveness = "No Information",
           population = "No Information") %>%
    select(borrower, project_description, pwsid, community_served, project_score, project_type, disadvantaged,
           expecting_funding, funding_amount, principal_forgiveness, population)

  # Lead Service Line List  
  oh_lsl <- fread(file.path(base_path, "oh-lslr.csv"),
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead",
           disadvantaged = case_when(
             grepl("PF", rate) ~ "Yes",
             TRUE ~ "No")) %>%
    select(epic_project_id, project_type, disadvantaged, estimated_lsl_portion_of_the_project)

  # Combine Lead and EC
  oh_ec_lsl <- bind_rows(oh_ec, oh_lsl)

  # Join all datasets and process
  oh_comp <- oh_comp %>%
    left_join(oh_pf, by="epic_project_id") %>%
    left_join(oh_reg_pf, by="epic_project_id") %>%
    left_join(oh_ec_lsl, by="epic_project_id") %>%
    mutate(
      project_type = case_when(
        is.na(project_type) ~ "General",
        TRUE ~ project_type),
      disadvantaged = case_when(
        !is.na(disadvantaged) ~ disadvantaged,
        !is.na(dac_pf) ~ "Yes",
        TRUE ~ "No"),
      expecting_funding = case_when(
        project_type == "Emerging Contaminants" ~ "Yes",
        project_type == "Lead" ~ "Yes",
        project_type == "General" & (grepl("BYPASS", dac_pf) | grepl("BYPASS", reg_pf)) ~ "No",
        TRUE ~ "Yes"),
      funding_amount = case_when(
        !is.na(estimated_ec_amount) ~ clean_numeric_string(estimated_ec_amount),
        !is.na(estimated_lsl_portion_of_the_project) ~ clean_numeric_string(estimated_lsl_portion_of_the_project),
        TRUE ~ funding_amount),
      dac_pf = clean_numeric_string(dac_pf),
      reg_pf = clean_numeric_string(reg_pf),
      principal_forgiveness = case_when(
        dac_pf != "No Information" ~ dac_pf,
        reg_pf != "No Information" ~ reg_pf,
        !is.na(est_ec_principal_forgiveness) ~ clean_numeric_string(est_ec_principal_forgiveness),
        project_type == "Lead" ~ "No Information",
        TRUE ~ "0"),
      principal_forgiveness = ifelse(project_type == "General" & principal_forgiveness == "No Information", "0", principal_forgiveness)
    )

  # Add extra EC projects and final cleanup
  oh_clean <- bind_rows(oh_comp, oh_ec_extra) %>%
    mutate(
      state = "Ohio",
      state_fiscal_year = "2025",
      project_id = as.character(NA),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = as.character(NA),
      project_rank = as.character(NA),
      project_score = replace_na(project_score, "No Information")
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  # Run validation tests
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}