#source("resources.R")

clean_oh <- function() {
  #directory path
  data_dir <- "../data/"
  
  standardize_columns <- function(df) {
    df %>%
      clean_names() %>%
      rename(
        borrower = entity,
        project_description = project,
        community_served = county,
        pwsid = pws_id
      )
  }
  
  # Base Project List
  oh_base <- fread(paste0(data_dir, "oh-ppl-base.csv"),
                   colClasses = "character", na.strings = "") %>%
    standardize_columns() %>%
    rename(
      funding_amount = estimated_loan_amount,
      population = sdwis_population
    ) %>%
    filter(borrower != "Entity") %>%
    mutate(
      project_type = "General",
      source_list = "base"
    )
  
  print(paste("Shape of oh_base:", nrow(oh_base), "rows,", ncol(oh_base), "columns"))
  
  # DAC Principal Forgiveness List
  oh_pf <- fread(paste0(data_dir, "oh-ppl-pf.csv"),
                 colClasses = "character", na.strings = "") %>%
    standardize_columns() %>%
    rename(
      funding_amount = estimated_loan_amount,
      principal_forgiveness = estimated_principal_forgiveness
    ) %>%
    filter(!grepl("EC List", principal_forgiveness)) %>%
    mutate(source_list = "dac_pf")
  
  print(paste("Shape of oh_pf:", nrow(oh_pf), "rows,", ncol(oh_pf), "columns"))
  
  # Regional Principal Forgiveness List
  oh_reg_pf <- fread(paste0(data_dir, "oh-ppl-regional-pf.csv"),
                     colClasses = "character", na.strings = "") %>%
    standardize_columns() %>%
    rename(
      funding_amount = estimated_loan_amount,
      principal_forgiveness = estimated_principal_forgiveness
    ) %>%
    mutate(source_list = "reg_pf")
  
  print(paste("Shape of oh_reg_pf:", nrow(oh_reg_pf), "rows,", ncol(oh_reg_pf), "columns"))
  
  # Emerging Contaminants List
  oh_ec <- fread(paste0(data_dir, "oh-ppl-ecr.csv"),
                 colClasses = "character", na.strings = "") %>%
    standardize_columns() %>%
    rename(
      funding_amount = estimated_ec_amount,
      principal_forgiveness = est_ec_principal_forgiveness,
      project_score = score_total_points
    ) %>%
    mutate(
      project_type = "Emerging Contaminants",
      source_list = "ec"
    )
  
  print(paste("Shape of oh_ec:", nrow(oh_ec), "rows,", ncol(oh_ec), "columns"))
  
  # Lead Service Line List
  oh_lsl <- fread(paste0(data_dir, "oh-lslr.csv"),
                  colClasses = "character", na.strings = "") %>%
    standardize_columns() %>%
    rename(
      funding_amount = estimated_lsl_portion_of_the_project,
      lsl_rate = rate
    ) %>%
    mutate(
      project_type = "Lead",
      source_list = "lsl"
    )
  
  print(paste("Shape of oh_lsl:", nrow(oh_lsl), "rows,", ncol(oh_lsl), "columns"))
  
  # First ---- base table with project types
  project_types <- bind_rows(oh_base, oh_pf, oh_reg_pf, oh_ec, oh_lsl) %>%
    group_by(borrower, project_description, pwsid) %>%
    summarise(
      project_type = case_when(
        any(source_list == "ec") ~ "Emerging Contaminants",
        any(source_list == "lsl") ~ "Lead",
        TRUE ~ "General"
      ),
      community_served = first(community_served),
      is_ec = any(source_list == "ec"),
      is_lsl = any(source_list == "lsl"),
      is_dac = any(source_list == "dac_pf"),
      is_reg = any(source_list == "reg_pf"),
      .groups = "drop"
    )
  
  # Handle each source separately
  base_data <- oh_base %>%
    select(borrower, project_description, pwsid, population, funding_amount) %>%
    distinct()
  
  ec_data <- oh_ec %>%
    select(borrower, project_description, pwsid, funding_amount, 
           principal_forgiveness, project_score, ec_contaminant) %>%
    distinct()
  
  lsl_data <- oh_lsl %>%
    select(borrower, project_description, pwsid, funding_amount, lsl_rate) %>%
    distinct()
  
  pf_data <- oh_pf %>%
    select(borrower, project_description, pwsid, principal_forgiveness) %>%
    distinct()
  
  reg_pf_data <- oh_reg_pf %>%
    select(borrower, project_description, pwsid, principal_forgiveness) %>%
    distinct()
  
  # Combine everything
  oh_clean <- project_types %>%
    left_join(base_data, by = c("borrower", "project_description", "pwsid")) %>%
    left_join(ec_data, by = c("borrower", "project_description", "pwsid"), suffix = c("", "_ec")) %>%
    left_join(lsl_data, by = c("borrower", "project_description", "pwsid"), suffix = c("", "_lsl")) %>%
    left_join(pf_data, by = c("borrower", "project_description", "pwsid"), suffix = c("", "_pf")) %>%
    left_join(reg_pf_data, by = c("borrower", "project_description", "pwsid"), suffix = c("", "_reg")) %>%
    mutate(
      # Clean funding amount
      funding_amount = case_when(
        is_ec ~ clean_numeric_string(funding_amount_ec),
        is_lsl ~ clean_numeric_string(funding_amount_lsl),
        TRUE ~ clean_numeric_string(funding_amount)
      ),
      
      # Clean principal forgiveness
      principal_forgiveness = case_when(
        is_ec ~ clean_numeric_string(principal_forgiveness),
        is_lsl ~ NA_character_,
        is_dac ~ clean_numeric_string(principal_forgiveness_pf),
        is_reg ~ clean_numeric_string(principal_forgiveness_reg),
        TRUE ~ "0"
      ),
      
      # Clean population
      population = clean_numeric_string(population),
      
      # Handle disadvantaged status
      disadvantaged = case_when(
        project_type == "Emerging Contaminants" ~ NA_character_,
        project_type == "Lead" & !is.na(lsl_rate) & grepl("PF", lsl_rate) ~ "Yes",
        project_type == "Lead" ~ "No",
        is_dac ~ "Yes",
        TRUE ~ "No"
      ),
      
      # Handle expecting funding
      expecting_funding = case_when(
        project_type == "Lead" ~ "Yes",
        project_type == "Emerging Contaminants" & grepl("\\*", funding_amount_ec) ~ "No",
        grepl("Bypass", coalesce(principal_forgiveness_pf, principal_forgiveness_reg)) ~ "No",
        TRUE ~ "Yes"
      ),
      
      # Project score only for EC
      project_score = if_else(is_ec, clean_numeric_string(project_score), NA_character_),
      
      state = "Ohio",
      state_fiscal_year = "2024",
      project_id = NA_character_,
      project_name = NA_character_,
      project_cost = "No Information",
      requested_amount = "No Information",
      project_rank = NA_character_
    ) %>%
    # Select final columns
    select(
      community_served,
      borrower,
      pwsid,
      project_id,
      project_name,
      project_type,
      project_cost,
      requested_amount,
      funding_amount,
      principal_forgiveness,
      population,
      project_description,
      disadvantaged,
      project_rank,
      project_score,
      expecting_funding,
      state,
      state_fiscal_year,
      ec_contaminant
    )
  
  #  diagnostics
  print(paste("Final shape of oh_clean:", nrow(oh_clean), "rows,", ncol(oh_clean), "columns"))
  
  print("Project Type Distribution:")
  print(table(oh_clean$project_type, useNA = "ifany"))
  
  print("Disadvantaged Status Distribution:")
  print(table(oh_clean$disadvantaged, oh_clean$project_type, useNA = "ifany"))
  
  print("Expecting Funding Distribution:")
  print(table(oh_clean$expecting_funding, oh_clean$project_type, useNA = "ifany"))
  
  print("Principal Forgiveness NA Distribution:")
  print(table(is.na(oh_clean$principal_forgiveness), oh_clean$project_type))
  
  # Run validation tests
  run_tests(oh_clean)
  
  return(oh_clean)
}