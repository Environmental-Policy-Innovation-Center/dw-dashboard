source("resources.R")

clean_oh <- function() {
  
  ## Base Project List (oh-ppl-base.csv)
  oh_base <- fread("year2/OH/data/oh-ppl-base.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(
      borrower = entity,
      project_description = project,
      population = sdwis_population,
      community_served = county,
      funding_amount = estimated_loan_amount
    ) %>%
    filter(borrower != "Entity") %>%
    mutate(in_base_ppl = TRUE)
  print(paste("Shape of oh_base:", nrow(oh_base), "rows,", ncol(oh_base), "columns"))
  
  ## Principal Forgiveness (PF) List (oh-ppl-pf.csv)
  oh_pf <- fread("year2/OH/data/oh-ppl-pf.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(
      borrower = entity,
      project_description = project,
      community_served = county,
      funding_amount = estimated_loan_amount,
      principal_forgiveness = estimated_principal_forgiveness
    ) %>%
    mutate(in_dac_ppl = TRUE)
  print(paste("Shape of oh_pf:", nrow(oh_pf), "rows,", ncol(oh_pf), "columns"))
  
  ## Regional Principal Forgiveness List (oh-ppl-regional-pf.csv)
  oh_reg_pf <- fread("year2/OH/data/oh-ppl-regional-pf.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(
      borrower = entity,
      project_description = project,
      community_served = county,
      funding_amount = estimated_loan_amount,
      principal_forgiveness = estimated_principal_forgiveness
    ) %>%
    mutate(in_regional_ppl = TRUE)
  print(paste("Shape of oh_reg_pf:", nrow(oh_reg_pf), "rows,", ncol(oh_reg_pf), "columns"))
  
  ## Emerging Contaminants (EC) List (oh-ppl-ecr.csv)
  oh_ecr <- fread("year2/OH/data/oh-ppl-ecr.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(
      borrower = entity,
      project_description = project,
      community_served = county,
      funding_amount = estimated_loan_amount,
      ec_funding = estimated_ec_amount,
      ec_principal_forgiveness = est_ec_principal_forgiveness,
      project_score = score_total_points
    ) %>%
    mutate(in_ec_ppl = TRUE)
  print(paste("Shape of oh_ecr:", nrow(oh_ecr), "rows,", ncol(oh_ecr), "columns"))
  
  ## Lead Service Line Replacement (LSLR) List (oh-lslr.csv)
  oh_lslr <- fread("year2/OH/data/oh-lslr.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(
      borrower = entity,
      project_description = project,
      community_served = county,
      funding_amount = estimated_loan_amount,
      lsl_funding = estimated_lsl_portion_of_the_project
    ) %>%
    mutate(in_lsl_ppl = TRUE)
  print(paste("Shape of oh_lslr:", nrow(oh_lslr), "rows,", ncol(oh_lslr), "columns"))
  
  # Merge all datasets
  oh_merged <- oh_base %>%
    full_join(oh_pf, by = c("borrower", "project_description", "pws_id", "community_served", "funding_amount"), suffix = c("", "_pf")) %>%
    full_join(oh_reg_pf, by = c("borrower", "project_description", "pws_id", "community_served", "funding_amount"), suffix = c("", "_reg_pf")) %>%
    full_join(oh_ecr, by = c("borrower", "project_description", "pws_id", "community_served", "funding_amount"), suffix = c("", "_ecr")) %>%
    full_join(oh_lslr, by = c("borrower", "project_description", "pws_id", "community_served", "funding_amount"), suffix = c("", "_lslr"))
  
  print(paste("Shape of oh_merged:", nrow(oh_merged), "rows,", ncol(oh_merged), "columns"))
  
  # Clean and format the final dataset
  oh_clean <- oh_merged %>%
    mutate(
      population = clean_numeric_string(coalesce(population, population_pf)),
      state = "Ohio",
      state_fiscal_year = "2024",
      
      # Project Type
      project_type = case_when(
        in_ec_ppl == TRUE ~ "Emerging Contaminants",
        in_lsl_ppl == TRUE ~ "Lead",
        TRUE ~ "General"  # All other projects are considered General
      ),
      
      # Funding Amount
      funding_amount = case_when(
        project_type == "Emerging Contaminants" ~ clean_numeric_string(ec_funding),
        project_type == "Lead" ~ clean_numeric_string(lsl_funding),
        TRUE ~ clean_numeric_string(funding_amount)
      ),
      
      # Principal Forgiveness
      principal_forgiveness = case_when(
        project_type == "Emerging Contaminants" ~ clean_numeric_string(ec_principal_forgiveness),
        project_type == "Lead" ~ "0",  # Set to "0" instead of NA
        in_dac_ppl == TRUE | in_regional_ppl == TRUE ~ 
          clean_numeric_string(coalesce(principal_forgiveness, principal_forgiveness_reg_pf)),
        TRUE ~ "0"
      ),
      
      # Disadvantaged
      disadvantaged = case_when(
        project_type == "General" & in_dac_ppl == TRUE & !grepl("EC List", principal_forgiveness) ~ "Yes",
        project_type == "Lead" & grepl("PF", rate) ~ "Yes",
        project_type == "Emerging Contaminants" ~ NA_character_,
        TRUE ~ "No"
      ),
      
      # Project Score
      project_score = if_else(project_type == "Emerging Contaminants",
                              clean_numeric_string(project_score),
                              NA_character_),
      
      # Expecting Funding
      expecting_funding = case_when(
        project_type == "General" & (grepl("Bypass", principal_forgiveness) | grepl("Bypass", principal_forgiveness_reg_pf)) ~ "No",
        project_type == "Emerging Contaminants" & grepl("\\*", ec_funding) ~ "No",
        TRUE ~ "Yes"
      ),
      
      # EC Contaminant
      ec_contaminant = if_else(project_type == "Emerging Contaminants", ec_contaminant, NA_character_),
      
      # Create placeholder columns for missing required fields
      project_id = as.character(NA),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = as.character(NA),
      project_rank = as.character(NA)
    )
  
  # Get the names of columns that exist in oh_clean
  existing_columns <- names(oh_clean)
  
  # Create a list of desired columns, using NA as a placeholder for missing columns
  desired_columns <- list(
    community_served = "community_served",
    borrower = "borrower",
    pwsid = "pws_id",
    project_id = "project_id",
    project_name = "project_name",
    project_type = "project_type",
    project_cost = "project_cost",
    requested_amount = "requested_amount",
    funding_amount = "funding_amount",
    principal_forgiveness = "principal_forgiveness",
    lsl_funding = if ("lsl_funding" %in% existing_columns) "lsl_funding" else NA,
    ec_funding = if ("ec_funding" %in% existing_columns) "ec_funding" else NA,
    ec_principal_forgiveness = if ("ec_principal_forgiveness" %in% existing_columns) "ec_principal_forgiveness" else NA,
    population = "population",
    project_description = "project_description",
    disadvantaged = "disadvantaged",
    project_rank = "project_rank",
    project_score = "project_score",
    expecting_funding = "expecting_funding",
    state = "state",
    state_fiscal_year = "state_fiscal_year",
    ec_contaminant = "ec_contaminant"
  )
  
  # Select columns, replacing missing columns with NA
  oh_clean <- oh_clean %>%
    select(!!!lapply(desired_columns, function(col) {
      if (is.na(col)) {
        NA_character_
      } else {
        sym(col)
      }
    }))
  
  # Convert NA columns to character vectors of NAs
  oh_clean <- oh_clean %>%
    mutate(across(where(is.logical), ~as.character(NA)))
  
  print(paste("Shape of oh_clean:", nrow(oh_clean), "rows,", ncol(oh_clean), "columns"))
  
  # Diagnostic information
  print("Project Type Distribution:")
  print(table(oh_clean$project_type, useNA = "ifany"))
  
  print("EC Contaminant NA Distribution:")
  print(table(is.na(oh_clean$ec_contaminant)))
  
  print("EC Contaminant values for Emerging Contaminants projects:")
  print(table(oh_clean$ec_contaminant[oh_clean$project_type == "Emerging Contaminants"], useNA = "always"))
  
  print("Disadvantaged Status Distribution:")
  print(table(oh_clean$disadvantaged, oh_clean$project_type, useNA = "ifany"))
  
  print("Number of unique borrowers:")
  print(length(unique(oh_clean$borrower)))
  
  print("Number of projects by borrower (top 10):")
  print(head(sort(table(oh_clean$borrower), decreasing = TRUE), 10))
  
  # Run validation tests on the cleaned data
  run_tests(oh_clean)
  
  # Clear unnecessary variables
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}