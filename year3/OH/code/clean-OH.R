source("resources.R")

clean_oh <- function() {
  
  ## Base Project List (oh-ppl-base.csv)
  oh_base <- fread("../data/oh-ppl-base.csv",
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
  oh_pf <- fread("../data/oh-ppl-pf.csv",
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
  oh_reg_pf <- fread("../data/oh-ppl-regional-pf.csv",
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
  oh_ecr <- fread("../data/oh-ppl-ecr.csv",
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
  oh_lslr <- fread("../data/oh-lslr.csv",
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
      state_fiscal_year = "2025",
      
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
      #ec_contaminant = if_else(project_type == "Emerging Contaminants", ec_contaminant, NA_character_),
      
      # Create placeholder columns for missing required fields
      project_id = as.character(NA),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = as.character(NA),
      project_rank = as.character(NA)
    )
  
  # Get the names of columns that exist in oh_clean
  existing_columns <- names(oh_clean)
  
  existing_columns <- names(oh_clean)
  print("Existing columns in oh_clean:")
  print(existing_columns)
  
  
  
  # Create a list of desired columns, using NA as a placeholder for missing columns
  desired_columns <- list(
    community_served = if ("community_served" %in% existing_columns) "community_served" else NA_character_,
    borrower = if ("borrower" %in% existing_columns) "borrower" else NA_character_,
    pwsid = if ("pws_id" %in% existing_columns) "pws_id" else NA_character_,
    project_id = if ("project_id" %in% existing_columns) "project_id" else NA_character_,
    project_name = if ("project_name" %in% existing_columns) "project_name" else NA_character_,
    project_type = if ("project_type" %in% existing_columns) "project_type" else NA_character_,
    project_cost = if ("project_cost" %in% existing_columns) "project_cost" else NA_character_,
    requested_amount = if ("requested_amount" %in% existing_columns) "requested_amount" else NA_character_,
    funding_amount = if ("funding_amount" %in% existing_columns) "funding_amount" else NA_character_,
    principal_forgiveness = if ("principal_forgiveness" %in% existing_columns) "principal_forgiveness" else NA_character_,
    lsl_funding = if ("lsl_funding" %in% existing_columns) "lsl_funding" else NA_character_,
    ec_funding = if ("ec_funding" %in% existing_columns) "ec_funding" else NA_character_,
    ec_principal_forgiveness = if ("ec_principal_forgiveness" %in% existing_columns) "ec_principal_forgiveness" else NA_character_,
    population = if ("population" %in% existing_columns) "population" else NA_character_,
    project_description = if ("project_description" %in% existing_columns) "project_description" else NA_character_,
    disadvantaged = if ("disadvantaged" %in% existing_columns) "disadvantaged" else NA_character_,
    project_rank = if ("project_rank" %in% existing_columns) "project_rank" else NA_character_,
    project_score = if ("project_score" %in% existing_columns) "project_score" else NA_character_,
    expecting_funding = if ("expecting_funding" %in% existing_columns) "expecting_funding" else NA_character_,
    state = if ("state" %in% existing_columns) "state" else NA_character_,
    state_fiscal_year = if ("state_fiscal_year" %in% existing_columns) "state_fiscal_year" else NA_character_
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
  
  #print("EC Contaminant NA Distribution:")
  #print(table(is.na(oh_clean$ec_contaminant)))
  
  #print("EC Contaminant values for Emerging Contaminants projects:")
  #print(table(oh_clean$ec_contaminant[oh_clean$project_type == "Emerging Contaminants"], useNA = "always"))
  
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