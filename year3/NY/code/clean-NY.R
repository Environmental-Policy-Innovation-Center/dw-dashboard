#source("resources.R")


clean_ny <- function() {
  base_path <- file.path("..", "data")
  
  # Step 1: Read all source files and check their dimensions
  # Multi-year list: Contains all eligible projects submitted for SRF assistance
  ny_base <- fread(file.path(base_path, "32-y3-NewYork_multi-year.csv"),
                   colClass="character", na.strings="") %>%
    clean_names()
  cat("Multi-year list dimensions:", dim(ny_base)[1], "rows,", dim(ny_base)[2], "columns\n")
  print("Multi-year list columns:")
  print(names(ny_base))
  
  # Annual List/BIL PPL: Projects with reports
  ny_annual <- fread(file.path(base_path, "32-y3-NewYork_bil_ppl.csv"),
                     colClass="character", na.strings="") %>%
    clean_names()
  cat("\nAnnual List dimensions:", dim(ny_annual)[1], "rows,", dim(ny_annual)[2], "columns\n")
  print("Annual List columns:")
  print(names(ny_annual))
  
  # Lead PPL: Lead service line projects
  ny_lead <- fread(file.path(base_path, "32-y3-NewYork_bil_lead.csv"),
                   colClasses="character", na.strings=c("", "NA")) %>%
    clean_names()
  cat("\nLead list dimensions:", dim(ny_lead)[1], "rows,", dim(ny_lead)[2], "columns\n")
  print("Lead list columns:")
  print(names(ny_lead))
  
  # EC PPL: Projects addressing emerging contaminants (e.g., PFAS)
  ny_ec <- fread(file.path(base_path, "32-y3-NewYork_bil_ec.csv"),
                 colClass="character", na.strings="") %>%
    clean_names()
  cat("\nEC list dimensions:", dim(ny_ec)[1], "rows,", dim(ny_ec)[2], "columns\n")
  print("EC list columns:")
  print(names(ny_ec))
  
  # Step 2: Analyze relationships between lists
  cat("\nChecking list relationships:")
  
  # Check projects on multiple special lists
  lead_ec_overlap <- intersect(ny_lead$project_number, ny_ec$project_number)
  cat("\nProjects in both Lead and EC lists:", length(lead_ec_overlap))
  if(length(lead_ec_overlap) > 0) {
    cat("\nWARNING: Found projects in both Lead and EC lists:", lead_ec_overlap)
  }
  
  # Check Annual List overlap with special lists
  annual_lead_overlap <- intersect(ny_annual$project_number, ny_lead$project_number)
  cat("\nProjects in both Annual and Lead lists:", length(annual_lead_overlap))
  
  annual_ec_overlap <- intersect(ny_annual$project_number, ny_ec$project_number)
  cat("\nProjects in both Annual and EC lists:", length(annual_ec_overlap))
  
  # Check multi-year list coverage
  multiyear_coverage <- ny_base %>%
    mutate(
      in_annual = project_number %in% ny_annual$project_number,
      in_lead = project_number %in% ny_lead$project_number,
      in_ec = project_number %in% ny_ec$project_number
    ) %>%
    summarise(
      total_projects = n(),
      in_annual_count = sum(in_annual),
      in_lead_count = sum(in_lead),
      in_ec_count = sum(in_ec),
      only_in_multiyear = sum(!in_annual & !in_lead & !in_ec)
    )
  
  cat("\n\nMulti-year List Coverage Analysis:")
  cat("\nTotal projects in multi-year list:", multiyear_coverage$total_projects)
  cat("\nProjects also in Annual List:", multiyear_coverage$in_annual_count)
  cat("\nProjects also in Lead List:", multiyear_coverage$in_lead_count)
  cat("\nProjects also in EC List:", multiyear_coverage$in_ec_count)
  cat("\nProjects only in Multi-year List:", multiyear_coverage$only_in_multiyear)
  
  # Step 3: Check for key fields needed for later processing
  cat("\n\nChecking key fields in each list:")
  
  # Check for score field (needed for disadvantaged status)
  cat("\nAnnual List - Projects with 'H' score:", 
      sum(str_detect(ny_annual$score, "H")), "out of", nrow(ny_annual))
  
  # Check for blue * in Lead list (needed for disadvantaged status)
  cat("\nLead List - Projects with blue * in system name:", 
      sum(str_detect(ny_lead$system_name, "blue \\*")), "out of", nrow(ny_lead))
  
  # Check for DAC in EC list (needed for disadvantaged status)
  cat("\nEC List - Projects with DAC status:", 
      sum(ny_ec$dac == "dac", na.rm = TRUE), "out of", nrow(ny_ec))
  
  # Check for funding line indicators
  cat("\nAnnual List - Projects above funding line:", 
      sum(str_detect(ny_annual$score, "above funding line")), "out of", nrow(ny_annual))
  
  # Step 4: First identify projects by list membership per data dictionary
  # Lead and EC PPL projects take priority over Annual List
  cat("\n\nProcessing projects by list membership:")
  
  # Process Lead projects first
  lead_projects <- ny_lead %>%
    mutate(
      project_type = "Lead",
      disadvantaged = case_when(
        str_detect(system_name, "blue \\*") ~ "Yes",
        TRUE ~ "No Information"
      ),
      expecting_funding = case_when(
        # Check if on Lead Funding List (would need additional file)
        TRUE ~ "Yes"  # Temporarily mark all as Yes
      )
    )
  cat("\nLead projects processed:", nrow(lead_projects))
  
  # Process EC projects second
  ec_projects <- ny_ec %>%
    mutate(
      project_type = "Emerging Contaminants",
      disadvantaged = case_when(
        dac == "dac" ~ "Yes",
        TRUE ~ "No Information"
      ),
      expecting_funding = case_when(
        # Check if on EC Funding List (would need additional file)
        TRUE ~ "Yes"  # Temporarily mark all as Yes
      )
    )
  cat("\nEC projects processed:", nrow(ec_projects))
  
  # Process Annual List projects (excluding those already in Lead/EC)
  annual_projects <- ny_annual %>%
    # Exclude projects already in Lead or EC lists
    filter(!project_number %in% c(lead_projects$project_number, 
                                  ec_projects$project_number)) %>%
    mutate(
      project_type = "General",
      disadvantaged = case_when(
        score == "H" | project_number %in% ny_annual$project_number ~ "Yes",
        TRUE ~ "No Information"
      ),
      expecting_funding = case_when(
        str_detect(score, "above funding line") ~ "Yes",
        # Check if on BIL-GS Funding List (would need additional file)
        TRUE ~ "No"
      )
    )
  cat("\nAnnual List projects processed:", nrow(annual_projects))
  
  # Process remaining Multi-year projects
  processed_numbers <- c(
    lead_projects$project_number,
    ec_projects$project_number,
    annual_projects$project_number
  )
  
  multiyear_projects <- ny_base %>%
    filter(!project_number %in% processed_numbers) %>%
    mutate(
      # For remaining projects, check descriptions
      project_type = case_when(
        str_detect(tolower(description), "lead") ~ "Lead",
        str_detect(tolower(description), "pfas|dioxane|cyanotoxins|mn") ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      disadvantaged = "No Information",  # Per data dictionary
      expecting_funding = "No"  # Not on any funding list
    )
  cat("\nRemaining Multi-year projects:", nrow(multiyear_projects))
  
  # Step 5: Combine all projects and check distributions
  ny_clean <- bind_rows(
    lead_projects %>% select(-dac),
    ec_projects %>% select(-dac, -pfas),
    annual_projects,
    multiyear_projects
  )
  
  cat("\n\nDistribution checks after initial combination:")
  cat("\nProject Types:\n")
  print(table(ny_clean$project_type))
  cat("\nDisadvantaged Status:\n")
  print(table(ny_clean$disadvantaged))
  cat("\nExpecting Funding:\n")
  print(table(ny_clean$expecting_funding))
  
  # Step 6: Clean and standardize columns
  ny_clean <- ny_clean %>%
    mutate(
      population = clean_numeric_string(pop),
      project_cost = clean_numeric_string(project_cost),
      project_id = str_squish(project_number),
      community_served = str_squish(county),
      borrower = paste(str_squish(system_name), "/", str_squish(borrower)),
      project_score = str_squish(score),
      project_description = str_squish(description),
      state = "New York",
      state_fiscal_year = "2025",
      pwsid = as.character(NA),
      project_name = as.character(NA),
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      project_rank = as.character(NA)
    )
  
  # Step 7: Implement Amendment 1
  ny_clean <- ny_clean %>%
    mutate(
      project_cost = ifelse(project_id == "17629", "1800000", project_cost),
      project_score = ifelse(project_id == "18854" & project_score == "40", "80", project_score)
    )
  
  # Step 8: Final column selection and validation
  ny_clean <- ny_clean %>%
    select(community_served, borrower, pwsid, project_id, project_name,
           project_type, project_cost, requested_amount, funding_amount,
           principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding,
           state, state_fiscal_year)
  
  cat("\n\nFinal shape checks:")
  cat("\nShape before removing zero costs:", dim(ny_clean)[1], "rows")
  
  # Remove zero-cost projects
  ny_clean_nonzero <- ny_clean %>%
    filter(project_cost != "0" & project_cost != "0.0" &
             project_cost != "" & !is.na(project_cost))
  
  # Final validation
  num_removed <- nrow(ny_clean) - nrow(ny_clean_nonzero)
  cat(paste0("\nRemoved ", num_removed, " zero/NA cost projects"))
  
  cat("\n\nFinal Distribution Checks:")
  cat("\nProject Types:\n")
  print(table(ny_clean_nonzero$project_type))
  cat("\nDisadvantaged Status:\n")
  print(table(ny_clean_nonzero$disadvantaged))
  cat("\nExpecting Funding:\n")
  print(table(ny_clean_nonzero$expecting_funding))
  
  # Check for any duplicates
  duplicates <- ny_clean_nonzero %>%
    group_by(project_id) %>%
    filter(n() > 1)
  
  if(nrow(duplicates) > 0) {
    cat("\nWARNING: Found", nrow(duplicates), "duplicate project IDs\n")
    print(duplicates %>% select(project_id, project_type))
  }
  
  run_tests(ny_clean_nonzero)
  return(ny_clean_nonzero)
}
  
  
  