clean_oh_y1 <- function() {
  
  # ohio EC string from data dictionary 
  oh_ec_str <- "cyanotoxin|dioxane|emerging contaminant|lithium|manganese|Mn|Perfluoro-n-pentanoic acid|PFPeA|PFAS|PFOA|PFOS|trihalomethanes|THM|Unregulated Contaminant Monitoring Rule|DBP|HAA5|haloacetic acid"
  
  ## Base fundable list 
  oh_fundable <- fread("year1/OH/data/oh-ppl-base.csv",
                   colClasses = "character", na.strings = "") |>
    clean_names() |>
    select(-v1) |>
    # reset all column names and drop column name row
    rename(entity = v2, project = v3, pwsid = v4, population = v5, county = v6, 
           estimated_loan_amount = v7, loan_type = v8, estimated_award_date = v9, rate = v10) |>
    filter(entity != "Entity") |>
    mutate(funding_amount = clean_numeric_string(estimated_loan_amount),
           project = str_squish(project),
           project_type = case_when(grepl(oh_ec_str, project) | grepl("HAB|PFAS", rate) ~ "Emerging Contaminants", 
                                    # this will be overwritten for lead projects
                                    # when we merge these three tables
                                    TRUE ~ "General"),
           # the "Mn" EC string incorrectly captured this as a EC project
           project_type = case_when(project == "Watermain Imps Bun. 1 - Grange Hall Booster Station Wtr Mns" ~ "General",
                                    TRUE ~ project_type),
           expecting_funding = "Yes")
  
  ## DAC PF
  oh_dac_ppl <- fread("year1/OH/data/oh-ppl-pf.csv",
                      colClasses = "character", na.strings = "") |>
    clean_names()
  
  ## Regional PF
  # TODO - figure out if these are also disadvantaged 
  oh_reg_ppl <- fread("year1/OH/data/oh-ppl-regional-pf.csv",
                      colClasses = "character", na.strings = "") |>
    clean_names() 
  
  # binding principal forgiveness tables: 
  oh_pf <- bind_rows(oh_dac_ppl, oh_reg_ppl) |>
    filter(estimated_principal_forgiveness != "BYPASS") |>
    mutate(project_score = str_squish(project_score), 
           # TODO - check with funding tracker team the EC tag should apply to
           # both the dac and regionalization ppls: 
           project_type = case_when(grepl("HAB|PFAS", rate) ~ "Emerging Contaminants", 
                                    # this will be overwritten for lead projects
                                    # when we merge these three tables
                                    TRUE ~ "General")) |>
    # TODO - also check that these should both be classified as disadvantaged
    mutate(disadvantaged = "Yes")  
  
  # merge PF into the base table
  oh_base <- oh_fundable |>
    left_join(oh_pf) |>
    mutate(principal_forgiveness = case_when(
      # fill in gaps where multiple projects or funding_amount discrepancies cause mismatches
      entity == "Walnut Creek Water Company" & estimated_loan_amount == "$3,950,000" ~ "$2,070,000",
      entity == "Rittman" ~ "$2,173,483",
      entity == "La Rue" ~ "$54,000",
      entity == "Piketon" & loan_type == "Construction" ~ "$3,994,717",
      entity == "Nelsonville" & loan_type == "Construction" ~ "$2,759,300",
      # replace PF where it attached to two projects
      project == "Village of Mantua Water Treatment Plant Liquid Chlorine" ~ as.character(NA),
      TRUE ~ estimated_principal_forgiveness), 
      project_type = case_when(grepl("LSL", rate) ~ "Lead", 
                               TRUE ~ project_type)) 
  
  
  # adding lead ppl: (59, 8)
  oh_lead_ppl <- fread("year1/OH/data/oh-lslr.csv",
                       colClasses = "character", na.strings = "") |>
    clean_names() |> 
    mutate(project_type = "Lead")
  
  # adding EC ppl:(9, 7)
  oh_pfas_ppl <- fread("year1/OH/data/oh-ppl-hab-pfas.csv",
                       colClasses = "character", na.strings = "") |>
    clean_names() |>
    mutate(project_type = "Emerging Contaminants")
  
  # combining everything together: 
  oh_clean <- bind_rows(oh_base, oh_lead_ppl, oh_pfas_ppl) |>
    mutate(borrower = str_squish(entity),
           pwsid = case_when(is.na(pwsid) ~ "No Information", 
                             TRUE ~ pwsid),
           project_id = as.character(NA),
           project_name = as.character(NA),
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           funding_amount = clean_numeric_string(estimated_loan_amount),
           project_description = str_squish(project),
           community_served = str_squish(county),  # TODO <- check this 
           project_cost = as.character(NA),
           requested_amount = clean_numeric_string(estimated_loan_amount), 
           project_description = str_squish(project),
           population = clean_numeric_string(population),
           disadvantaged = case_when(is.na(disadvantaged) ~ "No", 
                                     TRUE ~ disadvantaged), 
           project_rank = as.character(NA), 
           project_score = case_when(is.na(project_score) ~ "No Information", 
                                     TRUE ~ project_score),
           expecting_funding = case_when(is.na(expecting_funding) ~ "No", 
                                     TRUE ~ expecting_funding), 
           state = "Ohio", 
           state_fiscal_year = "2023") |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}