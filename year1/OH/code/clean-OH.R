clean_oh_y1 <- function() {
  
  # NOTE: all of these tables may overlap with one another! 
  
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
           expecting_funding = "Yes", 
           source_list = "fundable")
  
  ## DAC PF
  oh_dac_ppl <- fread("year1/OH/data/oh-ppl-pf.csv",
                      colClasses = "character", na.strings = "") |>
    clean_names() %>%
    mutate(source_list = "dac_ppl", 
           disadvantaged = "Yes") |>
    select(-readiness_to_proceed)
  
  ## Regional PF
  oh_reg_ppl <- fread("year1/OH/data/oh-ppl-regional-pf.csv",
                      colClasses = "character", na.strings = "") |>
    clean_names() |>
    mutate(source_list = "regionalization_ppl")|>
    select(-readiness_to_proceed)
  
  # merging the dac and regionalization list together: 
  oh_dac_reg <- merge(oh_dac_ppl, oh_reg_ppl, 
                      by = c("entity", "project", 
                             "county", "estimated_loan_amount", 
                             "loan_type", 
                             "estimated_award_date", 
                             "rate", "project_score"), all = T)|>
    mutate(principal_forgiveness = case_when(!is.na(estimated_principal_forgiveness.x) ~ estimated_principal_forgiveness.x, 
                                             is.na(estimated_principal_forgiveness.x) & !is.na(estimated_principal_forgiveness.y) ~ estimated_principal_forgiveness.y,
                                             TRUE ~ "No Information")) |>
    # handling bypass in og DAC list: 
    mutate(principal_forgiveness = case_when(estimated_principal_forgiveness.y == "BYPASS" & is.na(estimated_principal_forgiveness.x) ~ "No Information", 
                                             TRUE ~ principal_forgiveness))|>
    select(-c(estimated_principal_forgiveness.y, estimated_principal_forgiveness.x)) |>
    mutate(project_score = str_squish(project_score), 
           project_type = case_when(grepl("HAB|PFAS", rate) ~ "Emerging Contaminants", 
                                    # this will be overwritten for lead projects
                                    # when we merge these three tables
                                    TRUE ~ "General")) %>%
    # standardizing project names that also appear on funding ppl, based on 
    # close names and the same loan amounts and rates - 
    # NOTE there is one project = "Warsaw Waterline Replacement and Extension" 
    # that has like 90% of the information in the fundable list but a mismatched 
    # rate... similar situation with La Rue's "Water Treatment Plant Improvements1"
    mutate(project = case_when(project == "Warsaw Waterline Replacement Extension" ~ "Warsaw Waterline Replacement and Extension", 
                               project == "U.S. 40/S.R. 37 Water Line Extension" ~ "U.S. 40/S.R. 37 Waterline Extension", 
                               project == "Eagle Wings Water Line Extension2" ~ "Eagle Wings Water Line Extension", 
                               project == "Phase 1 Waterline Replacement2" ~ "Phase 1 Waterline Replacement",
                               project == "Waterline Replacement Project2" ~ "Waterline Replacement Project",
                               project == "Waterline Replacement Phase 23" ~ "Waterline Replacement Phase 2",
                               TRUE ~ project), 
           entity = case_when(entity == "Northwestern Water & Sewer District" ~ "Northwestern Water & Sewer Dist", 
                              entity == "Adams County Regional Water District" ~ "Adams County Regional Water Dist",
                              TRUE ~ entity))
  
  # merge dac and reg into the base table 
  oh_base <- merge(oh_fundable, oh_dac_reg, by = c("entity", "project", "county", 
                                                   # "estimated_loan_amount", 
                                                   "estimated_award_date",
                                                   "loan_type", "rate"), all = T) |>
    # resolve project types for lists on dac or reg ppl but not fundable
    # list
    mutate(project_type  = case_when(is.na(project_type.y) ~ project_type.x, 
                                     is.na(project_type.x) ~ project_type.y, 
                                     !is.na(project_type.y) & !is.na(project_type.x) ~ project_type.y)) |>
    # add in the lead projects 
    mutate(project_type = case_when(grepl("LSL", rate) ~ "Lead", 
                                    TRUE ~ project_type)) %>%
    # # add in requested amount (same as funding amount on the fundable list)
    mutate(requested_amount = case_when(!is.na(funding_amount) ~ funding_amount,
                                        TRUE ~ clean_numeric_string(estimated_loan_amount.y))) |>
    # remove extra columns from merge
    select(-c(project_type.x, project_type.y, estimated_loan_amount.x, estimated_loan_amount.y)) |>
    # this project does not have LSL in "rate" but is clearly a lsl project:
    mutate(project_type = case_when(project == "Lead Service Line Replacement" & project_type == "General" ~ "Lead", 
                                    project ==  "Lead Service Line and Water Line Replacement" & entity == "Dunkirk" ~ "Lead", 
                                    project == "Water Line and Household Lead Line Replacement" & entity == "Glenmont" ~ "Lead",
                                    TRUE ~ project_type))
  
  # adding lead ppl: (59, 8)
  oh_lead_ppl <- fread("year1/OH/data/oh-lslr.csv",
                       colClasses = "character", na.strings = "") |>
    clean_names() |> 
    mutate(project_type = "Lead", 
           source_list = "lead_ppl")
  
  # adding EC ppl:(9, 7)
  oh_pfas_ppl <- fread("year1/OH/data/oh-ppl-hab-pfas.csv",
                       colClasses = "character", na.strings = "") |>
    clean_names() |>
    mutate(project_type = "Emerging Contaminants", 
           source_list = "ec_ppl")
  
  # binding the lead and ec together: 
  oh_lead_ec <- bind_rows(oh_lead_ppl, oh_pfas_ppl) %>%
    mutate(requested_amount = clean_numeric_string(estimated_loan_amount)) %>%
    select(-c(estimated_loan_amount, estimated_lsl_eligible_costs, source_list)) %>%
    mutate(project = ifelse(project ==  "Waterline and Household Lead Line Replacement", "Water line and Household Lead Line Replacement", project),
           entity = ifelse(entity == "Northwestern Water & Sewer District", "Northwestern Water & Sewer Dist", entity)) %>%
    mutate(project = gsub("\\*", "", project)) 
   
  # need to merge with oh_base to include systems that were not funded and/or 
  # DAC or regionalization projects, while capturing projects that are on these 
  # lists already
  # combining everything together: 
  oh_clean <- merge(oh_base, oh_lead_ec, 
                           by = c("entity", "project", "county", 
                                  "requested_amount", "loan_type", 
                                  "estimated_award_date", "rate", 
                                  "project_type"), 
                           all = T) |>
    mutate(borrower = str_squish(entity),
           pwsid = case_when(is.na(pwsid) ~ "No Information", 
                             TRUE ~ pwsid),
           project_id = as.character(NA),
           project_name = as.character(NA),
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           funding_amount = clean_numeric_string(funding_amount),
           project_description = str_squish(project),
           community_served = str_squish(county),  
           project_cost = as.character(NA),
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

