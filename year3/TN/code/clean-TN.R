clean_tn_y3 <- function() {
  
  # (64, 21) - gen PPL
  gen_ppl <- fread("year3/TN/data/basegen_supp_ppl.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # manually remove projects found on lead/ec list already
    filter(is.na(on_lead_ec)) %>%
    mutate(project_type = case_when(grepl("lead", project_description, ignore.case = TRUE) ~ "Lead", 
                                    grepl("PFAS|PFOS|Emerging.Contaminants", 
                                          project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
                                    TRUE ~ "General"),
           # these rows are blank for projects below the ranking line & not 
           # expecting funding
           project_rank = replace_na(rank_order, "No Information"), 
           project_score = replace_na(priority_points, "No Information"),
           expecting_funding = case_when(project_rank == "No Information" ~ "No", 
                                         TRUE ~ "Yes"))

  # (9, 14) - emerging contaminants 
  ec_ppl <- fread("year3/TN/data/ec_ppl.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants")
  
  # (7, 14) lead projects: 
  lead_ppl <- fread("year3/TN/data/lead_ppl.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead")
  
  # (16, 18) - binding and fixing the rank order for lead and ec projects: 
  lead_ec <- bind_rows(ec_ppl, lead_ppl) %>%
    # adding extra columns that are the same for both ppls: 
    mutate(project_rank = as.character(rank_order), 
           expecting_funding = "Yes", 
           project_score = priority_points)
  
  # (80, 18) cleaning - binding gen, lead & EC together 
  tn_clean <- bind_rows(gen_ppl, lead_ec) %>%
    mutate(community_served = str_squish(county),
           # removing weird characters from local government column
           borrower = str_replace_all(local_government, "\\*", ""),
           borrower = str_replace_all(borrower, "\\+", ""),
           borrower = str_squish(borrower),
           pwsid = str_squish(pwsid_number), 
           project_id = as.character(NA),
           project_name = as.character(NA), 
           project_cost = clean_numeric_string(total_project_amount), 
           requested_amount = as.character(NA), 
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_description = str_squish(project_description), 
           population = clean_numeric_string(pop_served), 
           disadvantaged = case_when(grepl("\\*", local_government) ~ "Yes",
                                     TRUE ~ "No"), 
           state = "Tennessee",
           state_fiscal_year = "2025")%>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(tn_clean)
  rm(list=setdiff(ls(), "tn_clean"))
  
  return(tn_clean)
}
